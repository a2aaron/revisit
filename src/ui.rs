use std::ffi::c_void;
use std::sync::Arc;

use iced::{futures, Align, Column, Command, Element, Row, Subscription};
use iced_audio::{knob, v_slider, Knob, NormalParam, VSlider};
use iced_baseview::{Application, Handle, WindowSubs};
use raw_window_handle::RawWindowHandle;
use tokio::sync::Notify;
use vst::{editor::Editor, host::Host};

use crate::params::{
    ModulationType, OSCParameterType, OSCType, ParameterType, RawOSC, RawParameters,
};

/// Create a widget (actually a column) that:
/// 1. Uses `$state` as the widget's `$widget::State` struct
/// 2. Sends the message ParameterChanged(normal, $parameter)
/// 3. Has the title `widget_name($parameter)`
macro_rules! widget {
    ($widget:ident, $state:expr, $parameter:expr) => {
        with_label(
            $widget::new($state, move |normal| {
                Message::ParameterChanged(normal.as_f32(), $parameter)
            }),
            &widget_name($parameter),
        );
    };
}

/// Create a combined element with the label `title` under a `widget`
fn with_label<'a>(widget: impl Into<Element<'a, Message>>, title: &str) -> Element<'a, Message> {
    Column::with_children(vec![widget.into(), iced::Text::new(title).size(15).into()])
        .align_items(Align::Center)
        .into()
}

/// Convience function to make a `NormalParam` using a `RawParameters`. The
/// `NormalParam` will have a default value of whatever the default value is for
/// a `RawParameters`.
fn make_normal_param(param_ref: &RawParameters, param_type: ParameterType) -> NormalParam {
    NormalParam {
        value: param_ref.get(param_type).into(),
        default: RawParameters::get_default(param_type).into(),
    }
}

/// Convience function to make `knob::State` out of a `RawParameters`
/// and `ParameterType`
fn make_knob(param_ref: &RawParameters, param_type: ParameterType) -> knob::State {
    knob::State::new(make_normal_param(param_ref, param_type))
}

/// Create a row of `knobs` with the title `title`.
fn knob_row<'a>(knobs: Vec<Element<'a, Message>>, title: &str) -> Element<'a, Message> {
    Column::new()
        .push(iced::Text::new(title).size(18))
        .push(
            Row::with_children(knobs)
                .align_items(Align::Center)
                .spacing(20),
        )
        .align_items(Align::Start)
        .spacing(2)
        .into()
}

/// Make a row of widgets with better default spacing.
fn row(widgets: Vec<Element<'_, Message>>) -> Element<'_, Message> {
    Row::with_children(widgets).spacing(5).into()
}

/// Make an empty column with better default spacing.
fn column<'a>() -> Column<'a, Message> {
    Column::new().spacing(5)
}

/// Make a pane without the additional `on_off` widget.
fn make_pane<'a>(
    title: &str,
    widgets: Vec<(Vec<Element<'a, Message>>, &str)>,
) -> Column<'a, Message> {
    make_pane_with_checkbox(title, None, widgets)
}

/// Makes a pane of widgets with the specified title.
/// `widgets` consists of a vector of (`inner_vec`, `row_title`). Each `inner_vec`
///  is a list of widgets that will be arranged in a row. The row will have the
/// heading of `row_title`.
/// If `on_off` is `Some(element)`. then the title for this pane will have the
/// `element` to the right of it.
fn make_pane_with_checkbox<'a>(
    title: &str,
    on_off: Option<Element<'a, Message>>,
    widgets: Vec<(Vec<Element<'a, Message>>, &str)>,
) -> Column<'a, Message> {
    // If there is no `on_off` element, just use a empty widget.
    // Picking `Length::Shrink` for a `Space` widget makes it have zero size.
    let on_off = on_off.unwrap_or_else(|| {
        iced::widget::Space::new(iced::Length::Shrink, iced::Length::Shrink).into()
    });
    let mut pane = column().push(row(vec![iced::Text::new(title).into(), on_off]));
    for (knobs, title) in widgets.into_iter() {
        pane = pane.push(knob_row(knobs, title));
    }
    pane
}

/// A replacement struct for `iced_audio::IntRange`. This is used for snapping
/// knobs which only take on discrete integer values (for example, the Shape knob).
/// However, unlike `iced_audio::IntRange`, this knob sets the integer regions
/// via truncation, rather than rounding. This makes each int region have the
/// same size, which is needed because this matches with how `RawParameters`'s
/// `AtomicFloat` values are convereted to their discrete values.
/// For example, if we have a parameter that takes on 4 values, then we convert
/// from the `AtomicFloat` to the integer as follows:
/// 0.00 to 0.25 = 0
/// 0.25 to 0.50 = 1
/// 0.50 to 0.75 = 2
/// 0.75 to 1.00 = 3
/// However, iced_audio's `IntRange` wouldn't do this for the `Normal` that is
/// held in a `knob::State`, because it _rounds_ the float instead of truncating
/// it, which produces the wrong thing! It would end up having:
/// 0.00 to 0.16 = 0
/// 0.16 to 0.50 = 1
/// 0.50 to 0.83 = 2
/// 0.83 to 1.00 = 3
/// Which isn't evenly spaced. Thus, `TruncatingIntRange` instead snaps floats
/// using truncation instead.
/// See also: https://www.desmos.com/calculator/esnnnbfzml for a visualization
struct TruncatingIntRange {
    num_regions: usize,
}

impl TruncatingIntRange {
    /// Snap a normalized float to the nearest normalized float value corresponding
    /// to an integer.
    fn snap(&self, value: f32) -> f32 {
        snap_float(value, self.num_regions)
    }

    /// Snap a knob to the nearest integer value.
    fn snap_knob(&self, knob: &mut knob::State) {
        knob.normal_param.value = self.snap(knob.normal().into()).into();
    }
}

/// Snap a float value in range 0.0-1.0 to the nearest f32 region
/// For example, snap_float(_, 4) will snap a float to either:
/// 0.0, 0.333, 0.666, or 1.0
fn snap_float(value: f32, num_regions: usize) -> f32 {
    // We subtract one from this denominator because we want there to only be
    // four jumps. See also https://www.desmos.com/calculator/esnnnbfzml
    let num_regions = num_regions as f32;
    (num_regions * value).floor() / (num_regions - 1.0)
}

/// A struct implementing `iced_native::Recipe`. This will send a
/// `Message::ForceRedraw` whenever the `notifier` is recieves a notification.
/// This struct is used to make the iced GUI update whenever the VST host alters
/// a parameter (say, via an automation curve).
struct NotifyRecipe {
    notifier: Arc<Notify>,
}

impl<H, I> iced_native::subscription::Recipe<H, I> for NotifyRecipe
where
    H: std::hash::Hasher,
{
    type Output = Message;

    fn hash(&self, state: &mut H) {
        // generic hash implementation. this isn't actually very important to us
        use std::hash::Hash;
        std::any::TypeId::of::<Self>().hash(state);
    }

    // This stream function was a nightmare to figure out initally, but here's
    // how it works (thank you to Cassie for explaining this to me!)
    // The function returns a `futures::BoxStream`, which is a async thing.
    // The BoxStream has a function that returns an `Option((message, next_state))`
    // If it returns None, then the stream ends (we don't want this)
    // If it returns `Some((message, next_state))` then we fire off the `message`
    // to iced, and the `next_state` becomes the argument for the input of the
    // function the next time it is run (which is... immediately, i think?)
    // Hence we essentially just run in a look and yield every so often.
    fn stream(
        self: Box<Self>,
        _: futures::stream::BoxStream<'static, I>,
    ) -> futures::stream::BoxStream<'static, Self::Output> {
        Box::pin(futures::stream::unfold(
            self.notifier.clone(),
            |notifier| async move {
                // Wait for the notifier to recieve a notification before firing
                // the message.
                notifier.notified().await;
                Some((Message::ForceRedraw, notifier))
            },
        ))
    }
}

/// A GUI message.
#[derive(Debug, Clone, Copy)]
pub enum Message {
    /// These indicate that the GUI has changed a parameter
    ParameterChanged(f32, ParameterType),
    /// These are called to make the GUI update itself. Usually, this means that
    /// the host has changed a parameter, so the GUI should change to match.
    /// Note that this works because sending a message also causes `iced` to
    /// call `view` and do a screen update. If it didn't do that, then this won't
    /// work.
    ForceRedraw,
}

/// A struct keeping track of the various knob states an oscillator has.
/// The ranges are used for snapping the knobs.
pub struct OSCKnobs {
    volume: knob::State,
    coarse_tune: knob::State,
    fine_tune: knob::State,
    attack: knob::State,
    hold: knob::State,
    decay: knob::State,
    sustain: knob::State,
    release: knob::State,
    vol_lfo_amplitude: knob::State,
    vol_lfo_period: knob::State,
    note_shape: knob::State,
    note_warp: knob::State,
    pitch_attack: knob::State,
    pitch_hold: knob::State,
    pitch_decay: knob::State,
    pitch_multiply: knob::State,
    pitch_release: knob::State,
    pitch_lfo_amplitude: knob::State,
    pitch_lfo_period: knob::State,
    filter_type: knob::State,
    filter_freq: knob::State,
    filter_q: knob::State,
    filter_gain: knob::State,
    coarse_tune_range: TruncatingIntRange,
    note_shape_range: TruncatingIntRange,
    filter_type_range: TruncatingIntRange,
}

impl OSCKnobs {
    fn new(param_ref: &RawParameters, osc: OSCType) -> OSCKnobs {
        use OSCParameterType::*;
        OSCKnobs {
            volume: make_knob(param_ref, (Volume, osc).into()),
            coarse_tune: make_knob(param_ref, (CoarseTune, osc).into()),
            fine_tune: make_knob(param_ref, (FineTune, osc).into()),
            attack: make_knob(param_ref, (VolAttack, osc).into()),
            hold: make_knob(param_ref, (VolHold, osc).into()),
            decay: make_knob(param_ref, (VolDecay, osc).into()),
            sustain: make_knob(param_ref, (VolSustain, osc).into()),
            release: make_knob(param_ref, (VolRelease, osc).into()),
            vol_lfo_amplitude: make_knob(param_ref, (VolLFOAmplitude, osc).into()),
            vol_lfo_period: make_knob(param_ref, (VolLFOPeriod, osc).into()),
            note_shape: make_knob(param_ref, (Shape, osc).into()),
            note_warp: make_knob(param_ref, (Warp, osc).into()),
            pitch_attack: make_knob(param_ref, (PitchAttack, osc).into()),
            pitch_hold: make_knob(param_ref, (PitchHold, osc).into()),
            pitch_decay: make_knob(param_ref, (PitchDecay, osc).into()),
            pitch_multiply: make_knob(param_ref, (PitchMultiply, osc).into()),
            pitch_release: make_knob(param_ref, (PitchRelease, osc).into()),
            pitch_lfo_amplitude: make_knob(param_ref, (PitchLFOAmplitude, osc).into()),
            pitch_lfo_period: make_knob(param_ref, (PitchLFOPeriod, osc).into()),
            filter_type: make_knob(param_ref, (FilterType, osc).into()),
            filter_freq: make_knob(param_ref, (FilterFreq, osc).into()),
            filter_q: make_knob(param_ref, (FilterQ, osc).into()),
            filter_gain: make_knob(param_ref, (FilterGain, osc).into()),
            coarse_tune_range: TruncatingIntRange {
                num_regions: 24 * 2 + 1,
            },
            note_shape_range: TruncatingIntRange {
                num_regions: crate::sound_gen::NoteShape::VARIANT_COUNT,
            },
            filter_type_range: TruncatingIntRange {
                num_regions: crate::params::FILTER_TYPE_VARIANT_COUNT,
            },
        }
    }

    /// Set the knob states using the `osc` reference provided.
    /// This method is called whenever a ForceRedraw happens.
    fn update(&mut self, osc: &RawOSC) {
        /// Sets a knob while also the TruncatingIntRange to snap the value to
        /// the right spot.
        fn set_knob_with_range(knob: &mut knob::State, range: &TruncatingIntRange, value: f32) {
            knob.set_normal(range.snap(value).into());
        }

        use OSCParameterType::*;
        self.volume.set_normal(osc.get(Volume).into());

        set_knob_with_range(
            &mut self.coarse_tune,
            &self.coarse_tune_range,
            osc.get(CoarseTune),
        );

        self.fine_tune.set_normal(osc.get(FineTune).into());
        self.attack.set_normal(osc.get(VolAttack).into());
        self.hold.set_normal(osc.get(VolHold).into());
        self.decay.set_normal(osc.get(VolDecay).into());
        self.sustain.set_normal(osc.get(VolSustain).into());
        self.release.set_normal(osc.get(VolRelease).into());
        self.vol_lfo_amplitude
            .set_normal(osc.get(VolLFOAmplitude).into());
        self.vol_lfo_period.set_normal(osc.get(VolLFOPeriod).into());

        set_knob_with_range(&mut self.note_shape, &self.note_shape_range, osc.get(Shape));

        self.note_warp.set_normal(osc.get(Warp).into());
        self.pitch_attack.set_normal(osc.get(PitchAttack).into());
        self.pitch_hold.set_normal(osc.get(PitchHold).into());
        self.pitch_decay.set_normal(osc.get(PitchDecay).into());
        self.pitch_multiply
            .set_normal(osc.get(PitchMultiply).into());
        self.pitch_release.set_normal(osc.get(PitchRelease).into());
        self.pitch_lfo_amplitude
            .set_normal(osc.get(PitchLFOAmplitude).into());
        self.pitch_lfo_period
            .set_normal(osc.get(PitchLFOPeriod).into());

        set_knob_with_range(
            &mut self.filter_type,
            &self.filter_type_range,
            osc.get(FilterType),
        );

        self.filter_freq.set_normal(osc.get(FilterFreq).into());
        self.filter_q.set_normal(osc.get(FilterQ).into());
        self.filter_gain.set_normal(osc.get(FilterGain).into());
    }

    /// Create the set of widget for a particular oscillator.
    /// `osc` is used to set what type of ParamterType is fired for the
    /// `ParameterChanged(f32, ParameterType)` messages. Ex: is `osc` is
    /// OSCType::OSC1, then the volume knob will fire
    /// `ParameterChanged(value, ParameterType::OSC1(OSCParameterType::Volume))`
    /// When `on_off` is `Some(widget)`, the widget will be inserted to the right
    /// of the title of this widget block. This is usually the OSC 2 mod control.
    fn make_widget<'a>(
        &'a mut self,
        osc: OSCType,
        on_off: Option<Element<'a, Message>>,
    ) -> Element<'a, Message> {
        use OSCParameterType::*;
        let volume = widget!(Knob, &mut self.volume, (Volume, osc).into());
        let fine_tune = widget!(Knob, &mut self.fine_tune, (FineTune, osc).into());
        let coarse_tune = widget!(Knob, &mut self.coarse_tune, (CoarseTune, osc).into());
        let shape = widget!(Knob, &mut self.note_shape, (Shape, osc).into());
        let warp = widget!(Knob, &mut self.note_warp, (Warp, osc).into());

        let attack = widget!(Knob, &mut self.attack, (VolAttack, osc).into());
        let hold = widget!(Knob, &mut self.hold, (VolHold, osc).into());
        let decay = widget!(Knob, &mut self.decay, (VolDecay, osc).into());
        let sustain = widget!(Knob, &mut self.sustain, (VolSustain, osc).into());
        let release = widget!(Knob, &mut self.release, (VolRelease, osc).into());

        let vol_lfo_amplitude = widget!(
            Knob,
            &mut self.vol_lfo_amplitude,
            (VolLFOAmplitude, osc).into()
        );
        let vol_lfo_period = widget!(Knob, &mut self.vol_lfo_period, (VolLFOPeriod, osc).into());

        let pitch_attack = widget!(Knob, &mut self.pitch_attack, (PitchAttack, osc).into());
        let pitch_hold = widget!(Knob, &mut self.pitch_hold, (PitchHold, osc).into());
        let pitch_decay = widget!(Knob, &mut self.pitch_decay, (PitchDecay, osc).into());
        let pitch_multiply = widget!(Knob, &mut self.pitch_multiply, (PitchMultiply, osc).into());
        let pitch_release = widget!(Knob, &mut self.pitch_release, (PitchRelease, osc).into());

        let pitch_lfo_amplitude = widget!(
            Knob,
            &mut self.pitch_lfo_amplitude,
            (PitchLFOAmplitude, osc).into()
        );
        let pitch_lfo_period = widget!(
            Knob,
            &mut self.pitch_lfo_period,
            (PitchLFOPeriod, osc).into()
        );

        let filter_type = widget!(Knob, &mut self.filter_type, (FilterType, osc).into());
        let filter_freq = widget!(Knob, &mut self.filter_freq, (FilterFreq, osc).into());
        let filter_q = widget!(Knob, &mut self.filter_q, (FilterQ, osc).into());
        let filter_gain = widget!(Knob, &mut self.filter_gain, (FilterGain, osc).into());

        let title = match osc {
            OSCType::OSC1 => "OSC 1",
            OSCType::OSC2 => "OSC 2",
        };

        let osc_pane = make_pane_with_checkbox(
            title,
            on_off,
            vec![
                (vec![volume, fine_tune, coarse_tune, shape, warp], "Sound"),
                (vec![attack, hold, decay, sustain, release], "Envelope"),
                (vec![vol_lfo_amplitude, vol_lfo_period], "LFO"),
            ],
        );

        let pitch_pane = make_pane(
            "PITCH",
            vec![
                (
                    vec![
                        pitch_attack,
                        pitch_hold,
                        pitch_decay,
                        pitch_multiply,
                        pitch_release,
                    ],
                    "Envelope",
                ),
                (vec![pitch_lfo_amplitude, pitch_lfo_period], "LFO"),
            ],
        );

        let filter_pane = make_pane(
            "FILTER",
            vec![(vec![filter_type, filter_freq, filter_q, filter_gain], "")],
        );

        Column::with_children(vec![osc_pane.into(), pitch_pane.into(), filter_pane.into()])
            .padding(20)
            .spacing(20)
            .into()
    }
}

/// The struct which manages the GUI. This struct handles both the messages
/// passed to it by iced as well as communciating with host VST for GUI stuff.
pub struct UIFrontEnd {
    // Knobs and sliders
    master_vol: v_slider::State,
    osc_1: OSCKnobs,
    osc_2: OSCKnobs,
    osc_2_mod: knob::State,
    osc_2_mod_range: TruncatingIntRange,
    // This is used so that the GUI can update the shared parameters object when
    // a GUI element is changed.
    params: std::sync::Arc<RawParameters>,
    // An `iced_baseview` handle. This is None if the GUI hasn't been opened
    // and `Some(Handle)` otherwise. This is used for some small things, such as
    // getting iced to run closing code when closing the window, as well as
    // being able to pass in keyboard events, because some VST hosts capture the
    // keyboard event.
    handle: Option<Handle>,
    // This notifier gains a message whenever the VST host alters a parameter,
    // and is used to force redraws to keep the GUI in sync with the parameters.
    notifier: Arc<Notify>,
    // This just tracks if the control key is held down. This is needed because
    // the VST host captures keyboard events, so we we need to manually track
    // keydown/keyup events.
    control_key: keyboard_types::KeyState,
}

impl Application for UIFrontEnd {
    type Message = Message;
    type Executor = iced_baseview::executor::Default;
    type Flags = (Arc<RawParameters>, Arc<Notify>);

    fn new(flags: Self::Flags) -> (Self, Command<Self::Message>) {
        let (params, notifier) = flags;
        let param_ref = params.as_ref();
        let ui = UIFrontEnd {
            master_vol: v_slider::State::new(make_normal_param(
                param_ref,
                ParameterType::MasterVolume,
            )),
            osc_1: OSCKnobs::new(param_ref, OSCType::OSC1),
            osc_2: OSCKnobs::new(param_ref, OSCType::OSC1),
            osc_2_mod: make_knob(param_ref, ParameterType::OSC2Mod),
            osc_2_mod_range: TruncatingIntRange {
                num_regions: ModulationType::VARIANT_COUNT,
            },
            params,
            handle: None,
            notifier,
            control_key: keyboard_types::KeyState::Up,
        };
        (ui, Command::none())
    }

    /// React to an incoming message
    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        match message {
            // The GUI has changed a parameter
            Message::ParameterChanged(value, param) => {
                // We set the parameter according to the changed value.
                self.params.set(value, param);

                // If the knob changed was a "snapping" knob, make sure the knob
                // ends up appearing snapped.
                match param {
                    ParameterType::OSC1(osc_param) | ParameterType::OSC2(osc_param) => {
                        let osc = match param {
                            ParameterType::OSC1(_) => &mut self.osc_1,
                            ParameterType::OSC2(_) => &mut self.osc_2,
                            _ => unreachable!(),
                        };
                        match osc_param {
                            OSCParameterType::CoarseTune => {
                                osc.coarse_tune_range.snap_knob(&mut osc.coarse_tune)
                            }
                            OSCParameterType::Shape => {
                                osc.note_shape_range.snap_knob(&mut osc.note_shape)
                            }
                            OSCParameterType::FilterType => {
                                osc.filter_type_range.snap_knob(&mut osc.filter_type)
                            }
                            _ => (),
                        }
                    }
                    ParameterType::OSC2Mod => {
                        self.osc_2_mod_range.snap_knob(&mut self.osc_2_mod);
                    }
                    ParameterType::MasterVolume => (),
                }

                // Make the VST host update its own parameter display. This is needed
                // so the host actually has updates with GUI.
                self.params.host.update_display();
            }
            // The host has changed a parameter, or a redraw was requested
            // We update the knobs based on the current parameter values
            Message::ForceRedraw => {
                // TODO : Don't use a RawParameters for this? Instead consider
                // using a normal Parameter struct and letting the knobs have
                // actual values instead of weird 0.0-1.0 normalized values.
                // This is fine as it is right now though.
                self.master_vol
                    .set_normal(self.params.get(ParameterType::MasterVolume).into());
                self.osc_1.update(&self.params.osc_1);
                self.osc_2.update(&self.params.osc_2);
                self.osc_2_mod
                    .set_normal(self.params.get(ParameterType::OSC2Mod).into());
            }
        }

        Command::none()
    }

    /// Set up the GUI elements
    /// Note that this isn't called every frame--it's only called when there's
    /// an update to the view (which happens to be only when messages happen)
    fn view(&mut self) -> iced::Element<'_, Self::Message> {
        let screen = self.size();
        let (screen_width, screen_height) = (screen.0 as u32, screen.1 as u32);

        let master_vol_widget = widget!(VSlider, &mut self.master_vol, ParameterType::MasterVolume);

        // TODO: Consider a smarter way for messages that doesn't involve always casting to f32
        let osc_2_mod = widget!(Knob, &mut self.osc_2_mod, ParameterType::OSC2Mod);

        let master_pane = master_vol_widget;
        let osc_1 = OSCKnobs::make_widget(&mut self.osc_1, OSCType::OSC1, None);
        let osc_2 = OSCKnobs::make_widget(&mut self.osc_2, OSCType::OSC2, Some(osc_2_mod));

        Row::new()
            .push(osc_1)
            .push(osc_2)
            .push(master_pane)
            .max_width(screen_width)
            .max_height(screen_height)
            .spacing(10)
            .into()
    }

    /// This allows iced to recieve messages from the notifier
    fn subscription(
        &self,
        _window_subs: &mut WindowSubs<Self::Message>,
    ) -> Subscription<Self::Message> {
        let recipe = NotifyRecipe {
            notifier: self.notifier.clone(),
        };
        Subscription::from_recipe(recipe)
    }
}

impl Editor for UIFrontEnd {
    fn size(&self) -> (i32, i32) {
        (800, 600)
    }

    fn position(&self) -> (i32, i32) {
        (0, 0)
    }

    fn open(&mut self, parent: *mut c_void) -> bool {
        #[cfg(target_os = "windows")]
        let parent = to_windows_handle(parent);

        #[cfg(target_os = "macos")]
        let parent = to_macos_handle(parent);

        #[cfg(not(any(target_os = "macos", target_os = "windows")))]
        error!("currently unsupported os!");

        let settings = iced_baseview::Settings {
            window: baseview::WindowOpenOptions {
                title: "Revisit".to_string(),
                size: baseview::Size::new(self.size().0 as f64, self.size().1 as f64),
                scale: baseview::WindowScalePolicy::SystemScaleFactor,
                parent: baseview::Parent::WithParent(parent),
            },
            flags: (self.params.clone(), self.params.notify.clone()),
        };
        if self.handle.is_none() {
            let (handle, _) = iced_baseview::Runner::<UIFrontEnd>::open(settings);
            self.handle = Some(handle);
        }
        true
    }

    fn idle(&mut self) {}

    fn close(&mut self) {
        if let Some(handle) = &mut self.handle {
            handle.request_window_close()
        };
        self.handle = None;
    }

    fn is_open(&mut self) -> bool {
        self.handle.is_some()
    }

    fn key_up(&mut self, keycode: vst::editor::KeyCode) -> bool {
        if let Some(handle) = &mut self.handle {
            match keycode.key {
                vst::editor::Key::Control | vst::editor::Key::Shift => {
                    if self.control_key == keyboard_types::KeyState::Down {
                        let event = to_keyboard_event(keycode, keyboard_types::KeyState::Up);
                        handle.send_baseview_event(baseview::Event::Keyboard(event));
                        self.control_key = keyboard_types::KeyState::Up;
                        return true;
                    }
                }
                _ => (),
            }
        }
        false
    }

    fn key_down(&mut self, keycode: vst::editor::KeyCode) -> bool {
        if let Some(handle) = &mut self.handle {
            match keycode.key {
                vst::editor::Key::Control | vst::editor::Key::Shift => {
                    if self.control_key == keyboard_types::KeyState::Up {
                        let event = to_keyboard_event(keycode, keyboard_types::KeyState::Down);
                        handle.send_baseview_event(baseview::Event::Keyboard(event));
                        self.control_key = keyboard_types::KeyState::Down;
                        return true;
                    }
                }
                _ => (),
            }
        }
        false
    }
}

/// The widget name for a given parameter
fn widget_name(param: ParameterType) -> String {
    use OSCParameterType::*;
    match param {
        ParameterType::MasterVolume => "Master Volume".to_string(),
        ParameterType::OSC1(param) | ParameterType::OSC2(param) => match param {
            Volume => "Volume".to_string(),
            FineTune => "Fine".to_string(),
            CoarseTune => "Coarse".to_string(),
            Shape => "Shape".to_string(),
            Warp => "Warp".to_string(),
            VolAttack => "A".to_string(),
            VolHold => "H".to_string(),
            VolDecay => "D".to_string(),
            VolSustain => "S".to_string(),
            VolRelease => "R".to_string(),
            VolLFOAmplitude => "Amplitude".to_string(),
            VolLFOPeriod => "Period".to_string(),
            PitchAttack => "A".to_string(),
            PitchHold => "H".to_string(),
            PitchDecay => "D".to_string(),
            PitchMultiply => "M".to_string(),
            PitchRelease => "R".to_string(),
            PitchLFOAmplitude => "Amplitude".to_string(),
            PitchLFOPeriod => "Period".to_string(),
            FilterType => "Filter Type".to_string(),
            FilterFreq => "Freq.".to_string(),
            FilterQ => "Q".to_string(),
            FilterGain => "Gain".to_string(),
        },
        ParameterType::OSC2Mod => "OSC 2 Mod".to_string(),
    }
}

/// Convert a `vst::Keycode` + `keyboard_types::KeyState` into an actual
/// `keyboard_types::KeyboardEvent`. This is minimal and pretty much only works
/// with the Control key, so you'll need to fix it if you wanna do more with it.
/// This is used to send keyboard events into `iced_baseview`.
fn to_keyboard_event(
    _: vst::editor::KeyCode,
    state: keyboard_types::KeyState,
) -> keyboard_types::KeyboardEvent {
    match state {
        keyboard_types::KeyState::Down => keyboard_types::KeyboardEvent {
            state,
            key: keyboard_types::Key::Control,
            code: keyboard_types::Code::ControlLeft,
            location: keyboard_types::Location::Standard,
            modifiers: keyboard_types::Modifiers::CONTROL,
            repeat: false,
            is_composing: false,
        },
        keyboard_types::KeyState::Up => keyboard_types::KeyboardEvent {
            state,
            key: keyboard_types::Key::Control,
            code: keyboard_types::Code::ControlLeft,
            location: keyboard_types::Location::Standard,
            modifiers: keyboard_types::Modifiers::empty(),
            repeat: false,
            is_composing: false,
        },
    }
}

#[cfg(target_os = "windows")]
fn to_windows_handle(parent: *mut c_void) -> RawWindowHandle {
    use raw_window_handle::windows::WindowsHandle;
    let mut handle = WindowsHandle::empty();
    handle.hwnd = parent;
    handle.hinstance = std::ptr::null_mut();
    RawWindowHandle::Windows(handle)
}

#[cfg(target_os = "macos")]
fn to_macos_handle(parent: *mut c_void) -> RawWindowHandle {
    use raw_window_handle::macos::MacOSHandle;
    let mut handle = MacOSHandle::empty();
    handle.ns_view = parent;
    handle.ns_window = std::ptr::null_mut();
    RawWindowHandle::MacOS(handle)
}
