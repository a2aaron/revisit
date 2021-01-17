use std::ffi::c_void;
use std::sync::Arc;

use iced::{futures, Align, Column, Command, Element, Row, Subscription};
use iced_audio::{knob, v_slider, IntRange, Knob, Normal, NormalParam, VSlider};
use iced_baseview::{Application, Handle, WindowSubs};
use raw_window_handle::RawWindowHandle;
use tokio::sync::Notify;
use vst::{editor::Editor, host::Host};

use crate::params::{
    ModulationType, OSCParameterType, OSCType, ParameterType, RawOSC, RawParameters,
};

// Create a widget (actually a column) that:
// 1. Uses `$state` as the widget's `$widget::State` struct
// 2. Sends the message ParameterChanged(normal, $parameter)
// 3. Has the title `widget_name($parameter)`
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

fn with_label<'a>(widget: impl Into<Element<'a, Message>>, title: &str) -> Element<'a, Message> {
    Column::with_children(vec![widget.into(), iced::Text::new(title).size(15).into()])
        .align_items(Align::Center)
        .into()
}

fn make_normal_param(param_ref: &RawParameters, param_type: ParameterType) -> NormalParam {
    NormalParam {
        value: param_ref.get(param_type).into(),
        default: RawParameters::get_default(param_type).into(),
    }
}

fn make_knob(param_ref: &RawParameters, param_type: ParameterType) -> knob::State {
    knob::State::new(make_normal_param(param_ref, param_type))
}

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

fn row(widgets: Vec<Element<'_, Message>>) -> Element<'_, Message> {
    Row::with_children(widgets).spacing(5).into()
}

fn column<'a>() -> Column<'a, Message> {
    Column::new().spacing(5)
}

fn make_pane<'a>(
    title: &str,
    widgets: Vec<(Vec<Element<'a, Message>>, &str)>,
) -> Column<'a, Message> {
    make_pane_with_checkbox(title, None, widgets)
}

fn make_pane_with_checkbox<'a>(
    title: &str,
    on_off: Option<Element<'a, Message>>,
    widgets: Vec<(Vec<Element<'a, Message>>, &str)>,
) -> Column<'a, Message> {
    let on_off = on_off.unwrap_or_else(|| {
        iced::widget::Space::new(iced::Length::Shrink, iced::Length::Shrink).into()
    });
    let mut pane = column().push(row(vec![iced::Text::new(title).into(), on_off]));
    for (knobs, title) in widgets.into_iter() {
        pane = pane.push(knob_row(knobs, title));
    }
    pane
}

struct TruncatingIntRange {
    num_regions: usize,
}

impl TruncatingIntRange {
    fn snap(&self, value: f32) -> f32 {
        snap_float(value, self.num_regions)
    }

    fn snap_knob(&self, knob: &mut knob::State) {
        knob.normal_param.value = self.snap(knob.normal().into()).into();
    }
}

/// Snap a float value in range 0.0-1.0 to the nearest f32 region
/// For example, snap_float(_, 4) will snap a float to either:
/// 0.0, 0.333, 0.666, or 1.0
fn snap_float(value: f32, num_regions: usize) -> f32 {
    let num_regions = num_regions as f32;
    (num_regions * value).floor() / (num_regions - 1.0)
}

struct NotifyRecipe {
    notifier: Arc<Notify>,
}

impl<H, I> iced_native::subscription::Recipe<H, I> for NotifyRecipe
where
    H: std::hash::Hasher,
{
    type Output = Message;

    fn hash(&self, state: &mut H) {
        use std::hash::Hash;

        std::any::TypeId::of::<Self>().hash(state);
    }

    fn stream(
        self: Box<Self>,
        _: futures::stream::BoxStream<'static, I>,
    ) -> futures::stream::BoxStream<'static, Self::Output> {
        Box::pin(futures::stream::unfold(
            self.notifier.clone(),
            |notifier| async move {
                notifier.notified().await;
                Some((Message::ForceRedraw, notifier))
            },
        ))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    ParameterChanged(f32, ParameterType),
    ForceRedraw,
}

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
            // Subtract one because we want there to be n different values, and these ranges happen to be end-inclusive
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
        // self.filter_type.set_normal(osc.get(FilterType).into());
        set_knob_with_range(
            &mut self.filter_type,
            &self.filter_type_range,
            osc.get(FilterType),
            // crate::params::FILTER_TYPE_VARIANT_COUNT,
        );
        self.filter_freq.set_normal(osc.get(FilterFreq).into());
        self.filter_q.set_normal(osc.get(FilterQ).into());
        self.filter_gain.set_normal(osc.get(FilterGain).into());
    }

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

        let osc_pane = make_pane_with_checkbox(
            "OSC",
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

pub struct UIFrontEnd {
    master_vol: v_slider::State,
    osc_1: OSCKnobs,
    osc_2: OSCKnobs,
    osc_2_mod: knob::State,
    osc_2_mod_range: TruncatingIntRange,
    params: std::sync::Arc<RawParameters>,
    handle: Option<Handle>,
    notifier: Arc<Notify>,
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

    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        match message {
            // The GUI has changed a parameter
            Message::ParameterChanged(value, param) => {
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
            }
            // The host has changed a parameter, or a redraw was requested
            // We update the knobs based on the current parameter values
            Message::ForceRedraw => {
                self.master_vol
                    .set_normal(self.params.get(ParameterType::MasterVolume).into());
                self.osc_1.update(&self.params.osc_1);
                self.osc_2.update(&self.params.osc_2);
                // TODO : Don't use a RawParameters for this
                self.osc_2_mod
                    .set_normal(self.params.get(ParameterType::OSC2Mod).into());
            }
        }
        // Make the host DAW update its own parameter display
        self.params.host.update_display();
        Command::none()
    }

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
