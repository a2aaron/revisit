use std::ffi::c_void;
use std::sync::Arc;

use iced::{futures, Align, Checkbox, Column, Command, Element, Row, Subscription};
use iced_audio::{knob, v_slider, Knob, NormalParam, VSlider};
use iced_baseview::{Application, Handle, WindowSubs};
use raw_window_handle::RawWindowHandle;
use tokio::sync::Notify;
use vst::{editor::Editor, host::Host};

use crate::{ParameterType, Parameters, RawParameters};

// Create a widget (actually a column) that:
// 1. Uses `$state` as the widget's `$widget::State` struct
// 2. Sends the message ParameterChanged(normal, $parameter)
// 3. Has the title `widget_name($parameter)`
macro_rules! widget {
    ($widget:ident, $state:expr, $parameter:expr) => {
        with_label(
            $widget::new($state, |normal| {
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
    Column::new().spacing(5).padding(20)
}

fn make_pane<'a>(
    title: &str,
    widgets: Vec<(Vec<Element<'a, Message>>, &str)>,
) -> Column<'a, Message> {
    let mut pane = column().push(iced::Text::new(title));
    for (knobs, title) in widgets.into_iter() {
        pane = pane.push(knob_row(knobs, title));
    }
    pane
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

pub struct UIFrontEnd {
    master_vol: v_slider::State,
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
    low_pass: knob::State,
    fm_on_off: bool,
    fm_vol: knob::State,
    fm_pitch: knob::State,
    fm_shape: knob::State,
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
        let params_real = Parameters::from(param_ref);
        let ui = UIFrontEnd {
            master_vol: v_slider::State::new(make_normal_param(
                param_ref,
                ParameterType::MasterVolume,
            )),
            attack: make_knob(param_ref, ParameterType::VolAttack),
            hold: make_knob(param_ref, ParameterType::VolHold),
            decay: make_knob(param_ref, ParameterType::VolDecay),
            sustain: make_knob(param_ref, ParameterType::VolSustain),
            release: make_knob(param_ref, ParameterType::VolRelease),
            vol_lfo_amplitude: make_knob(param_ref, ParameterType::VolLFOAmplitude),
            vol_lfo_period: make_knob(param_ref, ParameterType::VolLFOPeriod),
            note_shape: make_knob(param_ref, ParameterType::Shape),
            note_warp: make_knob(param_ref, ParameterType::Warp),
            pitch_attack: make_knob(param_ref, ParameterType::PitchAttack),
            pitch_hold: make_knob(param_ref, ParameterType::PitchHold),
            pitch_decay: make_knob(param_ref, ParameterType::PitchDecay),
            pitch_multiply: make_knob(param_ref, ParameterType::PitchMultiply),
            pitch_release: make_knob(param_ref, ParameterType::PitchRelease),
            pitch_lfo_amplitude: make_knob(param_ref, ParameterType::PitchLFOAmplitude),
            pitch_lfo_period: make_knob(param_ref, ParameterType::PitchLFOPeriod),
            low_pass: make_knob(param_ref, ParameterType::LowPassAlpha),
            fm_on_off: params_real.fm_on_off,
            fm_vol: make_knob(param_ref, ParameterType::FMVolume),
            fm_pitch: make_knob(param_ref, ParameterType::FMPitchMultiplier),
            fm_shape: make_knob(param_ref, ParameterType::FMShape),
            params,
            handle: None,
            notifier,
            control_key: keyboard_types::KeyState::Up,
        };
        (ui, Command::none())
    }

    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        match message {
            Message::ParameterChanged(value, ParameterType::FMOnOff) => {
                self.params.set(value, ParameterType::FMOnOff);
                self.fm_on_off = value > 0.5;
            }
            Message::ParameterChanged(value, param) => {
                self.params.set(value, param);
            }
            Message::ForceRedraw => {
                self.master_vol
                    .set(self.params.get(ParameterType::MasterVolume).into());
                self.attack
                    .set(self.params.get(ParameterType::VolAttack).into());
                self.hold
                    .set(self.params.get(ParameterType::VolHold).into());
                self.decay
                    .set(self.params.get(ParameterType::VolDecay).into());
                self.sustain
                    .set(self.params.get(ParameterType::VolSustain).into());
                self.release
                    .set(self.params.get(ParameterType::VolRelease).into());
                self.vol_lfo_amplitude
                    .set(self.params.get(ParameterType::VolLFOAmplitude).into());
                self.vol_lfo_period
                    .set(self.params.get(ParameterType::VolLFOPeriod).into());
                self.note_shape
                    .set(self.params.get(ParameterType::Shape).into());
                self.note_warp
                    .set(self.params.get(ParameterType::Warp).into());
                self.pitch_attack
                    .set(self.params.get(ParameterType::PitchAttack).into());
                self.pitch_hold
                    .set(self.params.get(ParameterType::PitchHold).into());
                self.pitch_decay
                    .set(self.params.get(ParameterType::PitchDecay).into());
                self.pitch_multiply
                    .set(self.params.get(ParameterType::PitchMultiply).into());
                self.pitch_release
                    .set(self.params.get(ParameterType::PitchRelease).into());
                self.pitch_lfo_amplitude
                    .set(self.params.get(ParameterType::PitchLFOAmplitude).into());
                self.pitch_lfo_period
                    .set(self.params.get(ParameterType::PitchLFOPeriod).into());
                self.low_pass
                    .set(self.params.get(ParameterType::LowPassAlpha).into());
                // TODO : Don't use a RawParameters for this
                self.fm_on_off = self.params.get(ParameterType::FMOnOff) > 0.5;
                self.fm_vol
                    .set(self.params.get(ParameterType::FMVolume).into());
                self.fm_pitch
                    .set(self.params.get(ParameterType::FMPitchMultiplier).into());
                self.fm_shape
                    .set(self.params.get(ParameterType::FMShape).into());
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

        let shape_widget = widget!(Knob, &mut self.note_shape, ParameterType::Shape);
        let warp_widget = widget!(Knob, &mut self.note_warp, ParameterType::Warp);

        let attack_widget = widget!(Knob, &mut self.attack, ParameterType::VolAttack);
        let hold_widget = widget!(Knob, &mut self.hold, ParameterType::VolHold);
        let decay_widget = widget!(Knob, &mut self.decay, ParameterType::VolDecay);
        let sustain_widget = widget!(Knob, &mut self.sustain, ParameterType::VolSustain);
        let release_widget = widget!(Knob, &mut self.release, ParameterType::VolRelease);

        let vol_lfo_amplitude_widget = widget!(
            Knob,
            &mut self.vol_lfo_amplitude,
            ParameterType::VolLFOAmplitude
        );
        let vol_lfo_period_widget =
            widget!(Knob, &mut self.vol_lfo_period, ParameterType::VolLFOPeriod);

        let pitch_attack_widget = widget!(Knob, &mut self.pitch_attack, ParameterType::PitchAttack);
        let pitch_hold_widget = widget!(Knob, &mut self.pitch_hold, ParameterType::PitchHold);
        let pitch_decay_widget = widget!(Knob, &mut self.pitch_decay, ParameterType::PitchDecay);
        let pitch_multiply_widget =
            widget!(Knob, &mut self.pitch_multiply, ParameterType::PitchMultiply);
        let pitch_release_widget =
            widget!(Knob, &mut self.pitch_release, ParameterType::PitchRelease);

        let pitch_lfo_amplitude_widget = widget!(
            Knob,
            &mut self.pitch_lfo_amplitude,
            ParameterType::PitchLFOAmplitude
        );
        let pitch_lfo_period_widget = widget!(
            Knob,
            &mut self.pitch_lfo_period,
            ParameterType::PitchLFOPeriod
        );

        let low_pass_widget = widget!(Knob, &mut self.low_pass, ParameterType::LowPassAlpha);
        // TODO: Consider a smarter way for messages that doesn't involve always casting to f32
        let fm_on_off_widget = Checkbox::new(self.fm_on_off, "ON/OFF", |on_off| {
            let normal = if on_off { 1.0 } else { 0.0 };
            Message::ParameterChanged(normal, ParameterType::FMOnOff)
        })
        .into();
        let fm_vol_widget = widget!(Knob, &mut self.fm_vol, ParameterType::FMVolume);
        let fm_pitch_widget = widget!(Knob, &mut self.fm_pitch, ParameterType::FMPitchMultiplier);
        let fm_shape_widget = widget!(Knob, &mut self.fm_shape, ParameterType::FMShape);

        let osc_pane = make_pane(
            "OSC 1",
            vec![
                (vec![shape_widget, warp_widget], "Shape"),
                (
                    vec![
                        attack_widget,
                        hold_widget,
                        decay_widget,
                        sustain_widget,
                        release_widget,
                    ],
                    "Envelope",
                ),
                (vec![vol_lfo_amplitude_widget, vol_lfo_period_widget], "LFO"),
            ],
        );

        let pitch_pane = make_pane(
            "PITCH",
            vec![
                (
                    vec![
                        pitch_attack_widget,
                        pitch_hold_widget,
                        pitch_decay_widget,
                        pitch_multiply_widget,
                        pitch_release_widget,
                    ],
                    "Envelope",
                ),
                (
                    vec![pitch_lfo_amplitude_widget, pitch_lfo_period_widget],
                    "LFO",
                ),
            ],
        );

        let filter_pane = make_pane("FILTER", vec![(vec![low_pass_widget], "Low Pass")]);

        let fm_pane = column().push(iced::Text::new("FM")).push(row(vec![
            fm_on_off_widget,
            fm_vol_widget,
            fm_pitch_widget,
            fm_shape_widget,
        ]));

        let master_pane = master_vol_widget;

        Row::new()
            .push(Column::new().push(osc_pane).push(pitch_pane).spacing(20))
            .push(Column::new().push(fm_pane).push(filter_pane).spacing(20))
            .push(master_pane)
            .max_width(screen_width)
            .max_height(screen_height)
            .spacing(20)
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
    match param {
        ParameterType::MasterVolume => "Master Volume".to_string(),
        ParameterType::Shape => "Shape".to_string(),
        ParameterType::Warp => "Warp".to_string(),
        ParameterType::VolAttack => "A".to_string(),
        ParameterType::VolHold => "H".to_string(),
        ParameterType::VolDecay => "D".to_string(),
        ParameterType::VolSustain => "S".to_string(),
        ParameterType::VolRelease => "R".to_string(),
        ParameterType::VolLFOAmplitude => "Amplitude".to_string(),
        ParameterType::VolLFOPeriod => "Period".to_string(),
        ParameterType::PitchAttack => "A".to_string(),
        ParameterType::PitchHold => "H".to_string(),
        ParameterType::PitchDecay => "D".to_string(),
        ParameterType::PitchMultiply => "M".to_string(),
        ParameterType::PitchRelease => "R".to_string(),
        ParameterType::PitchLFOAmplitude => "Amplitude".to_string(),
        ParameterType::PitchLFOPeriod => "Period".to_string(),
        ParameterType::LowPassAlpha => "Alpha".to_string(),
        ParameterType::FMOnOff => "ON/OFF".to_string(),
        ParameterType::FMVolume => "Amount".to_string(),
        ParameterType::FMPitchMultiplier => "Pitch Multiplier".to_string(),
        ParameterType::FMShape => "Shape".to_string(),
        ParameterType::Error => "Bepis".to_string(),
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
