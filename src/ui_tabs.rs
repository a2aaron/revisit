use std::hash::Hash;

use iced::{
    Align, Background, Column, Element, HorizontalAlignment, Point, Rectangle, Row, Vector,
    VerticalAlignment,
};
use iced_audio::{
    knob::{self, LineCap},
    v_slider, Knob, NormalParam, VSlider,
};
use iced_graphics::{
    canvas::{Frame, LineJoin, Stroke},
    Backend, Primitive, Renderer,
};
use iced_native::{layout, mouse, Widget};

use crate::{
    params::{
        biquad_to_string, to_filter_type, EnvelopeParam, ModulationType, OSCParameterType, OSCType,
        ParameterType, RawOSC, RawParameters,
    },
    sound_gen::NoteShape,
    ui::Message,
};

const LABEL_WIDTH: u16 = 45;
const KNOB_SPACING: u16 = 12;
const KNOB_SIZE: iced::Length = iced::Length::Units(29);
const PANE_SPACING: u16 = 15;
const PANE_PADDING: u16 = 15;
const INTERPANE_SPACING: u16 = 15;

const GREEN: iced::Color = iced::Color {
    r: 0.0,
    g: 1.0,
    b: 0.0,
    a: 1.0,
};

const BLACK_GREEN: iced::Color = iced::Color {
    r: 0.0,
    g: 0.2,
    b: 0.0,
    a: 1.0,
};

const GREY: iced::Color = iced::Color {
    r: 0.33,
    g: 0.33,
    b: 0.33,
    a: 1.0,
};

const GREEN_TRANS: iced::Color = iced::Color {
    r: 0.0,
    g: 1.0,
    b: 0.0,
    a: 0.5,
};

/// Use: widget!(identifier, state, param, title);
/// Create a widget (actually a column) that:
/// 1. Uses `$state` as the widget's `$widget::State` struct
/// 2. Sends the message ParameterChanged(normal, $parameter)
/// 3. Has the title `title` (or `widget_name($parameter)` if not provided)
macro_rules! widget {
    (VSlider, $state:expr, $parameter:expr, $title:expr) => {
        with_label(
            VSlider::new($state, move |normal| {
                Message::ParameterChanged(normal.as_f32(), $parameter)
            }),
            $title,
        );
    };

    ($widget:ident, $state:expr, $parameter:expr, $title:expr) => {
        with_label(
            $widget::new($state, move |normal| {
                Message::ParameterChanged(normal.as_f32(), $parameter)
            })
            .size(KNOB_SIZE),
            $title,
        );
    };

    ($widget:ident, $state:expr, $parameter:expr) => {
        widget!($widget, $state, $parameter, &widget_name($parameter));
    };
}

/// The main tab of the GUI, holding all the main knob stuff.
pub struct MainTab {
    pub master_vol: v_slider::State,
    pub osc_1: OSCKnobs,
    pub osc_2: OSCKnobs,
}

impl MainTab {
    pub fn new(params: &RawParameters) -> MainTab {
        MainTab {
            master_vol: v_slider::State::new(make_normal_param(
                params,
                ParameterType::MasterVolume,
            )),
            osc_1: OSCKnobs::new(params, OSCType::OSC1),
            osc_2: OSCKnobs::new(params, OSCType::OSC2),
        }
    }

    pub fn update(&mut self, message: Message, params: &RawParameters) {
        match message {
            // The GUI has changed the modulation type
            Message::OSC2ModChanged(mod_type) => {
                // TODO: Consider using an actual parameter instead of RawParameters
                params.osc_2_mod.set(mod_type.into());
            }
            // The GUI has changed a parameter via knob
            Message::ParameterChanged(value, param) => {
                // We set the parameter according to the changed value.
                params.set(value, param);

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
                    // This is definitely unreachable because OSC2Mod handling is
                    // done by the OSC2ModChanged message
                    ParameterType::OSC2Mod => unreachable!(),
                    ParameterType::MasterVolume => (),
                }
            }
            // The host has changed a parameter, or a redraw was requested
            // We update the knobs based on the current parameter values
            Message::ForceRedraw => {
                // TODO : Don't use a RawParameters for this? Instead consider
                // using a normal Parameter struct and letting the knobs have
                // actual values instead of weird 0.0-1.0 normalized values.
                // This is fine as it is right now though.
                self.master_vol
                    .set_normal(params.get(ParameterType::MasterVolume).into());
                self.osc_1.update(&params.osc_1);
                self.osc_2.update(&params.osc_2);
            }
            Message::ChangeTab(_) => (),
        }
    }

    pub fn view(
        &mut self,
        screen_width: u32,
        screen_height: u32,
        params: &RawParameters,
    ) -> iced::Element<'_, Message> {
        let master_vol_widget = widget!(VSlider, &mut self.master_vol, ParameterType::MasterVolume);

        // TODO: Consider a smarter way for messages that doesn't involve always casting to f32
        let osc_2_mod = iced::Element::new(ModTypeSelector::new(
            params.osc_2_mod.get().into(),
            vec!["Mix", "AM", "FM", "PM", "Warp"],
        ));

        let master_pane = master_vol_widget;
        let osc_1 = OSCKnobs::make_widget(&mut self.osc_1, OSCType::OSC1, None);
        let osc_2 = OSCKnobs::make_widget(&mut self.osc_2, OSCType::OSC2, Some(osc_2_mod));

        Row::new()
            .push(osc_1)
            .push(osc_2)
            .push(master_pane)
            .max_width(screen_width)
            .max_height(screen_height)
            .height(iced::Length::Shrink)
            .spacing(INTERPANE_SPACING)
            .into()
    }
}

/// The tab which has all the various modulation envelopes/LFOs
pub struct ModulationTab {}

impl ModulationTab {
    pub fn new(_params: &RawParameters) -> ModulationTab {
        ModulationTab {}
    }

    pub fn update(&mut self, message: Message, _params: &RawParameters) {
        // TODO!
        match message {
            _ => (),
        }
    }

    pub fn view(
        &mut self,
        _screen_width: u32,
        _screen_height: u32,
        _params: &RawParameters,
    ) -> iced::Element<'_, Message> {
        iced::Text::new("! ! TODO ! !").size(100).into()
    }
}

/// The tab which handles preset loading and saving.
pub struct PresetTab {
    // uh...
}

impl PresetTab {
    pub fn new(_params: &RawParameters) -> PresetTab {
        PresetTab {}
    }

    pub fn update(&mut self, message: Message, _params: &RawParameters) {
        // TODO!
        match message {
            _ => (),
        }
    }

    pub fn view(
        &mut self,
        _screen_width: u32,
        _screen_height: u32,
        _params: &RawParameters,
    ) -> iced::Element<'_, Message> {
        iced::Text::new("! ! TODO ! !").size(48).into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tabs {
    Main,
    Preset,
    Modulation,
}

/// A struct keeping track of the various knob states an oscillator has.
/// The ranges are used for snapping the knobs.
pub struct OSCKnobs {
    volume: knob::State,
    pan: knob::State,
    phase: knob::State,
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
    fn new(params: &RawParameters, osc: OSCType) -> OSCKnobs {
        use EnvelopeParam::*;
        use OSCParameterType::*;
        OSCKnobs {
            volume: make_knob(params, (Volume, osc).into()),
            pan: make_knob(params, (Pan, osc).into()),
            phase: make_knob(params, (Phase, osc).into()),
            coarse_tune: make_knob(params, (CoarseTune, osc).into()),
            fine_tune: make_knob(params, (FineTune, osc).into()),
            attack: make_knob(params, (VolumeEnv(Attack), osc).into()),
            hold: make_knob(params, (VolumeEnv(Hold), osc).into()),
            decay: make_knob(params, (VolumeEnv(Decay), osc).into()),
            sustain: make_knob(params, (VolumeEnv(Sustain), osc).into()),
            release: make_knob(params, (VolumeEnv(Release), osc).into()),
            vol_lfo_amplitude: make_knob(params, (VolLFOAmplitude, osc).into()),
            vol_lfo_period: make_knob(params, (VolLFOPeriod, osc).into()),
            note_shape: make_knob(params, (Shape, osc).into()),
            note_warp: make_knob(params, (Warp, osc).into()),
            pitch_attack: make_knob(params, (PitchAttack, osc).into()),
            pitch_hold: make_knob(params, (PitchHold, osc).into()),
            pitch_decay: make_knob(params, (PitchDecay, osc).into()),
            pitch_multiply: make_knob(params, (PitchMultiply, osc).into()),
            pitch_release: make_knob(params, (PitchRelease, osc).into()),
            pitch_lfo_amplitude: make_knob(params, (PitchLFOAmplitude, osc).into()),
            pitch_lfo_period: make_knob(params, (PitchLFOPeriod, osc).into()),
            filter_type: make_knob(params, (FilterType, osc).into()),
            filter_freq: make_knob(params, (FilterFreq, osc).into()),
            filter_q: make_knob(params, (FilterQ, osc).into()),
            filter_gain: make_knob(params, (FilterGain, osc).into()),
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

        use EnvelopeParam::*;
        use OSCParameterType::*;
        self.volume.set_normal(osc.get(Volume).into());
        self.pan.set_normal(osc.get(Pan).into());
        self.phase.set_normal(osc.get(Phase).into());

        set_knob_with_range(
            &mut self.coarse_tune,
            &self.coarse_tune_range,
            osc.get(CoarseTune),
        );

        self.fine_tune.set_normal(osc.get(FineTune).into());
        self.attack.set_normal(osc.get(VolumeEnv(Attack)).into());
        self.hold.set_normal(osc.get(VolumeEnv(Hold)).into());
        self.decay.set_normal(osc.get(VolumeEnv(Decay)).into());
        self.sustain.set_normal(osc.get(VolumeEnv(Sustain)).into());
        self.release.set_normal(osc.get(VolumeEnv(Release)).into());
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
        use EnvelopeParam::*;
        use OSCParameterType::*;

        let volume = widget!(Knob, &mut self.volume, (Volume, osc).into());
        let pan = widget!(Knob, &mut self.pan, (Pan, osc).into());
        let phase = widget!(Knob, &mut self.phase, (Phase, osc).into());
        let fine_tune = widget!(Knob, &mut self.fine_tune, (FineTune, osc).into());
        let coarse_tune = widget!(Knob, &mut self.coarse_tune, (CoarseTune, osc).into());

        let shape_title = NoteShape::from_warp(
            self.note_shape.normal().into(),
            self.note_warp.normal().into(),
        )
        .to_string();
        let shape = widget!(
            Knob,
            &mut self.note_shape,
            (Shape, osc).into(),
            &shape_title
        );

        let warp = widget!(Knob, &mut self.note_warp, (Warp, osc).into());

        let attack = widget!(Knob, &mut self.attack, (VolumeEnv(Attack), osc).into());
        let hold = widget!(Knob, &mut self.hold, (VolumeEnv(Hold), osc).into());
        let decay = widget!(Knob, &mut self.decay, (VolumeEnv(Decay), osc).into());
        let sustain = widget!(Knob, &mut self.sustain, (VolumeEnv(Sustain), osc).into());
        let release = widget!(Knob, &mut self.release, (VolumeEnv(Release), osc).into());

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

        let filter_type_title = biquad_to_string(to_filter_type(
            self.filter_type.normal().into(),
            self.filter_gain.normal().into(),
        ));

        let filter_type = widget!(
            Knob,
            &mut self.filter_type,
            (FilterType, osc).into(),
            &filter_type_title
        );
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
                (vec![volume, pan, phase, coarse_tune, fine_tune], "Sound"),
                (vec![shape, warp], "Shape"),
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
            .padding(PANE_PADDING)
            .spacing(PANE_SPACING)
            .into()
    }
}

struct ModTypeSelector {
    text: Vec<&'static str>,
    text_size: f32,
    // Which element is currently selected
    pub selected: ModulationType,
    width: iced::Length,
    height: iced::Length,
}

impl ModTypeSelector {
    fn new(selected: ModulationType, text: Vec<&'static str>) -> ModTypeSelector {
        ModTypeSelector {
            text,
            text_size: 15.0,
            selected,
            width: iced::Length::Units(200),
            height: iced::Length::Units(20),
        }
    }
}

fn to_mod_type(x: usize) -> ModulationType {
    match x {
        0 => ModulationType::Mix,
        1 => ModulationType::AmpMod,
        2 => ModulationType::FreqMod,
        3 => ModulationType::PhaseMod,
        4 => ModulationType::WarpMod,
        _ => unreachable!(),
    }
}

fn to_usize(x: ModulationType) -> usize {
    match x {
        ModulationType::Mix => 0,
        ModulationType::AmpMod => 1,
        ModulationType::FreqMod => 2,
        ModulationType::PhaseMod => 3,
        ModulationType::WarpMod => 4,
    }
}

impl<B: Backend> Widget<Message, Renderer<B>> for ModTypeSelector {
    fn width(&self) -> iced::Length {
        self.width
    }

    fn height(&self) -> iced::Length {
        self.height
    }

    fn layout(
        &self,
        _renderer: &Renderer<B>,
        limits: &iced_native::layout::Limits,
    ) -> layout::Node {
        let limits = limits.width(self.width).height(self.height);
        let size = limits.resolve(iced_native::Size::ZERO);
        layout::Node::new(size)
    }

    fn draw(
        &self,
        _renderer: &mut Renderer<B>,
        _defaults: &<iced_graphics::Renderer<B> as iced_native::Renderer>::Defaults,
        layout: iced_native::Layout<'_>,
        _cursor_position: iced::Point,
        _viewport: &iced::Rectangle,
    ) -> (Primitive, mouse::Interaction) {
        let rect = Rectangle::new(Point::ORIGIN, layout.bounds().size());
        let rects = split_rect_horiz(rect, self.text.len());

        let mut frame = Frame::new(rect.size());
        let mut stroke = Stroke {
            color: iced_native::Color::BLACK,
            width: 1.0,
            line_cap: LineCap::Round,
            line_join: LineJoin::Miter,
        };

        for (i, rect) in rects.iter().enumerate() {
            if self.selected == to_mod_type(i) {
                stroke = stroke.with_color(BLACK_GREEN);
            } else {
                stroke = stroke.with_color(GREY);
            }

            let text = iced_graphics::canvas::Text {
                content: self.text[i].to_string(),
                position: Point::new(((i as f32) + 0.5) * rect.width, rect.height / 2.0),
                color: if self.selected == to_mod_type(i) {
                    BLACK_GREEN
                } else {
                    GREY
                },
                size: self.text_size,
                font: iced_graphics::Font::Default,
                vertical_alignment: VerticalAlignment::Center,
                horizontal_alignment: HorizontalAlignment::Center,
            };
            frame.fill_text(text);
        }

        let rounded_rect = Primitive::Quad {
            bounds: rects[to_usize(self.selected)],
            background: Background::Color(GREEN_TRANS),
            border_radius: 3.0,
            border_width: 1.0,
            border_color: GREEN,
        };

        let primitive = Primitive::Group {
            primitives: vec![rounded_rect, frame.into_geometry().into_primitive()],
        };

        let primitive = Primitive::Translate {
            translation: Vector::new(layout.position().x, layout.position().y),
            content: Box::new(primitive),
        };

        (primitive, mouse::Interaction::Idle)
    }

    fn hash_layout(&self, state: &mut iced_native::Hasher) {
        struct Marker;
        std::any::TypeId::of::<Marker>().hash(state);
        // We only need to hash things which could cause a relayout, which is
        // just the width and height
        self.width.hash(state);
        self.height.hash(state);
    }

    fn on_event(
        &mut self,
        event: iced_native::Event,
        layout: iced_native::Layout<'_>,
        cursor_position: iced::Point,
        messages: &mut Vec<Message>,
        _renderer: &Renderer<B>,
        _clipboard: Option<&dyn iced_native::Clipboard>,
    ) -> iced_native::event::Status {
        if let iced_native::Event::Mouse(mouse_event) = event {
            if mouse_event == mouse::Event::ButtonPressed(mouse::Button::Left) {
                let bounds = layout.bounds();
                let squares = split_rect_horiz(bounds, self.text.len());
                for (i, square) in squares.iter().enumerate() {
                    if square.contains(cursor_position) {
                        self.selected = to_mod_type(i);
                        match i {
                            0 => messages.push(Message::OSC2ModChanged(ModulationType::Mix)),
                            1 => messages.push(Message::OSC2ModChanged(ModulationType::AmpMod)),
                            2 => messages.push(Message::OSC2ModChanged(ModulationType::FreqMod)),
                            3 => messages.push(Message::OSC2ModChanged(ModulationType::PhaseMod)),
                            4 => messages.push(Message::OSC2ModChanged(ModulationType::WarpMod)),
                            _ => unreachable!(),
                        }
                        break;
                    }
                }
                return iced_native::event::Status::Captured;
            }
        }
        iced_native::event::Status::Ignored
    }
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

    let mut pane = column().push(
        row(vec![
            iced::Text::new(title)
                .vertical_alignment(VerticalAlignment::Center)
                .into(),
            on_off,
        ])
        .align_items(Align::Center),
    );

    for (knobs, title) in widgets.into_iter() {
        pane = pane.push(knob_row(knobs, title));
    }
    pane
}

/// Make a pane without the additional `on_off` widget.
fn make_pane<'a>(
    title: &str,
    widgets: Vec<(Vec<Element<'a, Message>>, &str)>,
) -> Column<'a, Message> {
    make_pane_with_checkbox(title, None, widgets)
}

/// Create a row of `knobs` with the title `title`.
fn knob_row<'a>(knobs: Vec<Element<'a, Message>>, title: &str) -> Element<'a, Message> {
    Column::new()
        .push(iced::Text::new(title).size(18))
        .push(
            Row::with_children(knobs)
                .align_items(Align::Center)
                .spacing(KNOB_SPACING),
        )
        .align_items(Align::Start)
        .spacing(2)
        .into()
}

/// Create a combined element with the label `title` under a `widget`
fn with_label<'a>(widget: impl Into<Element<'a, Message>>, title: &str) -> Element<'a, Message> {
    let text = iced::Text::new(title)
        .size(15)
        .width(iced::Length::Units(LABEL_WIDTH))
        .horizontal_alignment(HorizontalAlignment::Center);
    Column::with_children(vec![widget.into(), text.into()])
        .align_items(Align::Center)
        .into()
}

/// Convience function to make `knob::State` out of a `RawParameters`
/// and `ParameterType`
fn make_knob(params: &RawParameters, param_type: ParameterType) -> knob::State {
    knob::State::new(make_normal_param(params, param_type))
}

/// Make a row of widgets with better default spacing.
fn row(widgets: Vec<Element<'_, Message>>) -> Row<'_, Message> {
    Row::with_children(widgets).spacing(5)
}

/// Make an empty column with better default spacing.
fn column<'a>() -> Column<'a, Message> {
    Column::new().spacing(5)
}

/// Convience function to make a `NormalParam` using a `RawParameters`. The
/// `NormalParam` will have a default value of whatever the default value is for
/// a `RawParameters`.
fn make_normal_param(params: &RawParameters, param_type: ParameterType) -> NormalParam {
    NormalParam {
        value: params.get(param_type).into(),
        default: RawParameters::get_default(param_type).into(),
    }
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

/// Split a rectangle horizontally
fn split_rect_horiz(rect: Rectangle, num_rects: usize) -> Vec<Rectangle> {
    let mut vec = Vec::with_capacity(num_rects);
    let new_width = rect.width / num_rects as f32;

    for i in 0..num_rects {
        vec.push(Rectangle {
            x: rect.x + i as f32 * new_width,
            y: rect.y,
            width: new_width,
            height: rect.height,
        })
    }
    vec
}

/// The widget name for a given parameter
fn widget_name(param: ParameterType) -> String {
    use EnvelopeParam::*;
    use OSCParameterType::*;
    match param {
        ParameterType::MasterVolume => "Master Volume".to_string(),
        ParameterType::OSC1(param) | ParameterType::OSC2(param) => match param {
            Volume => "Volume".to_string(),
            Phase => "Phase".to_string(),
            Pan => "Pan".to_string(),
            FineTune => "Fine".to_string(),
            CoarseTune => "Coarse".to_string(),
            Shape => "Shape".to_string(),
            Warp => "Warp".to_string(),
            VolumeEnv(Attack) => "A".to_string(),
            VolumeEnv(Hold) => "H".to_string(),
            VolumeEnv(Decay) => "D".to_string(),
            VolumeEnv(Sustain) => "S".to_string(),
            VolumeEnv(Release) => "R".to_string(),
            VolumeEnv(Multiply) => "M".to_string(),
            VolLFOAmplitude => "Amount".to_string(),
            VolLFOPeriod => "Period".to_string(),
            PitchAttack => "A".to_string(),
            PitchHold => "H".to_string(),
            PitchDecay => "D".to_string(),
            PitchMultiply => "M".to_string(),
            PitchRelease => "R".to_string(),
            PitchLFOAmplitude => "Amount".to_string(),
            PitchLFOPeriod => "Period".to_string(),
            FilterType => "Filter Type".to_string(),
            FilterFreq => "Freq.".to_string(),
            FilterQ => "Q".to_string(),
            FilterGain => "Gain".to_string(),
        },
        ParameterType::OSC2Mod => "OSC 2 Mod".to_string(),
    }
}
