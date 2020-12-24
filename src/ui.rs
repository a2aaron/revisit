use std::ffi::c_void;
use std::sync::Arc;

use iced::{futures, Align, Column, Command, Container, Element, Length, Subscription};
use iced_audio::{knob, v_slider, FloatRange, Knob, Normal, NormalParam, VSlider};
use iced_baseview::{Application, Handle, WindowSubs};
use log::info;
use raw_window_handle::RawWindowHandle;
use tokio::sync::Notify;
use vst::{editor::Editor, host::Host};

use crate::{ParameterType, RawParameters};

struct RecipeStruct {
    notifier: Arc<Notify>,
}

impl<H, I> iced_native::subscription::Recipe<H, I> for RecipeStruct
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
    decay: knob::State,
    sustain: knob::State,
    release: knob::State,
    params: std::sync::Arc<RawParameters>,
    handle: Option<Handle>,
    notifier: Arc<Notify>,
}

impl Application for UIFrontEnd {
    type Message = Message;
    type Executor = iced_baseview::executor::Default;
    type Flags = (Arc<RawParameters>, Arc<Notify>);

    fn new(flags: Self::Flags) -> (Self, Command<Self::Message>) {
        let (params, notifier) = flags;
        let master_vol = params.volume.get();
        let attack = params.vol_adsr.attack.get();
        let decay = params.vol_adsr.decay.get();
        let sustain = params.vol_adsr.sustain.get();
        let release = params.vol_adsr.release.get();
        let ui = UIFrontEnd {
            master_vol: v_slider::State::new(make_normal_param(master_vol, 1.0)),
            attack: knob::State::new(make_normal_param(attack, 1.0)),
            decay: knob::State::new(make_normal_param(decay, 1.0)),
            sustain: knob::State::new(make_normal_param(sustain, 1.0)),
            release: knob::State::new(make_normal_param(release, 1.0)),
            params,
            handle: None,
            notifier,
        };
        info!("Called new!");
        (ui, Command::none())
    }

    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        match message {
            Message::ParameterChanged(value, param) => self.params.set(value, param),
            Message::ForceRedraw => {
                self.master_vol.set(Normal::new(self.params.volume.get()));
                self.attack
                    .set(Normal::new(self.params.vol_adsr.attack.get()));
                self.decay
                    .set(Normal::new(self.params.vol_adsr.decay.get()));
                self.sustain
                    .set(Normal::new(self.params.vol_adsr.sustain.get()));
                self.release
                    .set(Normal::new(self.params.vol_adsr.release.get()));
            }
        }
        // Make the host DAW update its own parameter display
        self.params.host.update_display();
        Command::none()
    }

    fn view(&mut self) -> iced::Element<'_, Self::Message> {
        let master_vol_widget = VSlider::new(&mut self.master_vol, |normal| {
            Message::ParameterChanged(normal.as_f32(), ParameterType::MasterVolume)
        });
        let attack_widget = Knob::new(&mut self.attack, |normal| {
            Message::ParameterChanged(normal.as_f32(), ParameterType::VolAttack)
        });

        let content: Element<_> = Column::new()
            .max_width(700)
            .max_height(600)
            .spacing(20)
            .padding(20)
            .push(
                Column::new()
                    .max_height(100)
                    .align_items(Align::Center)
                    .push(master_vol_widget)
                    .push(iced::Text::new("Master Volume")),
            )
            .push(
                Column::new()
                    .max_height(100)
                    .align_items(Align::Center)
                    .push(attack_widget)
                    .push(iced::Text::new("Attack")),
            )
            .into();

        Container::new(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }

    fn subscription(
        &self,
        _window_subs: &mut WindowSubs<Self::Message>,
    ) -> Subscription<Self::Message> {
        let recipe = RecipeStruct {
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
        #[cfg(windows)]
        {
            let parent = to_windows_handle(parent);

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
            info!("Opened the GUI");
            true
        }

        #[cfg(not(windows))]
        {
            false
        }
    }

    fn idle(&mut self) {}

    fn close(&mut self) {
        info!("Closed the GUI");

        if let Some(handle) = &mut self.handle {
            handle.request_window_close()
        };
        self.handle = None;
    }

    fn is_open(&mut self) -> bool {
        self.handle.is_some()
    }
}

fn make_normal_param(value: f32, default: f32) -> NormalParam {
    NormalParam {
        value: Normal::new(value),
        default: Normal::new(default),
    }
}

#[cfg(windows)]
fn to_windows_handle(parent: *mut c_void) -> RawWindowHandle {
    use raw_window_handle::windows::WindowsHandle;
    let mut handle = WindowsHandle::empty();
    handle.hwnd = parent;
    handle.hinstance = std::ptr::null_mut();
    RawWindowHandle::Windows(handle)
}
