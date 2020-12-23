use std::ffi::c_void;

use iced::{Column, Command, Container, Element, Length};
use iced_audio::{knob, FloatRange};
use iced_audio::{Knob, Normal};
use raw_window_handle::RawWindowHandle;
use vst::{editor::Editor, host::Host};

use crate::RawParameters;

#[derive(Debug, Clone, Copy)]
pub enum Message {
    Example(Normal),
}

pub struct UIFrontEnd {
    is_open: bool,
    master_vol_knob: knob::State,
    params: std::sync::Arc<RawParameters>,
}

impl iced_baseview::Application for UIFrontEnd {
    type Message = Message;
    type Executor = iced_baseview::executor::Default;
    type Flags = std::sync::Arc<RawParameters>;

    fn new(params: Self::Flags) -> (Self, Command<Self::Message>) {
        let master_vol_range = FloatRange::default_bipolar();
        let ui = UIFrontEnd {
            master_vol_knob: knob::State::new(master_vol_range.normal_param(1.0, 1.0)),
            params,
            is_open: false,
        };
        (ui, Command::none())
    }

    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        match message {
            Message::Example(normal) => {
                self.params.volume.set(normal.as_f32());
                // Make the host DAW update its own parameter display
                self.params.host.update_display();
            }
        }
        Command::none()
    }

    fn view(&mut self) -> iced::Element<'_, Self::Message> {
        let master_vol_widget = Knob::new(&mut self.master_vol_knob, Message::Example);

        let content: Element<_> = Column::new()
            .max_width(100)
            .max_height(100)
            .spacing(20)
            .padding(20)
            .push(master_vol_widget)
            .into();

        Container::new(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }
}

impl Editor for UIFrontEnd {
    fn size(&self) -> (i32, i32) {
        (200, 200)
    }

    fn position(&self) -> (i32, i32) {
        (0, 0)
    }

    fn open(&mut self, parent: *mut c_void) -> bool {
        #[cfg(windows)]
        {
            let parent = to_windows_handle(parent);

            let settings = iced_baseview::Settings {
                window: iced_baseview::settings::Window {
                    title: "Revisit".to_string(),
                    logical_size: (self.size().0 as u32, self.size().1 as u32),
                    scale_policy: iced_baseview::WindowScalePolicy::SystemScaleFactor,
                },
                flags: self.params.clone(),
            };

            let (_handle, _) = iced_baseview::Runner::<UIFrontEnd>::open(
                settings,
                iced_baseview::Parent::WithParent(parent),
            );
            self.is_open = true;
            true
        }

        #[cfg(not(windows))]
        {
            false
        }
    }

    fn is_open(&mut self) -> bool {
        self.is_open
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
