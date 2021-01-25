use std::sync::Arc;
use std::{ffi::c_void, hash::Hash};

use iced::{futures, Command, Subscription};

use iced_baseview::{Application, Handle, WindowSubs};

use raw_window_handle::RawWindowHandle;
use tokio::sync::Notify;
use vst::{editor::Editor, host::Host};

use crate::{
    params::{ModulationType, ParameterType, RawParameters},
    ui_tabs::MainTab,
};

/// A GUI message.
#[derive(Debug, Clone, Copy)]
pub enum Message {
    /// This indicates that the GUI had changed the ModulationType parameter
    OSC2ModChanged(ModulationType),
    /// These indicate that the GUI has changed a parameter via a knob
    ParameterChanged(f32, ParameterType),
    /// These are called to make the GUI update itself. Usually, this means that
    /// the host has changed a parameter, so the GUI should change to match.
    /// Note that this works because sending a message also causes `iced` to
    /// call `view` and do a screen update. If it didn't do that, then this won't
    /// work.
    ForceRedraw,
}

/// The struct which manages the GUI. This struct handles both the messages
/// passed to it by iced as well as communciating with host VST for GUI stuff.
pub struct UIFrontEnd {
    main_tab: MainTab,
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
        let ui = UIFrontEnd {
            main_tab: MainTab::new(params.as_ref()),
            params,
            handle: None,
            notifier,
            control_key: keyboard_types::KeyState::Up,
        };
        (ui, Command::none())
    }

    /// React to an incoming message
    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        self.main_tab.update(message, self.params.as_ref());

        // Make the VST host update its own parameter display. This is needed
        // so the host actually has updates with GUI.
        match message {
            Message::OSC2ModChanged(_) | Message::ParameterChanged(_, _) => {
                self.params.host.update_display()
            }
            Message::ForceRedraw => (),
        }
        Command::none()
    }

    /// Set up the GUI elements
    /// Note that this isn't called every frame--it's only called when there's
    /// an update to the view (which happens to be only when messages happen)
    fn view(&mut self) -> iced::Element<'_, Self::Message> {
        let (screen_width, screen_height) = self.size();
        self.main_tab.view(
            screen_width as u32,
            screen_height as u32,
            self.params.as_ref(),
        )
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
