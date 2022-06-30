# ReViSiT - A VST synth written in Rust

## Features
- Pitchbend
- 2 Oscillators with four signal generator types
- Controllable volume and pitch envelopes
- Controllable volume and pitch LFOs
- Amplitude/FM/Phase/Warp mix-modulation
- Controllable filter bank
- A modulation bank, able to modulate any parameter
- A UI made in Iced
- Editable JSON presets

## How to Install
Simply clone the repo and build it with `cargo build --release`. This should build a `revisit.dll` file in `target/release/`. Move this file to your favorite DAW's VST folder, and everything should just work when you import it into the DAW.

This VST has been tested to work with the following DAWs on Windows 10
- Ableton
- LMMS
- VSTHost

I do not know if it will work with other OSes or other DAWs, although hopefully it should.

## Architecture

ReViSiT is built using RudioAudio's [vst-rs](https://github.com/RustAudio/vst-rs) crate along with [iced](https://github.com/iced-rs/iced) for the GUI front end. 

If you wish to fork or use ReViSiT as an example VST, feel free. `lib.rs`, `sound_gen.rs`, and `params.rs` contain most of the code required for the core sound generation, while `ui.rs` and `ui_tabs.rs` contain most of the UI code.