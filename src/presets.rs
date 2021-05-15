use std::{
    ffi::OsStr,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};

use crate::{
    params::{
        GeneralEnvParams, ModBankSend, ModulationBank, ModulationSend, ModulationType, OSCParams,
        OSCType, Parameters, RawParameters, VolEnvParams, LFO,
    },
    sound_gen::{Decibel, FilterParams},
};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct PresetData {
    pub name: String,
    pub master_vol: f32,
}

impl PresetData {
    pub fn from_raw(params: &RawParameters, name: String) -> Self {
        PresetData {
            name,
            master_vol: params.master_vol.get(),
        }
    }
}

impl From<PresetData> for Parameters {
    fn from(preset: PresetData) -> Parameters {
        fn default_osc() -> OSCParams {
            OSCParams {
                volume: Decibel::from_db(crate::params::DEFAULT_OSC_VOL),
                shape: crate::sound_gen::NoteShape::Sine,
                pan: 0.5,
                phase: 0.0,
                coarse_tune: 0,
                fine_tune: 0.0,
                vol_adsr: default_vol_env(),
                vol_lfo: default_lfo(),
                pitch_adsr: default_env(),
                pitch_lfo: default_lfo(),
                filter_params: FilterParams {
                    filter: biquad::Type::LowPass,
                    freq: 22100.0,
                    q_value: 1.00,
                },
            }
        }

        fn default_env() -> GeneralEnvParams {
            GeneralEnvParams {
                attack: 0.001,
                hold: 0.015,
                decay: 0.001,
                sustain: 0.0,
                release: 0.001,
                multiply: 0.0,
            }
        }

        fn default_vol_env() -> VolEnvParams {
            VolEnvParams {
                attack: 0.002,
                hold: 0.0,
                decay: 15.0,
                sustain: Decibel::from_db(crate::params::DEFAULT_VOL_SUSTAIN),
                release: 35.0,
            }
        }

        fn default_lfo() -> LFO {
            LFO {
                amplitude: 0.0,
                period: 300.0,
            }
        }

        Parameters {
            osc_1: default_osc(),
            osc_2: default_osc(),
            master_vol: Decibel::from_db(preset.master_vol),
            osc_2_mod: ModulationType::AmpMod,
            mod_bank: ModulationBank {
                env_1: default_env(),
                env_1_send: ModBankSend {
                    mod_type: ModulationSend::Amplitude,
                    osc: OSCType::OSC1,
                },
                env_2: default_env(),
                env_2_send: ModBankSend {
                    mod_type: ModulationSend::Amplitude,
                    osc: OSCType::OSC2,
                },
            },
        }
    }
}

impl From<Parameters> for PresetData {
    fn from(params: Parameters) -> Self {
        PresetData {
            name: "Unnamed Preset".to_string(),
            master_vol: params.master_vol.get_db(),
        }
    }
}

/// Try to parse a given file into a `PresetData`
pub fn try_parse_file(file: impl AsRef<Path>) -> Result<PresetData, Box<dyn std::error::Error>> {
    let mut file = std::fs::File::open(file)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(serde_json::from_str(&contents)?)
}

/// Read a folder and try to parse any `json` files in it into a PresetData.
pub fn get_presets_from_folder(folder: impl AsRef<Path>) -> std::io::Result<Vec<PresetData>> {
    let mut presets = vec![];
    for entry in (std::fs::read_dir(folder)?).flatten() {
        if Some(std::ffi::OsStr::new("json")) == entry.path().extension() {
            match try_parse_file(&entry.path()) {
                Ok(preset) => {
                    presets.push(preset);
                    log::info!("Parsed {}!", entry.path().display());
                }
                Err(err) => log::info!("Couldn't parse {}: {:?}", entry.path().display(), err),
            }
        }
    }
    Ok(presets)
}

/// Save a PresetData to the given path. This returns an error if the file could
/// not be saved or if serialization fails for any reason.
pub fn save_preset_to_file(
    preset: PresetData,
    path: impl AsRef<Path>,
) -> Result<(), Box<dyn std::error::Error>> {
    let file = File::create(&path)?;
    Ok(serde_json::to_writer(file, &preset)?)
}

/// Get a file name which is not currently used by any file in the given folder
/// The file will have the file name and extention provided.
pub fn get_free_file_name(
    folder: impl AsRef<Path>,
    file_name: impl AsRef<OsStr>,
    extention: impl AsRef<OsStr>,
) -> (PathBuf, usize) {
    let mut i = 0;
    loop {
        let mut path = folder.as_ref().to_path_buf();
        path.set_file_name(format!("{} {}", file_name.as_ref().to_str().unwrap(), i));
        path.set_extension(&extention);
        if !path.exists() {
            return (path, i);
        }
        i += 1;
    }
}
