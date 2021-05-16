use std::{
    ffi::OsStr,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};

use crate::{
    params::{OSCParams, Parameters},
    sound_gen::Decibel,
};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct PresetData {
    pub name: String,
    pub master_vol: Decibel,
    pub osc_1: PresetDataOSC,
    pub osc_2: PresetDataOSC,
}

impl PresetData {
    pub fn from_params(params: &Parameters, name: String) -> Self {
        PresetData {
            name,
            master_vol: params.master_vol,
            osc_1: PresetDataOSC::from(&params.osc_1),
            osc_2: PresetDataOSC::from(&params.osc_2),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct PresetDataOSC {
    pub volume: Decibel,
    pub vol_sustain: Decibel,
}

impl From<&OSCParams> for PresetDataOSC {
    fn from(params: &OSCParams) -> Self {
        PresetDataOSC {
            volume: params.volume,
            vol_sustain: params.vol_adsr.sustain,
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
pub fn get_presets_from_folder(folder: impl AsRef<Path>) -> std::io::Result<Vec<PathBuf>> {
    let mut presets = vec![];
    for entry in (std::fs::read_dir(folder)?).flatten() {
        if Some(std::ffi::OsStr::new("json")) == entry.path().extension() {
            presets.push(entry.path());
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
    Ok(serde_json::to_writer_pretty(file, &preset)?)
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
