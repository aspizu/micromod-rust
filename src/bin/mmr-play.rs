use std::{
    io::{BufWriter, Write as _},
    process::Command,
};

use micromod_c2rust_test::MmC2r;

fn main() {
    let mod_data =
        std::fs::read(std::env::args_os().nth(1).expect("Need path to song"))
            .unwrap();
    let output_file = std::fs::File::create("micromod_out.pcm").unwrap();
    let mut writer = BufWriter::new(output_file);
    let mut state = MmC2r::new(&mod_data, 48_000).unwrap();
    loop {
        let mut out = [0; 4096];
        if !state.get_audio(&mut out, 2048) {
            break;
        }
        for sample in out {
            writer.write_all(sample.to_le_bytes().as_slice()).unwrap();
        }
    }
    Command::new("ffmpeg")
        .args([
            "-f",
            "s16le",
            "-ar",
            "48000",
            "-ac",
            "2",
            "-i",
            "micromod_out.pcm",
            "micromod_out.mp3",
            "-y",
        ])
        .status()
        .unwrap();
}
