//! Experiment with converting [Micromod](https://github.com/martincameron/micromod) with
//! [c2rust](https://c2rust.com/).
//!
//! Safetyfication done manually

#![warn(missing_docs)]

mod consts;

use std::cmp::Ordering;

#[derive(Copy, Clone, Default)]
struct Channel {
    note: Note,
    period: u16,
    porta_period: u16,
    sample_offset: usize,
    sample_idx: usize,
    step: usize,
    volume: u8,
    panning: u8,
    fine_tune: u8,
    ampl: u8,
    mute: u8,
    id: u8,
    instrument: u8,
    assigned: u8,
    porta_speed: u8,
    pl_row: u8,
    fx_count: u8,
    vibrato_type: u8,
    vibrato_phase: u8,
    vibrato_speed: u8,
    vibrato_depth: u8,
    tremolo_type: u8,
    tremolo_phase: u8,
    tremolo_speed: u8,
    tremolo_depth: u8,
    tremolo_add: i8,
    vibrato_add: i8,
    arpeggio_add: i8,
}
#[derive(Copy, Clone, Default)]
struct Note {
    key: u16,
    instrument: u8,
    effect: u8,
    param: u8,
}
#[derive(Copy, Clone)]
struct Instrument<'src> {
    volume: u8,
    fine_tune: u8,
    loop_start: usize,
    loop_length: usize,
    sample_data: &'src [i8],
}

impl Instrument<'_> {
    fn dummy() -> Self {
        Self {
            volume: 0,
            fine_tune: 0,
            loop_start: 0,
            loop_length: 0,
            sample_data: &[],
        }
    }
}

/// Immutable source data of the module
struct ModSrc<'src> {
    instruments: Vec<Instrument<'src>>,
    module_data: &'src [i8],
    pattern_data: &'src [u8],
    sequence: &'src [u8],
    num_patterns: isize,
    num_channels: isize,
    song_length: isize,
}

#[derive(Default)]
struct PlaybackState {
    gain: isize,
    c2_rate: isize,
    tick_len: isize,
    tick_offset: isize,
    pattern: isize,
    break_pattern: isize,
    row: isize,
    next_row: isize,
    tick: isize,
    speed: isize,
    pl_count: isize,
    pl_channel: isize,
    random_seed: isize,
}

/// A micromod decoding instance thingy.
pub struct MmC2r<'src> {
    sample_rate: isize,
    channels: [Channel; 16],
    src: ModSrc<'src>,
    playback: PlaybackState,
}

fn calculate_num_patterns(module_header: &[i8]) -> isize {
    let mut num_patterns_0;
    let mut order_entry;
    let mut pattern_0;
    num_patterns_0 = 0;
    pattern_0 = 0;
    while pattern_0 < 128 {
        order_entry = (module_header[(952 + pattern_0) as usize] as i32
            & 0x7f_i32) as isize;
        if order_entry >= num_patterns_0 {
            num_patterns_0 = order_entry + 1;
        }
        pattern_0 += 1;
    }
    num_patterns_0
}

fn calculate_num_channels(module_header: &[i8]) -> Option<isize> {
    const MAX_CHANNELS: isize = 16;
    let numchan: isize = match ((module_header[1082] as isize) << 8)
        | module_header[1083] as isize
    {
        // M.K.  M!K!     N.T.     FLT4
        0x4b2e | 0x4b21 | 0x542e | 0x5434 => 4,
        // xCHN
        0x484e => (module_header[1080] - 48) as isize,
        // xxCH
        0x4348 => {
            (((module_header[1080] - 48) * 10) + (module_header[1081] - 48))
                as isize
        }
        // Not recognised.
        _ => 0,
    };
    if numchan > MAX_CHANNELS {
        None
    } else {
        Some(numchan)
    }
}
fn unsigned_short_big_endian(buf: &[i8], offset: isize) -> isize {
    ((buf[offset as usize] as i32 & 0xff_i32) << 8
        | buf[(offset + 1) as usize] as i32 & 0xff_i32) as isize
}
fn set_tempo(tempo: isize, tick_len: &mut isize, sample_rate: isize) {
    *tick_len = ((sample_rate << 1) + (sample_rate >> 1)) / tempo;
}
fn update_frequency(
    chan: &mut Channel,
    sample_rate: isize,
    gain: &mut isize,
    c2_rate: &mut isize,
) {
    let mut period;
    let mut volume;

    period = (chan.period as i32 + chan.vibrato_add as i32) as isize;
    period = (period * consts::ARP_TUNING[chan.arpeggio_add as usize] as isize)
        >> 11;
    period = (period >> 1) + (period & 1);
    if period < 14 {
        period = 6848;
    }
    let freq = (*c2_rate * 428 / period) as usize;
    chan.step = (freq << 14).wrapping_div(sample_rate as usize);
    volume = (chan.volume as i32 + chan.tremolo_add as i32) as isize;
    volume = volume.clamp(0, 64);
    chan.ampl = ((volume * *gain) >> 5) as u8;
}
fn tone_portamento(chan: &mut Channel) {
    let mut source;

    source = chan.period as isize;
    let dest = chan.porta_period as isize;
    match source.cmp(&dest) {
        Ordering::Less => {
            source += chan.porta_speed as isize;
            if source > dest {
                source = dest;
            }
        }
        Ordering::Equal => { /* Do absolutely nothing */ }
        Ordering::Greater => {
            source -= chan.porta_speed as isize;
            if source < dest {
                source = dest;
            }
        }
    }
    chan.period = source as u16;
}
fn volume_slide(chan: &mut Channel, param: isize) {
    let mut volume;
    volume = chan.volume as isize + (param >> 4) - (param & 0xf);
    volume = volume.clamp(0, 64);
    chan.volume = volume as u8;
}
fn waveform(phase: isize, type_0: isize, random_seed: &mut isize) -> isize {
    let mut amplitude: isize = 0;
    match type_0 & 0x3 {
        0 => {
            amplitude = consts::SINE_TABLE[(phase & 0x1f) as usize] as isize;
            if phase & 0x20 > 0 {
                amplitude = -amplitude;
            }
        }
        1 => {
            amplitude = 255 - (((phase + 0x20) & 0x3f) << 3);
        }
        2 => {
            amplitude = 255 - ((phase & 0x20) << 4);
        }
        3 => {
            amplitude = (*random_seed >> 20) - 255;
            *random_seed = (*random_seed * 65 + 17) & 0x1fffffff;
        }
        _ => {}
    }
    amplitude
}
fn vibrato(chan: &mut Channel, random_seed: &mut isize) {
    chan.vibrato_add = ((waveform(
        chan.vibrato_phase as isize,
        chan.vibrato_type as isize,
        random_seed,
    ) * chan.vibrato_depth as isize)
        >> 7) as i8;
}
fn tremolo(chan: &mut Channel, random_seed: &mut isize) {
    chan.tremolo_add = ((waveform(
        chan.tremolo_phase as isize,
        chan.tremolo_type as isize,
        random_seed,
    ) * chan.tremolo_depth as isize)
        >> 6) as i8;
}
fn trigger(channel: &mut Channel, instruments: &[Instrument]) {
    let period;

    let ins = channel.note.instrument as isize;
    if ins > 0 && ins < 32 {
        channel.assigned = ins as u8;
        channel.sample_offset = 0;
        channel.fine_tune = instruments[ins as usize].fine_tune;
        channel.volume = instruments[ins as usize].volume;
        if instruments[ins as usize].loop_length > 0
            && channel.instrument as i32 > 0
        {
            channel.instrument = ins as u8;
        }
    }
    if channel.note.effect as i32 == 0x9_i32 {
        channel.sample_offset =
            ((channel.note.param as i32 & 0xff_i32) << 8) as usize;
    } else if channel.note.effect as i32 == 0x15_i32 {
        channel.fine_tune = channel.note.param;
    }
    if channel.note.key as i32 > 0 {
        period = ((channel.note.key as i32
            * consts::FINE_TUNING[(channel.fine_tune as i32 & 0xf_i32) as usize]
                as i32)
            >> 11) as isize;
        channel.porta_period = ((period >> 1) + (period & 1)) as u16;
        if channel.note.effect as i32 != 0x3_i32
            && channel.note.effect as i32 != 0x5_i32
        {
            channel.instrument = channel.assigned;
            channel.period = channel.porta_period;
            channel.sample_idx = channel.sample_offset << 14;
            if (channel.vibrato_type as i32) < 4 {
                channel.vibrato_phase = 0;
            }
            if (channel.tremolo_type as i32) < 4 {
                channel.tremolo_phase = 0;
            }
        }
    }
}
fn channel_row(
    chan: &mut Channel,
    sample_rate: isize,
    src: &ModSrc,
    playback: &mut PlaybackState,
) {
    let volume;
    let period;
    let effect = chan.note.effect as isize;
    let param = chan.note.param as isize;
    let fresh0 = &mut chan.fx_count;
    *fresh0 = 0;
    let fresh1 = &mut chan.arpeggio_add;
    *fresh1 = *fresh0 as i8;
    let fresh2 = &mut chan.tremolo_add;
    *fresh2 = *fresh1;
    chan.vibrato_add = *fresh2;
    if !(effect == 0x1d && param > 0) {
        trigger(chan, &src.instruments);
    }
    match effect {
        3 => {
            if param > 0 {
                chan.porta_speed = param as u8;
            }
        }
        4 => {
            if param & 0xf0 > 0 {
                chan.vibrato_speed = (param >> 4) as u8;
            }
            if param & 0xf > 0 {
                chan.vibrato_depth = (param & 0xf) as u8;
            }
            vibrato(chan, &mut playback.random_seed);
        }
        6 => {
            vibrato(chan, &mut playback.random_seed);
        }
        7 => {
            if param & 0xf0 > 0 {
                chan.tremolo_speed = (param >> 4) as u8;
            }
            if param & 0xf > 0 {
                chan.tremolo_depth = (param & 0xf) as u8;
            }
            tremolo(chan, &mut playback.random_seed);
        }
        8 => {
            if src.num_channels != 4 {
                chan.panning = (if param < 128 { param } else { 127 }) as u8;
            }
        }
        11 => {
            if playback.pl_count < 0 {
                playback.break_pattern = param;
                playback.next_row = 0;
            }
        }
        12 => {
            chan.volume = (if param > 64 { 64 } else { param }) as u8;
        }
        13 => {
            if playback.pl_count < 0 {
                if playback.break_pattern < 0 {
                    playback.break_pattern = playback.pattern + 1;
                }
                playback.next_row = (param >> 4) * 10 + (param & 0xf);
                if playback.next_row >= 64 {
                    playback.next_row = 0;
                }
            }
        }
        15 => {
            if param > 0 {
                if param < 32 {
                    playback.speed = param;
                    playback.tick = playback.speed;
                } else {
                    set_tempo(param, &mut playback.tick_len, sample_rate);
                }
            }
        }
        17 => {
            period = chan.period as isize - param;
            chan.period = (if period < 0 { 0 } else { period }) as u16;
        }
        18 => {
            period = chan.period as isize + param;
            chan.period = (if period > 65535 { 65535 } else { period }) as u16;
        }
        20 => {
            if param < 8 {
                chan.vibrato_type = param as u8;
            }
        }
        22 => {
            if param == 0 {
                chan.pl_row = playback.row as u8;
            }
            if (chan.pl_row as isize) < playback.row
                && playback.break_pattern < 0
            {
                if playback.pl_count < 0 {
                    playback.pl_count = param;
                    playback.pl_channel = chan.id as isize;
                }
                if playback.pl_channel == chan.id as isize {
                    if playback.pl_count == 0 {
                        chan.pl_row = (playback.row + 1) as u8;
                    } else {
                        playback.next_row = chan.pl_row as isize;
                    }
                    playback.pl_count -= 1;
                }
            }
        }
        23 => {
            if param < 8 {
                chan.tremolo_type = param as u8;
            }
        }
        26 => {
            volume = chan.volume as isize + param;
            chan.volume = (if volume > 64 { 64 } else { volume }) as u8;
        }
        27 => {
            volume = chan.volume as isize - param;
            chan.volume = (if volume < 0 { 0 } else { volume }) as u8;
        }
        28 => {
            if param <= 0 {
                chan.volume = 0;
            }
        }
        30 => {
            playback.tick = playback.speed + playback.speed * param;
        }
        _ => {}
    }
    update_frequency(
        chan,
        sample_rate,
        &mut playback.gain,
        &mut playback.c2_rate,
    );
}
fn channel_tick(
    chan: &mut Channel,
    sample_rate: isize,
    gain: &mut isize,
    c2_rate: &mut isize,
    random_seed: &mut isize,
    instruments: &[Instrument],
) {
    let period;
    let effect = chan.note.effect as isize;
    let param = chan.note.param as isize;
    let fresh3 = &mut chan.fx_count;
    *fresh3 = (*fresh3).wrapping_add(1);
    match effect {
        1 => {
            period = chan.period as isize - param;
            chan.period = (if period < 0 { 0 } else { period }) as u16;
        }
        2 => {
            period = chan.period as isize + param;
            chan.period = (if period > 65535 { 65535 } else { period }) as u16;
        }
        3 => {
            tone_portamento(chan);
        }
        4 => {
            let fresh4 = &mut chan.vibrato_phase;
            *fresh4 = (*fresh4 as i32 + chan.vibrato_speed as i32) as u8;
            vibrato(chan, random_seed);
        }
        5 => {
            tone_portamento(chan);
            volume_slide(chan, param);
        }
        6 => {
            let fresh5 = &mut chan.vibrato_phase;
            *fresh5 = (*fresh5 as i32 + chan.vibrato_speed as i32) as u8;
            vibrato(chan, random_seed);
            volume_slide(chan, param);
        }
        7 => {
            let fresh6 = &mut chan.tremolo_phase;
            *fresh6 = (*fresh6 as i32 + chan.tremolo_speed as i32) as u8;
            tremolo(chan, random_seed);
        }
        10 => {
            volume_slide(chan, param);
        }
        14 => {
            if chan.fx_count as i32 > 2 {
                chan.fx_count = 0;
            }
            if chan.fx_count as i32 == 0 {
                chan.arpeggio_add = 0;
            }
            if chan.fx_count as i32 == 1 {
                chan.arpeggio_add = (param >> 4) as i8;
            }
            if chan.fx_count as i32 == 2 {
                chan.arpeggio_add = (param & 0xf) as i8;
            }
        }
        25 => {
            if chan.fx_count as isize >= param {
                chan.fx_count = 0;
                chan.sample_idx = 0;
            }
        }
        28 => {
            if param == chan.fx_count as isize {
                chan.volume = 0;
            }
        }
        29 => {
            if param == chan.fx_count as isize {
                trigger(chan, instruments);
            }
        }
        _ => {}
    }
    if effect > 0 {
        update_frequency(chan, sample_rate, gain, c2_rate);
    }
}
fn sequence_row(state: &mut MmC2r) -> bool {
    let mut song_end = false;
    let mut chan_idx;
    let mut pat_offset;
    let mut effect;
    let mut param;
    let mut note;
    if state.playback.next_row < 0 {
        state.playback.break_pattern = state.playback.pattern + 1;
        state.playback.next_row = 0;
    }
    if state.playback.break_pattern >= 0 {
        if state.playback.break_pattern >= state.src.song_length {
            state.playback.next_row = 0;
            state.playback.break_pattern = state.playback.next_row;
        }
        if state.playback.break_pattern <= state.playback.pattern {
            song_end = true;
        }
        state.playback.pattern = state.playback.break_pattern;
        chan_idx = 0;
        while chan_idx < state.src.num_channels {
            state.channels[chan_idx as usize].pl_row = 0;
            chan_idx += 1;
        }
        state.playback.break_pattern = -1;
    }
    state.playback.row = state.playback.next_row;
    state.playback.next_row = state.playback.row + 1;
    if state.playback.next_row >= 64 {
        state.playback.next_row = -1;
    }
    pat_offset = ((state.src.sequence[state.playback.pattern as usize] as i32
        * 64) as isize
        + state.playback.row)
        * state.src.num_channels
        * 4;
    chan_idx = 0;
    while chan_idx < state.src.num_channels {
        note = &mut (state.channels[chan_idx as usize]).note;
        let pattern_data = state.src.pattern_data;
        note.key =
            ((pattern_data[pat_offset as usize] as i32 & 0xf_i32) << 8) as u16;
        let fresh7 = &mut note.key;
        *fresh7 = (*fresh7 as i32
            | pattern_data[(pat_offset + 1) as usize] as i32)
            as u16;
        note.instrument =
            (pattern_data[(pat_offset + 2) as usize] as i32 >> 4) as u8;
        let fresh8 = &mut note.instrument;
        *fresh8 = (*fresh8 as i32
            | pattern_data[pat_offset as usize] as i32 & 0x10_i32)
            as u8;
        effect =
            (pattern_data[(pat_offset + 2) as usize] as i32 & 0xf_i32) as isize;
        param = pattern_data[(pat_offset + 3) as usize] as isize;
        pat_offset += 4;
        if effect == 0xe {
            effect = 0x10 | param >> 4;
            param &= 0xf;
        }
        if effect == 0 && param > 0 {
            effect = 0xe;
        }
        note.effect = effect as u8;
        note.param = param as u8;
        channel_row(
            &mut state.channels[chan_idx as usize],
            state.sample_rate,
            &state.src,
            &mut state.playback,
        );
        chan_idx += 1;
    }
    song_end
}
fn sequence_tick(state: &mut MmC2r) -> bool {
    let mut song_end = false;
    let mut chan_idx;
    state.playback.tick -= 1;
    if state.playback.tick <= 0 {
        state.playback.tick = state.playback.speed;
        song_end = sequence_row(state);
    } else {
        chan_idx = 0;
        while chan_idx < state.src.num_channels {
            channel_tick(
                &mut state.channels[chan_idx as usize],
                state.sample_rate,
                &mut state.playback.gain,
                &mut state.playback.c2_rate,
                &mut state.playback.random_seed,
                &state.src.instruments,
            );
            chan_idx += 1;
        }
    }
    song_end
}
fn resample(
    chan: &mut Channel,
    buf: &mut [i16],
    offset: isize,
    count: isize,
    instruments: &[Instrument],
) {
    let mut epos;
    let mut buf_idx: usize = (offset << 1) as usize;
    let buf_end: usize = ((offset + count) << 1) as usize;
    let mut sidx: usize = chan.sample_idx;
    let step: usize = chan.step;
    let llen: usize = instruments[chan.instrument as usize].loop_length;
    let lep1: usize =
        (instruments[chan.instrument as usize].loop_start).wrapping_add(llen);
    let sdat = instruments[chan.instrument as usize].sample_data;
    let mut ampl: i16 =
        (if chan.mute == 0 { chan.ampl as i32 } else { 0 }) as i16;
    let lamp: i16 =
        ((ampl as i32 * (127_i32 - chan.panning as i32)) >> 5) as i16;
    let ramp: i16 = ((ampl as i32 * chan.panning as i32) >> 5) as i16;
    while buf_idx < buf_end {
        if sidx >= lep1 {
            if llen <= 16384 {
                sidx = lep1;
                break;
            } else {
                while sidx >= lep1 {
                    sidx = sidx.wrapping_sub(llen);
                }
            }
        }
        epos = sidx.wrapping_add(
            (buf_end.wrapping_sub(buf_idx) >> 1).wrapping_mul(step),
        );
        if lamp as i32 != 0 || ramp as i32 != 0 {
            if epos > lep1 {
                epos = lep1;
            }
            if lamp as i32 != 0 && ramp as i32 != 0 {
                while sidx < epos {
                    ampl = sdat[(sidx >> 14) as usize] as i16;
                    let fresh9 = buf_idx;
                    buf_idx = buf_idx.wrapping_add(1);
                    let fresh10 = &mut (buf[fresh9 as usize]);
                    *fresh10 = (*fresh10 as i32
                        + ((ampl as i32 * lamp as i32) >> 2))
                        as i16;
                    let fresh11 = buf_idx;
                    buf_idx = buf_idx.wrapping_add(1);
                    let fresh12 = &mut (buf[fresh11 as usize]);
                    *fresh12 = (*fresh12 as i32
                        + ((ampl as i32 * ramp as i32) >> 2))
                        as i16;
                    sidx = sidx.wrapping_add(step);
                }
            } else {
                if ramp != 0 {
                    buf_idx = buf_idx.wrapping_add(1);
                }
                while sidx < epos {
                    let fresh13 = &mut (buf[buf_idx as usize]);
                    *fresh13 = (*fresh13 as i32
                        + sdat[(sidx >> 14) as usize] as i32 * ampl as i32)
                        as i16;
                    buf_idx = buf_idx.wrapping_add(2);
                    sidx = sidx.wrapping_add(step);
                }
                buf_idx &= -2_i32 as usize;
            }
        } else {
            buf_idx = buf_end;
            sidx = epos;
        }
    }
    chan.sample_idx = sidx;
}

/// Get a nice version string. I guess.
pub fn version() -> &'static str {
    consts::MICROMOD_VERSION
}

/// An error that can happen when trying to initialize micromod
#[derive(Debug)]
pub enum InitError {
    /// Number of channels is incorrect
    ChannelNumIncorrect,
    /// Sampling rate is incorrect (below 8khz?)
    SamplingRateIncorrect,
}

impl MmC2r<'_> {
    /// Create a new micromod decoder apparatus.
    pub fn new(data: &[u8], sample_rate: isize) -> Result<MmC2r, InitError> {
        let num_channels =
            match calculate_num_channels(bytemuck::cast_slice(data)) {
                Some(num_channels) => num_channels,
                None => return Err(InitError::ChannelNumIncorrect),
            };
        if sample_rate < 8000 {
            return Err(InitError::SamplingRateIncorrect);
        }
        let song_length = (data[950] as i32 & 0x7f_i32) as isize;
        let sequence = &data[952..];
        let pattern_data = &data[1084..];
        let mut state = MmC2r {
            sample_rate,
            channels: Default::default(),
            src: ModSrc {
                instruments: Vec::new(),
                module_data: bytemuck::cast_slice(data),
                pattern_data,
                sequence,
                num_patterns: Default::default(),
                num_channels,
                song_length,
            },
            playback: PlaybackState::default(),
        };
        state.src.num_patterns = calculate_num_patterns(state.src.module_data);
        let mut sample_data_offset =
            1084 + state.src.num_patterns * 64 * state.src.num_channels * 4;
        let mut inst_idx = 1;
        // First instrument is an unused dummy instrument
        state.src.instruments.push(Instrument::dummy());
        while inst_idx < 32 {
            let sample_length = unsigned_short_big_endian(
                state.src.module_data,
                inst_idx * 30 + 12,
            ) * 2;
            let fine_tune =
                (state.src.module_data[(inst_idx * 30 + 14) as usize] as i32
                    & 0xf_i32) as isize;
            let fine_tune = ((fine_tune & 0x7) - (fine_tune & 0x8) + 8) as u8;
            let volume = (state.src.module_data[(inst_idx * 30 + 15) as usize]
                as i32
                & 0x7f_i32) as isize;
            let volume = (if volume > 64 { 64 } else { volume }) as u8;
            let mut loop_start = unsigned_short_big_endian(
                state.src.module_data,
                inst_idx * 30 + 16,
            ) * 2;
            let mut loop_length = unsigned_short_big_endian(
                state.src.module_data,
                inst_idx * 30 + 18,
            ) * 2;
            if loop_start + loop_length > sample_length {
                if loop_start / 2 + loop_length <= sample_length {
                    loop_start /= 2;
                } else {
                    loop_length = sample_length - loop_start;
                }
            }
            if loop_length < 4 {
                loop_start = sample_length;
                loop_length = 0;
            }
            let loop_start = (loop_start << 14) as usize;
            let loop_length = (loop_length << 14) as usize;
            let sample_data = &bytemuck::cast_slice::<u8, i8>(data)
                [sample_data_offset as usize..];
            sample_data_offset += sample_length;
            inst_idx += 1;
            state.src.instruments.push(Instrument {
                volume,
                fine_tune,
                loop_start,
                loop_length,
                sample_data,
            });
        }
        state.playback.c2_rate =
            (if state.src.num_channels > 4 { 8363 } else { 8287 }) as isize;
        state.playback.gain =
            (if state.src.num_channels > 4 { 32 } else { 64 }) as isize;
        state.mute_channel(-1);
        micromod_set_position(0, &mut state);
        Ok(state)
    }
    /// Fill a buffer with delicious samples
    pub fn get_audio(
        &mut self,
        output_buffer: &mut [i16],
        mut count: isize,
    ) -> bool {
        let mut offset;
        let mut remain;
        let mut chan_idx;
        if self.src.num_channels <= 0 {
            return false;
        }
        offset = 0;
        let mut cnt = true;
        while count > 0 {
            remain = self.playback.tick_len - self.playback.tick_offset;
            if remain > count {
                remain = count;
            }
            chan_idx = 0;
            while chan_idx < self.src.num_channels {
                resample(
                    &mut self.channels[chan_idx as usize],
                    output_buffer,
                    offset,
                    remain,
                    &self.src.instruments,
                );
                chan_idx += 1;
            }
            self.playback.tick_offset += remain;
            if self.playback.tick_offset == self.playback.tick_len {
                if sequence_tick(self) {
                    cnt = false;
                }
                self.playback.tick_offset = 0;
            }
            offset += remain;
            count -= remain;
        }
        cnt
    }
    /// Calculate the length of the module file... In samples. Presumably.
    pub fn calculate_mod_file_len(&self) -> Option<isize> {
        let module_header = self.src.module_data;
        let mut length;

        let mut inst_idx;
        let numchan = calculate_num_channels(module_header)?;
        length =
            1084 + 4 * numchan * 64 * calculate_num_patterns(module_header);
        inst_idx = 1;
        while inst_idx < 32 {
            length +=
                unsigned_short_big_endian(module_header, inst_idx * 30 + 12)
                    * 2;
            inst_idx += 1;
        }
        Some(length)
    }
    /// Calculate the song duration... Okay.
    pub fn calculate_song_duration(&mut self) -> isize {
        let mut duration;
        duration = 0;
        if self.src.num_channels > 0 {
            micromod_set_position(0, self);
            let mut song_end = false;
            while !song_end {
                duration += self.playback.tick_len;
                song_end = sequence_tick(self);
            }
            micromod_set_position(0, self);
        }
        duration
    }
    /// Mute a channel.
    pub fn mute_channel(&mut self, channel: isize) -> isize {
        let mut chan_idx;
        if channel < 0 {
            chan_idx = 0;
            while chan_idx < self.src.num_channels {
                self.channels[chan_idx as usize].mute = 0;
                chan_idx += 1;
            }
        } else if channel < self.src.num_channels {
            self.channels[channel as usize].mute = 1;
        }
        self.src.num_channels
    }
    /// Set some gainz.
    pub fn set_gain(&mut self, value: isize) {
        self.playback.gain = value;
    }
}

fn micromod_set_position(mut pos: isize, state: &mut MmC2r) {
    let mut chan_idx;
    let mut chan;
    if state.src.num_channels <= 0 {
        return;
    }
    if pos >= state.src.song_length {
        pos = 0;
    }
    state.playback.break_pattern = pos;
    state.playback.next_row = 0;
    state.playback.tick = 1;
    state.playback.speed = 6;
    set_tempo(125, &mut state.playback.tick_len, state.sample_rate);
    state.playback.pl_channel = -1;
    state.playback.pl_count = state.playback.pl_channel;
    state.playback.random_seed = 0xabcdef;
    chan_idx = 0;
    while chan_idx < state.src.num_channels {
        chan = &mut state.channels[chan_idx as usize];
        chan.id = chan_idx as u8;
        let fresh15 = &mut chan.assigned;
        *fresh15 = 0;
        chan.instrument = *fresh15;
        chan.volume = 0;
        match chan_idx & 0x3 {
            0 | 3 => {
                chan.panning = 0;
            }
            1 | 2 => {
                chan.panning = 127;
            }
            _ => {}
        }
        chan_idx += 1;
    }
    sequence_tick(state);
    state.playback.tick_offset = 0;
}
