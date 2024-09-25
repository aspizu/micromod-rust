use core::panic;
use std::io::{BufWriter, Write};

const MAX_CHANNELS: u32 = 16;
const FP_SHIFT: u32 = 14;
const FP_ONE: u32 = 16384;
const FP_MASK: u32 = 16383;
const MICROMOD_VERSION: &'static str =
    "Micromod Protrackerr replay 20180625 (c)mumart@gmail.com";

#[derive(Default)]
struct Note {
    key: u16,
    instrument: u8,
    effect: u8,
    param: u8,
}

struct Instrument<'a> {
    volume: u8,
    fine_tune: u8,
    loop_start: u32,
    loop_length: u32,
    sample_data: &'a [u8],
}

#[derive(Default)]
struct Channel {
    note: Note,
    period: u16,
    porta_period: u16,
    sample_offset: u32,
    sample_idx: u32,
    step: u32,
    volume: u8,
    panning: u8,
    fine_tune: u8,
    ampl: u8,
    mute: bool,
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

const fine_tuning: &'static [u16] = &[
    4340, 4308, 4277, 4247, 4216, 4186, 4156, 4126, 4096, 4067, 4037, 4008,
    3979, 3951, 3922, 3894,
];

const arp_tuning: &'static [u16] = &[
    4096, 3866, 3649, 3444, 3251, 3069, 2896, 2734, 2580, 2435, 2299, 2170,
    2048, 1933, 1825, 1722,
];

const sine_table: &'static [u8] = &[
    0, 24, 49, 74, 97, 120, 141, 161, 180, 197, 212, 224, 235, 244, 250, 253,
    255, 253, 250, 244, 235, 224, 212, 197, 180, 161, 141, 120, 97, 74, 49, 24,
];

fn calculate_num_patterns(module_header: &[u8]) -> u8 {
    let mut num_patterns = 0;
    for pattern in 0..128 {
        let order_entry = module_header[952 + pattern] & 0x7F;
        if order_entry >= num_patterns {
            num_patterns = order_entry + 1;
        }
    }
    num_patterns
}

fn calculate_num_channels(module_header: &[u8]) -> u8 {
    match &module_header[1082..1084] {
        b"K." | b"K!" | b"T." | b"T4" => 4,
        b"HN" => module_header[1080] - 48,
        b"CH" => (module_header[1080] - 48) * 10 + module_header[1081] - 48,
        _ => 0,
    }
    .clamp(0, MAX_CHANNELS as u8)
}

struct State<'a> {
    module_data: &'a [u8],
    pattern_data: &'a [u8],
    sequence: &'a [u8],
    song_length: u8,
    restart: u8,
    num_patterns: u8,
    num_channels: u8,
    instruments: Vec<Instrument<'a>>,
    sample_rate: u32,
    gain: u32,
    c2_rate: u32,
    tick_len: u32,
    tick_offset: u32,
    pattern: u8,
    break_pattern: i32,
    row: u8,
    next_row: i32,
    tick: i32,
    speed: u32,
    pl_count: i32,
    pl_channel: i32,
    random_seed: u32,
}

impl<'a> State<'a> {
    fn new(
        data: &'a [u8],
        sampling_rate: u32,
    ) -> Option<(State<'a>, Vec<Channel>)> {
        if sampling_rate < 8000 {
            panic!("Sampling rate too low");
        }
        let num_channels = calculate_num_channels(data);
        if num_channels <= 0 {
            return None;
        }
        let mut channels =
            (0..num_channels).map(|_| Channel::default()).collect();
        let num_patterns = calculate_num_patterns(data);
        let mut sample_data_offset =
            1084 + num_patterns as usize * 64 * num_channels as usize * 4;
        let mut instruments = vec![];
        for i in 1..32 {
            let i = i * 30;
            let sample_length = 2 * u16::from_be_bytes(
                data[i + 12..i + 14].try_into().unwrap(),
            ) as usize;
            let fine_tune = data[i + 14] & 0x7F;
            let fine_tune = (fine_tune & 0x7) - (fine_tune & 0x8) + 8;
            let volume = data[i + 15] & 0x7F;
            let volume = volume.min(64);
            let mut loop_start = 2 * u16::from_be_bytes(
                data[i + 16..i + 18].try_into().unwrap(),
            ) as usize;
            let mut loop_length = 2 * u16::from_be_bytes(
                data[i + 18..i + 20].try_into().unwrap(),
            ) as usize;
            if loop_start + loop_length > sample_length {
                if loop_start / 2 + loop_length <= sample_length {
                    loop_start = loop_start / 2;
                } else {
                    loop_length = sample_length - loop_start;
                }
            }
            if loop_length < 4 {
                loop_start = sample_length;
                loop_length = 0;
            }
            let loop_start = (loop_start << FP_SHIFT) as u32;
            let loop_length = (loop_length << FP_SHIFT) as u32;
            instruments.push(Instrument {
                volume,
                fine_tune,
                loop_start,
                loop_length,
                sample_data: &data
                    [sample_data_offset..sample_data_offset + sample_length],
            });
            sample_data_offset += sample_length;
        }
        let num_patterns = calculate_num_patterns(data);
        let mut state = State {
            module_data: data,
            pattern_data: &data[1084..],
            sequence: &data[952..],
            song_length: data[950] & 0x7F, // change to u32 if broken
            restart: data[951] & 0x7F,     // change to u32 if broken
            num_patterns,
            num_channels,
            instruments,
            sample_rate: sampling_rate,
            c2_rate: if num_channels > 4 { 8363 } else { 8287 },
            gain: if num_channels > 4 { 32 } else { 64 },
            tick_len: 0,
            tick_offset: 0,
            pattern: 0,
            break_pattern: 0,
            row: 0,
            next_row: 0,
            tick: 0,
            speed: 0,
            pl_count: 0,
            pl_channel: 0,
            random_seed: 0,
        };
        state.mute_channel(&mut channels, -1);
        state.set_position(&mut channels, 0);
        Some((state, channels))
    }

    fn mute_channel(&mut self, channels: &mut Vec<Channel>, channel: i32) {
        if channel < 0 {
            for chan in channels.iter_mut() {
                chan.mute = false;
            }
        } else if channel < self.num_channels as i32 {
            channels[channel as usize].mute = true;
        }
    }

    fn set_tempo(&mut self, tempo: u8) {
        self.tick_len =
            ((self.sample_rate << 1) + (self.sample_rate >> 1)) / tempo as u32;
    }

    fn update_frequency(&mut self, chan: &mut Channel) {
        let period = chan.period as i32 + chan.vibrato_add as i32;
        let period =
            period * arp_tuning[chan.arpeggio_add as usize] as i32 >> 11;
        let period = (period >> 1) + (period & 1);
        let period = if period < 14 { 6848 } else { period };
        let freq = self.c2_rate * 428 / period as u32;
        chan.step = (freq << FP_SHIFT) / self.sample_rate;
        let volume = chan.volume as i32 + chan.tremolo_add as i32;
        let volume = volume.clamp(0, 64);
        chan.ampl = ((volume * self.gain as i32) >> 5) as u8;
    }

    fn tone_portamento(&mut self, chan: &mut Channel) {
        let mut source = chan.period;
        let dest = chan.porta_period;
        if source < dest {
            source += chan.porta_speed as u16;
            if source > dest {
                source = dest;
            }
        } else if source > dest {
            source = source.wrapping_sub(chan.porta_speed as u16);
            if source < dest {
                source = dest;
            }
        }
        chan.period = source;
    }

    fn volume_slide(&mut self, chan: &mut Channel, param: u8) {
        let volume =
            chan.volume as i32 + (param as i32 >> 4) - (param as i32 & 0xF);
        chan.volume = volume.clamp(0, 64) as u8;
    }

    fn waveform(&mut self, phase: u8, type_: u8) -> i32 {
        match type_ & 0x3 {
            0 => {
                let amplitude = sine_table[(phase & 0x1F) as usize] as i32;
                if (phase & 0x20) > 0 {
                    -amplitude
                } else {
                    amplitude
                }
            }
            1 => 255 - (((phase + 0x20) & 0x3F) << 3) as i32,
            2 => (255 - (phase & 0x20) << 4) as i32,
            3 => {
                let amplitude = (self.random_seed >> 20) as i32 - 255;
                self.random_seed = (self.random_seed * 65 + 17) & 0x1FFFFFFF;
                amplitude
            }
            _ => unreachable!(),
        }
    }

    fn vibrato(&mut self, chan: &mut Channel) {
        chan.vibrato_add = (self
            .waveform(chan.vibrato_phase, chan.vibrato_type)
            * chan.vibrato_depth as i32
            >> 7) as i8;
    }

    fn tremolo(&mut self, chan: &mut Channel) {
        chan.tremolo_add = (self
            .waveform(chan.tremolo_phase, chan.tremolo_type)
            * chan.tremolo_depth as i32
            >> 6) as i8;
    }

    fn trigger(&mut self, channel: &mut Channel) {
        let ins = channel.note.instrument;
        if (1..31).contains(&ins) {
            channel.assigned = ins;
            channel.sample_offset = 0;
            channel.fine_tune = self.instruments[ins as usize].fine_tune;
            channel.volume = self.instruments[ins as usize].volume;
            if self.instruments[ins as usize].loop_length > 0
                && channel.instrument > 0
            {
                channel.instrument = ins;
            }
        }
        if channel.note.effect == 0x09 {
            channel.sample_offset = ((channel.note.param & 0xFF) as u32) << 8;
        } else if channel.note.effect == 0x15 {
            channel.fine_tune = channel.note.param;
        }
        if channel.note.key > 0 {
            let period = (channel.note.key as u32
                * fine_tuning[(channel.fine_tune & 0xF) as usize] as u32)
                >> 11;
            channel.porta_period = ((period >> 1) + (period & 1)) as u16;
            if channel.note.effect != 0x3 && channel.note.effect != 0x5 {
                channel.instrument = channel.assigned;
                channel.period = channel.porta_period;
                channel.sample_idx = channel.sample_offset << FP_SHIFT;
                if channel.vibrato_type < 4 {
                    channel.vibrato_phase = 0;
                }
                if channel.tremolo_type < 4 {
                    channel.tremolo_phase = 0;
                }
            }
        }
    }

    fn channel_row(&mut self, chan: &mut Channel) {
        let effect = chan.note.effect;
        let param = chan.note.param;
        chan.vibrato_add = 0;
        chan.tremolo_add = 0;
        chan.arpeggio_add = 0;
        chan.fx_count = 0;
        if !(effect == 0x1D && param > 0) {
            self.trigger(chan);
        }
        match effect {
            0x3 => {
                if param > 0 {
                    chan.porta_speed = param;
                }
            }
            0x4 => {
                if (param & 0xF0) > 0 {
                    chan.vibrato_speed = param >> 4;
                }
                if (param & 0x0F) > 0 {
                    chan.vibrato_depth = param & 0xF;
                }
                self.vibrato(chan);
            }
            0x6 => {
                self.vibrato(chan);
            }
            0x7 => {
                if (param & 0xF0) > 0 {
                    chan.tremolo_speed = param >> 4;
                }
                if (param & 0x0F) > 0 {
                    chan.tremolo_depth = param & 0xF;
                }
                self.tremolo(chan);
            }
            0x8 => {
                if self.num_channels != 4 {
                    chan.panning = param.clamp(127, 255);
                }
            }
            0xB => {
                if self.pl_count < 0 {
                    self.break_pattern = param as i32;
                    self.next_row = 0;
                }
            }
            0xC => {
                chan.volume = param.clamp(0, 64);
            }
            0xD => {
                if self.pl_count < 0 {
                    if self.break_pattern < 0 {
                        self.break_pattern = (self.pattern + 1) as i32;
                    }
                    self.next_row = ((param >> 4) * 10 + (param & 0xF)) as i32;
                    if self.next_row >= 64 {
                        self.next_row = 0;
                    }
                }
            }
            0xF => {
                if param > 0 {
                    if param < 32 {
                        self.tick = param as i32;
                        self.speed = param as u32;
                    } else {
                        self.set_tempo(param);
                    }
                }
            }
            0x11 => {
                chan.period = chan.period.saturating_sub(param as u16);
            }
            0x12 => {
                chan.period = chan.period.saturating_add(param as u16);
            }
            0x14 => {
                if param < 8 {
                    chan.vibrato_type = param;
                }
            }
            0x16 => {
                if param == 0 {
                    chan.pl_row = self.row;
                }
                if chan.pl_row < self.row && self.break_pattern < 0 {
                    if self.pl_count < 0 {
                        self.pl_count = param as i32;
                        self.pl_channel = chan.id as i32;
                    }
                    if self.pl_channel == chan.id as i32 {
                        if self.pl_count == 0 {
                            chan.pl_row = self.row + 1;
                        } else {
                            self.next_row = chan.pl_row as i32;
                        }
                        self.pl_count -= 1;
                    }
                }
            }
            0x17 => {
                if param < 8 {
                    chan.tremolo_type = param;
                }
            }
            0x1A => {
                chan.volume = (chan.volume + param).clamp(0, 64);
            }
            0x1B => {
                chan.volume = (chan.volume - param).clamp(0, 64);
            }
            0x1C => {
                if param <= 0 {
                    chan.volume = 0;
                }
            }
            0x1E => {
                self.tick = (self.speed + self.speed * param as u32) as i32;
            }
            _ => {}
        }
        self.update_frequency(chan);
    }

    fn channel_tick(&mut self, chan: &mut Channel) {
        let effect = chan.note.effect;
        let param = chan.note.param;
        chan.fx_count += 1;
        match effect {
            0x1 => {
                chan.period = chan.period.saturating_sub(param as u16);
            }
            0x2 => {
                chan.period = chan.period.saturating_add(param as u16);
            }
            0x3 => {
                self.tone_portamento(chan);
            }
            0x4 => {
                chan.vibrato_phase =
                    chan.vibrato_phase.wrapping_add(chan.vibrato_speed);
            }
            0x5 => {
                self.tone_portamento(chan);
                self.volume_slide(chan, param);
            }
            0x6 => {
                chan.vibrato_phase += chan.vibrato_speed;
                self.vibrato(chan);
                self.volume_slide(chan, param);
            }
            0x7 => {
                chan.tremolo_phase += chan.tremolo_speed;
                self.tremolo(chan);
            }
            0xA => {
                self.volume_slide(chan, param);
            }
            0xE => {
                chan.arpeggio_add = match chan.fx_count {
                    0 => 0,
                    1 => param >> 4,
                    2 => param & 0xF,
                    _ => {
                        chan.fx_count = 0;
                        0
                    }
                } as i8;
            }
            0x19 => {
                if chan.fx_count >= param {
                    chan.fx_count = 0;
                    chan.sample_idx = 0;
                }
            }
            0x1C => {
                if param == chan.fx_count {
                    chan.volume = 0;
                }
            }
            0x1D => {
                if param == chan.fx_count {
                    self.trigger(chan);
                }
            }
            _ => {}
        }
        if effect > 0 {
            self.update_frequency(chan);
        }
    }

    fn sequence_row(&mut self, channels: &mut Vec<Channel>) -> i32 {
        let mut song_end = 0;
        if self.next_row < 0 {
            self.break_pattern = (self.pattern + 1) as i32;
            self.next_row = 0;
        }
        if self.break_pattern >= 0 {
            if self.break_pattern >= self.song_length as i32 {
                self.break_pattern = 0;
                self.next_row = 0;
            }
            if self.break_pattern <= self.pattern as i32 {
                song_end = 1;
            }
            self.pattern = self.break_pattern as u8;
            for chan_idx in 0..self.num_channels {
                let chan = &mut channels[chan_idx as usize];
                chan.pl_row = 0;
            }
            self.break_pattern = -1;
        }
        self.row = self.next_row as u8;
        self.next_row = self.row as i32 + 1;
        if self.next_row >= 64 {
            self.next_row = -1;
        }
        let mut pat_offset = (self.sequence[self.pattern as usize] as usize
            * 64
            + self.row as usize)
            * self.num_channels as usize
            * 4;
        for chan_idx in 0..self.num_channels {
            let chan = &mut channels[chan_idx as usize];
            chan.note.key = (self.pattern_data[pat_offset] as u16 & 0xF) << 8;
            chan.note.key |= self.pattern_data[pat_offset + 1] as u16;
            chan.note.instrument = self.pattern_data[pat_offset + 2] >> 4;
            chan.note.instrument |= self.pattern_data[pat_offset] & 0x10;
            let mut effect = self.pattern_data[pat_offset + 2] & 0xF;
            let mut param = self.pattern_data[pat_offset + 3];
            pat_offset += 4;
            if effect == 0xE {
                effect = 0x10 | (param >> 4);
                param &= 0xF;
            }
            if effect == 0 && param > 0 {
                effect = 0xE;
            }
            chan.note.effect = effect;
            chan.note.param = param;
            self.channel_row(chan);
        }
        song_end
    }

    fn sequence_tick(&mut self, channels: &mut Vec<Channel>) -> i32 {
        let mut song_end = 0;
        self.tick -= 1;
        if self.tick <= 0 {
            self.tick = self.speed as i32;
            song_end = self.sequence_row(channels);
        } else {
            for i in 0..self.num_channels as usize {
                self.channel_tick(&mut channels[i])
            }
        }
        song_end
    }

    fn resample(
        &mut self,
        chan: &mut Channel,
        buf: &mut [i16],
        offset: usize,
        count: usize,
    ) {
        let mut epos: usize;
        let mut buf_idx = offset << 1;
        let buf_end = (offset + count) << 1;
        let mut sidx = chan.sample_idx;
        let step = chan.step;
        let llen = self.instruments[chan.instrument as usize].loop_length;
        let lep1 = self.instruments[chan.instrument as usize].loop_start + llen;
        let sdat = self.instruments[chan.instrument as usize].sample_data;
        let mut ampl = if chan.mute { 0 } else { chan.ampl };
        let lamp = ampl as usize * (127 - chan.panning as usize) >> 5;
        let ramp = ampl as usize * chan.panning as usize >> 5;
        while buf_idx < buf_end {
            if sidx >= lep1 as u32 {
                if llen <= FP_ONE {
                    sidx = lep1 as u32;
                    break;
                }
                while sidx >= lep1 as u32 {
                    sidx -= llen as u32;
                }
            }
            epos = sidx as usize
                + ((buf_end as usize - buf_idx as usize) >> 1) * step as usize;
            if lamp != 0 || ramp != 0 {
                if epos > lep1 as usize {
                    epos = lep1 as usize;
                }
                if lamp != 0 && ramp != 0 {
                    while sidx < epos as u32 {
                        ampl = sdat[sidx as usize >> FP_SHIFT];
                        buf[buf_idx] += ampl as i16 * lamp as i16 >> 2;
                        buf_idx += 1;
                        buf[buf_idx] += ampl as i16 * ramp as i16 >> 2;
                        buf_idx += 1;
                        sidx += step;
                    }
                } else {
                    if ramp != 0 {
                        buf_idx += 1;
                    }
                    while sidx < epos as u32 {
                        let sample = buf[buf_idx] = buf[buf_idx]
                            .saturating_add(
                                (sdat[sidx as usize >> FP_SHIFT] as i32 + 255)
                                    .saturating_mul(ampl as i32)
                                    as i16,
                            );
                        buf_idx += 2;
                        sidx += step;
                    }
                    buf_idx &= usize::MAX ^ 0b1;
                }
            } else {
                buf_idx = buf_end;
                sidx = epos as u32;
            }
        }
        chan.sample_idx = sidx;
    }

    fn set_position(&mut self, channels: &mut Vec<Channel>, mut pos: u8) {
        if self.num_channels <= 0 {
            return;
        }
        if pos >= self.song_length {
            pos = 0;
        }
        self.break_pattern = pos as i32;
        self.next_row = 0;
        self.tick = 1;
        self.speed = 6;
        self.set_tempo(125);
        self.pl_count = -1;
        self.pl_channel = -1;
        self.random_seed = 0xABCDEF;
        for i in 0..self.num_channels as usize {
            let chan = &mut channels[i];
            chan.id = i as u8;
            chan.instrument = 0;
            chan.assigned = 0;
            chan.volume = 0;
            match i & 0x3 {
                0 | 3 => {
                    chan.panning = 0;
                }
                1 | 2 => {
                    chan.panning = 127;
                }
                _ => unreachable!(),
            }
        }
        self.sequence_tick(channels);
        self.tick_offset = 0;
    }

    fn get_audio(
        &mut self,
        channels: &mut Vec<Channel>,
        output_buffer: &mut [i16],
    ) {
        output_buffer.fill(0);
        if self.num_channels <= 0 {
            return;
        }
        let mut offset = 0;
        let mut count = output_buffer.len() / 2;
        while count > 0 {
            let mut remain = self.tick_len - self.tick_offset;
            if remain > count as u32 {
                remain = count as u32;
            }
            for chan_idx in 0..self.num_channels as usize {
                let chan = &mut channels[chan_idx];
                self.resample(chan, output_buffer, offset, remain as usize);
            }
            self.tick_offset += remain;
            if self.tick_offset == self.tick_len {
                self.sequence_tick(channels);
                self.tick_offset = 0;
            }
            offset += remain as usize;
            count -= remain as usize;
        }
    }
}

fn main() {
    let module =
        std::fs::read("C:/Users/aspizu/Music/Mods/mod/back_again.mod").unwrap();
    let (mut state, mut channels) = State::new(&module, 44100).unwrap();
    println!("num_channels: {}", state.num_channels);
    println!("song_length: {}", state.song_length);
    println!("instruments: {}", state.instruments.len());
    for (i, instrument) in state.instruments.iter().enumerate() {
        println!(
            "{} length: {}, volume: {}, fine_tune: {}, loop_start: {}, loop_length: {}",
            i,
            instrument.sample_data.len(),
            instrument.volume,
            instrument.fine_tune,
            instrument.loop_start,
            instrument.loop_length
        );
    }
    let output_file = std::fs::File::create("output.pcm").unwrap();
    let mut writer = BufWriter::new(output_file);
    state.set_position(&mut channels, 0);
    for _ in 0..100 {
        let mut buffer = [0; 44100 * 2];
        state.get_audio(&mut channels, &mut buffer);
        for sample in buffer {
            writer.write_all(sample.to_le_bytes().as_slice()).unwrap();
        }
    }
}
