extern crate chrono;
extern crate regex;

use std::{
    collections::HashMap,
    fs::File,
    io::{self, prelude::*, BufReader},
    str::FromStr,
};
use chrono::{
    NaiveDateTime,
    Timelike,
};
use regex::Regex;

type GuardId =  u32;

#[derive(PartialEq,Debug)]
enum Event {
    Begin(GuardId),
    Sleep,
    WakeUp
}

#[derive(PartialEq,Debug)]
struct LogEntry {
    event: Event,
    time: NaiveDateTime,
}

struct Guard {
    slept: u32,
    minutes: HashMap<u32, u32>,
}

impl Guard {
    fn new() -> Self {
        Guard{
            slept: 0,
            minutes: HashMap::<u32, u32>::new(),
        }
    }

    fn log_minute(&mut self, start: u32, end: u32) {
        self.slept += end - start;
        (start..end).for_each(|minute| {
            *self.minutes.entry(minute).or_insert(0) += 1;
        });
    }
}

impl FromStr for LogEntry {
    type Err = regex::Error;

	fn from_str(repr: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"\[([^\]]+)\] (Guard #(\d+) begins shift|(falls asleep)|(wakes up))").unwrap();
        let caps = match re.captures(repr) {
			None => return Err(regex::Error::Syntax(repr.to_string())),
            Some(caps) => caps,
        };
        let time_str = caps.get(1).unwrap().as_str();
        let time = NaiveDateTime::parse_from_str(time_str, "%Y-%m-%d %H:%M").unwrap();
        if caps.get(3).is_some() {
            let id = caps.get(3).unwrap().as_str().parse::<u32>().unwrap();
            return Ok(LogEntry{event: Event::Begin(id), time: time});
        } else if caps.get(4).is_some() {
            return Ok(LogEntry{event: Event::Sleep, time: time});
        } else if caps.get(5).is_some() {
            return Ok(LogEntry{event: Event::WakeUp, time: time});
        }
        Err(regex::Error::Syntax(repr.to_string()))
    }
}

fn build_guard_log(log: &Vec<LogEntry>) -> Option<HashMap<u32, Guard>> {
    let mut guard : u32 = match log.first().unwrap() {
        LogEntry {
            event: Event::Begin(id),
            ..
        } => *id,
        _ => return None
    };
    let mut map = HashMap::<u32, Guard>::new();
    let mut sleepstart = 0;
    for entry in log {
        match entry.event {
            Event::Begin(id) => {
                guard = id;
            },
            Event::Sleep => {
                sleepstart = entry.time.minute();
            },
            Event::WakeUp => {
                map.entry(guard).or_insert(Guard::new())
                    .log_minute(sleepstart, entry.time.minute());
            }
        }
    }
    Some(map)
}

fn part1(log: &Vec<LogEntry>) -> u32 {
    let guards = build_guard_log(log).unwrap();
    let (id, guard) = guards.into_iter().max_by_key(|(_, guard)| guard.slept).unwrap();
    let (minute, _) = guard.minutes.into_iter().max_by_key(|&(_, count)| count).unwrap();
    id * minute
}

fn part2(log: &Vec<LogEntry>) -> u32 {
    let guards = build_guard_log(log).unwrap();
    let (id, (min, _)) = guards
        .into_iter()
        .map(|(id, guard)| {
            (id,
             guard.minutes
                .into_iter()
                .max_by_key(|&(_, count)| count).unwrap())
        })
        .max_by_key(|&(_, (_, count))| count)
        .unwrap();
    id * min
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let mut log : Vec<LogEntry> = reader
        .lines()
        .map(|line| LogEntry::from_str(&line.unwrap()).unwrap())
        .collect();

    log.sort_by_key(|entry| entry.time);

    println!("Part 1: {}", part1(&log));
    println!("Part 2: {}", part2(&log));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::NaiveDate;

    #[test]
    fn log_entry_from_str() {
        let entry = LogEntry{event: Event::Begin(10), time: NaiveDate::from_ymd(1518, 11, 1).and_hms(0, 0, 0)};
        assert_eq!(entry, LogEntry::from_str("[1518-11-01 00:00] Guard #10 begins shift").unwrap());

        let entry = LogEntry{event: Event::Sleep, time: NaiveDate::from_ymd(1518, 11, 3).and_hms(0, 24, 0)};
        assert_eq!(entry, LogEntry::from_str("[1518-11-03 00:24] falls asleep").unwrap());

        let entry = LogEntry{event: Event::WakeUp, time: NaiveDate::from_ymd(1518, 11, 5).and_hms(0, 55, 0)};
        assert_eq!(entry, LogEntry::from_str("[1518-11-05 00:55] wakes up").unwrap());
    }
}
