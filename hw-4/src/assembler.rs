use crate::bit_writer::BitWriter;
use std::io::{self, Write};

#[derive(Debug)]
pub enum Command {
    Push(u16),
    Read(u16),
    Write(u16),
    Pop,
}

fn parse_single(line: &str) -> Option<Command> {
    match line.split(char::is_whitespace).collect::<Vec<&str>>()[..] {
        ["PUSH", val] => Some(Command::Push(val.parse::<u16>().ok()?)),
        ["READ", addr] => Some(Command::Read(addr.parse::<u16>().ok()?)),
        ["WRITE", addr] => Some(Command::Write(addr.parse::<u16>().ok()?)),
        ["POP"] => Some(Command::Pop),
        _ => None,
    }
}

pub fn parse(text: &str) -> Option<Vec<Command>> {
    let mut commands = vec![];

    for line in text.lines() {
        if line.is_empty() {
            continue;
        }

        match parse_single(line) {
            Some(cmd) => commands.push(cmd),
            None => return None,
        }
    }

    Some(commands)
}

fn write_command<'a, T: Write>(writer: &mut BitWriter<'a, T>, command: &Command) -> io::Result<()> {
    match command {
        Command::Push(value) => {
            writer.write_bits(7, 5)?;
            writer.write_bits(*value as u64, 16)?;
            writer.write_bits(0, 3)?;
        }
        Command::Read(addr) => {
            writer.write_bits(23, 5)?;
            writer.write_bits(*addr as u64, 10)?;
            writer.write_bits(0, 1)?;
        }
        Command::Write(addr) => {
            writer.write_bits(24, 5)?;
            writer.write_bits(*addr as u64, 10)?;
            writer.write_bits(0, 1)?;
        }
        Command::Pop => {
            writer.write_bits(17, 5)?;
            writer.write_bits(0, 3)?;
        }
    };

    writer.flush()?;
    Ok(())
}

pub fn write<T: Write>(writer: &mut T, commands: &[Command]) -> io::Result<Vec<(String, Vec<u8>)>> {
    let mut writer = BitWriter::new(writer);
    let mut log = Vec::new();

    for cmd in commands {
        write_command(&mut writer, cmd)?;
        log.push((format!("{:?}", cmd), writer.pop_byte_log()));
    }

    Ok(log)
}
