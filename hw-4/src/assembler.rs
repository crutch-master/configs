use crate::bit_writer::BitWriter;
use std::io::{self, Write};

#[derive(Debug)]
pub enum Command {
    Push(u16),
    Read(u16),
    Write(u16),
    Popcnt,
}

fn parse_single(line: &str) -> Option<Command> {
    match line.split(char::is_whitespace).collect::<Vec<&str>>()[..] {
        ["PUSH", val] => Some(Command::Push(val.parse::<u16>().ok()?)),
        ["READ", addr] => Some(Command::Read(addr.parse::<u16>().ok()?)),
        ["WRITE", addr] => Some(Command::Write(addr.parse::<u16>().ok()?)),
        ["POPCNT"] => Some(Command::Popcnt),
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
        Command::Popcnt => {
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

#[cfg(test)]
mod test {
    use super::*;

    fn cmd_to_hex(command: &Command) -> io::Result<String> {
        let mut buffer = Vec::new();
        let mut writer = BitWriter::new(&mut buffer);
        write_command(&mut writer, command)?;

        Ok(buffer
            .iter()
            .map(|byte| format!("{:02X}", byte))
            .collect::<Vec<_>>()
            .join(""))
    }

    #[test]
    fn push() {
        assert_eq!(cmd_to_hex(&Command::Push(711)).unwrap(), "E75800");
    }

    #[test]
    fn read() {
        assert_eq!(cmd_to_hex(&Command::Read(146)).unwrap(), "5712");
    }

    #[test]
    fn write() {
        assert_eq!(cmd_to_hex(&Command::Write(140)).unwrap(), "9811");
    }

    #[test]
    fn popcnt() {
        assert_eq!(cmd_to_hex(&Command::Popcnt).unwrap(), "11");
    }
}
