use crate::bit_writer::BitWriter;
use std::io::{self, Write};

pub enum Command {
    Push(u16),
    Read(u16),
    Write(u16),
    Pop,
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

pub fn write<T: Write>(writer: &mut T, commands: &[Command]) -> io::Result<()> {
    let mut writer = BitWriter::new(writer);

    for cmd in commands {
        write_command(&mut writer, cmd)?;
    }

    Ok(())
}
