use std::io::{self, Write};

pub struct BitWriter<'a, T: Write> {
    stream: &'a mut T,
    byte: u8,
    index: u8,
}

impl<'a, T: Write> BitWriter<'a, T> {
    pub fn new(stream: &'a mut T) -> Self {
        Self {
            stream,
            byte: 0,
            index: 0,
        }
    }

    pub fn write_bit(&mut self, bit: bool) -> io::Result<()> {
        if bit {
            self.byte |= 1 << self.index;
        }

        self.index += 1;

        if self.index >= 8 {
            self.stream.write(&[self.byte])?;
            self.byte = 0;
            self.index %= 8;
        }

        Ok(())
    }

    pub fn write_bits(&mut self, bits: u64, count: u8) -> io::Result<()> {
        for shift in 0..count {
            self.write_bit(((bits >> shift) & 1) == 1)?;
        }

        Ok(())
    }

    pub fn flush(&mut self) -> io::Result<()> {
        if self.index == 0 {
            return Ok(());
        }

        self.stream.write(&[self.byte])?;
        self.byte = 0;
        self.index = 0;
        Ok(())
    }
}
