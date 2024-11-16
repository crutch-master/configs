use std::io::{self, Write};

pub struct BitWriter<'a, T: Write> {
    stream: &'a mut T,
    byte: u8,
    index: u8,
    byte_log: Vec<u8>,
}

impl<'a, T: Write> BitWriter<'a, T> {
    pub fn new(stream: &'a mut T) -> Self {
        Self {
            stream,
            byte: 0,
            index: 0,
            byte_log: Vec::new(),
        }
    }

    pub fn write_bit(&mut self, bit: bool) -> io::Result<()> {
        if bit {
            self.byte |= 1 << self.index;
        }

        self.index += 1;

        if self.index >= 8 {
            self.byte_log.push(self.byte);
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

        self.byte_log.push(self.byte);
        self.stream.write(&[self.byte])?;
        self.byte = 0;
        self.index = 0;
        Ok(())
    }

    pub fn pop_byte_log(&mut self) -> Vec<u8> {
        let clone = self.byte_log.clone();
        self.byte_log.clear();
        clone
    }
}
