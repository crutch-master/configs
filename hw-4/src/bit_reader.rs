use std::io::{self, Read};

pub struct BitReader<'a, T: Read> {
    stream: &'a mut T,
    byte: u8,
    index: u8,
}

impl<'a, T: Read> BitReader<'a, T> {
    pub fn new(stream: &'a mut T) -> Self {
        Self {
            stream,
            byte: 0,
            index: 8,
        }
    }

    pub fn read_bit(&mut self) -> io::Result<u8> {
        if self.index >= 8 {
            let mut buff: [u8; 1] = [0];
            self.stream.read_exact(&mut buff)?;
            self.byte = buff[0];
            self.index %= 8;
        }

        let result = (self.byte >> self.index) & 1;
        self.index += 1;
        Ok(result)
    }

    pub fn read_bits(&mut self, count: u8) -> io::Result<u64> {
        let mut result: u64 = 0;

        for shift in 0..count {
            result |= (self.read_bit()? as u64) << shift;
        }

        Ok(result)
    }
}
