use crate::bit_reader::BitReader;
use std::{
    error::Error,
    fmt::{Display, Formatter},
    io::{self, Read},
};

pub struct Vm {
    pub stack: Vec<u16>,
    pub heap: [u16; 512],
}

#[derive(Debug)]
pub enum ExecErr {
    UnknownCommand,
    EmptyStack,
    IoError(io::Error),
}

impl From<io::Error> for ExecErr {
    fn from(err: io::Error) -> Self {
        Self::IoError(err)
    }
}

impl Display for ExecErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for ExecErr {}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            heap: [0; 512],
        }
    }

    fn exec_single<'a>(&mut self, from: &mut BitReader<'a, impl Read>) -> Result<(), ExecErr> {
        let code = from.read_bits(5).map_err(ExecErr::from)?;

        match code {
            7 => {
                let value = from.read_bits(16).map_err(ExecErr::from)?;
                from.read_bits(3).map_err(ExecErr::from)?;

                self.stack.push(value as u16);
                Ok(())
            }
            23 => {
                let addr = from.read_bits(10).map_err(ExecErr::from)?;
                from.read_bits(1).map_err(ExecErr::from)?;

                self.stack.push(self.heap[addr as usize]);
                Ok(())
            }
            24 => {
                let addr = from.read_bits(10).map_err(ExecErr::from)?;
                from.read_bits(1).map_err(ExecErr::from)?;

                self.heap[addr as usize] = self.stack.pop().ok_or(ExecErr::EmptyStack)?;
                Ok(())
            }
            17 => {
                from.read_bits(3).map_err(ExecErr::from)?;

                let top = self.stack.pop().ok_or(ExecErr::EmptyStack)?;
                let mut cnt = 0;

                for i in 0..16 {
                    cnt += (top >> i) & 1;
                }

                self.stack.push(cnt);
                Ok(())
            }
            _ => Err(ExecErr::UnknownCommand),
        }
    }

    pub fn exec<T: Read>(&mut self, reader: &mut T) -> Result<(), ExecErr> {
        let mut reader = BitReader::new(reader);

        loop {
            match self.exec_single(&mut reader) {
                Ok(_) => (),
                Err(ExecErr::IoError(err)) => {
                    if err.kind() == io::ErrorKind::UnexpectedEof {
                        break;
                    } else {
                        return Err(ExecErr::IoError(err));
                    }
                }
                e => return e,
            }
        }

        Ok(())
    }
}
