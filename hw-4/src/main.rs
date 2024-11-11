mod assembler;
mod bit_reader;
mod bit_writer;
mod vm;

use crate::{
    assembler::{write, Command},
    vm::Vm,
};
use std::io::{BufReader, BufWriter};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let commands = vec![Command::Push(12), Command::Pop];
    let binary: Vec<u8> = vec![];
    let mut buf_writer = BufWriter::new(binary);
    write(&mut buf_writer, &commands)?;

    let mut buf_reader = BufReader::new(buf_writer.buffer());
    let mut vm = Vm::new();
    dbg!(vm.exec(&mut buf_reader));

    Ok(())
}
