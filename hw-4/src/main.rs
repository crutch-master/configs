mod assembler;
mod bit_reader;
mod bit_writer;
mod vm;

use crate::{assembler::{parse, write}, vm::Vm};
use serde_json::to_string;
use std::{env::args, fs, io};

fn assemble(
    source_file: &str,
    dest_file: &str,
    log_file: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = String::from_utf8(fs::read(source_file)?)?;
    let commands = parse(&source).ok_or("invalid code")?;

    let file = fs::OpenOptions::new()
        .write(true)
        .create(true)
        .open(dest_file)?;
    let mut buff_writer = io::BufWriter::new(file);
    let log = write(&mut buff_writer, &commands)?;
    fs::write(log_file, to_string(&log)?)?;

    Ok(())
}

fn interpret(binary_file: &str, out_file: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut reader = io::BufReader::new(fs::File::open(binary_file)?);
    let mut vm = Vm::new();
    vm.exec(&mut reader)?;

    let formatted_memory = to_string(vm.heap.as_slice())?;
    fs::write(out_file, formatted_memory)?;

    Ok(())
}

fn main() {
    let action = args()
        .nth(1)
        .expect("expected first argument to be the action");

    match action.as_str() {
        "assemble" => {
            let source_file = args()
                .nth(2)
                .expect("expected second argument to be the source file");
            let dest_file = args()
                .nth(3)
                .expect("expected third argument to be the destination file");
            let log_file = args()
                .nth(4)
                .expect("expected fourth argument to be the logs file");

            assemble(&source_file, &dest_file, &log_file).unwrap()
        }
        "interpret" => {
            let binary_file = args()
                .nth(2)
                .expect("expected second argument to be the binary file");
            let out_file = args()
                .nth(3)
                .expect("expected third argument to be the result file");

            interpret(&binary_file, &out_file).unwrap()
        }
        _ => panic!("first argument should either be 'assemble' or 'interpret'"),
    }
}
