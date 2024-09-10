use csv::Writer;
use directory::Directory;
use iced::{Application, Font, Settings};
use session::Session;
use std::{cell::RefCell, env, fs::OpenOptions, io::BufWriter, rc::Rc};
use terminal::Terminal;

mod directory;
mod session;
mod terminal;

fn main() -> iced::Result {
    let archive_path = env::args().nth(1).expect("no archive given");
    let history_path = env::args().nth(2).expect("no history given");

    let root = Directory::load(&archive_path).unwrap();
    let session = Rc::new(RefCell::new(Session::new(root)));

    Terminal::run(Settings {
        default_font: Font::MONOSPACE,
        ..Settings::with_flags(session.clone())
    })?;

    let mut file = OpenOptions::new()
        .append(true)
        .create(true)
        .open(history_path)
        .expect("unable to open history file");
    let mut writer = BufWriter::new(&mut file);
    let mut csv_writer = Writer::from_writer(&mut writer);

    for record in session.borrow().get_history() {
        csv_writer
            .write_record(&[record.0.to_string(), record.1])
            .unwrap();
    }

    csv_writer.flush().expect("unable to write history");
    Ok(())
}
