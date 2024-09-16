use directory::Directory;
use history::{dump_history, load_history};
use iced::{Application, Font, Settings};
use session::Session;
use std::{cell::RefCell, env, fs::OpenOptions, io::Read, rc::Rc};
use terminal::Terminal;

mod directory;
mod history;
mod session;
mod terminal;

fn main() -> iced::Result {
    let archive_path = env::args().nth(1).expect("no archive given");
    let history_path = env::args().nth(2).expect("no history given");
    let script_path = env::args().nth(3).expect("no script given");

    let root = Directory::load(&archive_path).expect("unable to load archive");
    let session = Rc::new(RefCell::new(Session::new(
        root,
        load_history(&history_path).expect("unable to load history"),
    )));

    let mut script = String::new();
    OpenOptions::new()
        .read(true)
        .open(script_path)
        .unwrap()
        .read_to_string(&mut script)
        .unwrap();
    let res = session.borrow_mut().exec_script(&script.trim());

    Terminal::run(Settings {
        default_font: Font::MONOSPACE,
        ..Settings::with_flags((res, session.clone()))
    })?;

    dump_history(&history_path, &session.borrow().get_history()).expect("unable to save history");
    Ok(())
}
