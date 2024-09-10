use directory::Directory;
use history::{dump_history, load_history};
use iced::{Application, Font, Settings};
use session::Session;
use std::{cell::RefCell, env, rc::Rc};
use terminal::Terminal;

mod directory;
mod history;
mod session;
mod terminal;

fn main() -> iced::Result {
    let archive_path = env::args().nth(1).expect("no archive given");
    let history_path = env::args().nth(2).expect("no history given");

    let root = Directory::load(&archive_path).expect("unable to load archive");
    let session = Rc::new(RefCell::new(Session::new(
        root,
        load_history(&history_path).expect("unable to load history"),
    )));

    Terminal::run(Settings {
        default_font: Font::MONOSPACE,
        ..Settings::with_flags(session.clone())
    })?;

    dump_history(&history_path, &session.borrow().get_history()).expect("unable to save history");
    Ok(())
}
