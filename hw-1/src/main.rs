use directory::Directory;
use iced::{Application, Font, Settings};
use session::Session;
use std::env;
use terminal::Terminal;

mod directory;
mod session;
mod terminal;

fn main() -> iced::Result {
    let archive_path = env::args().nth(1).expect("no archive given");
    let root = Directory::load(&archive_path).unwrap();
    let session = Session::new(root);

    Terminal::run(Settings {
        default_font: Font::MONOSPACE,
        ..Settings::with_flags(session)
    })
}
