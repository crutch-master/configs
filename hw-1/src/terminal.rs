use crate::session::Session;
use iced::{
    executor,
    theme::Theme,
    widget::{column, scrollable, text, text_input},
    Application, Command, Element, Length,
};

pub struct Terminal {
    session: Session,

    content: String,
    command: String,
    scroll_id: scrollable::Id,
}

#[derive(Debug, Clone)]
pub enum Message {
    CommandUpdated(String),
    CommandSubmitted,
}

impl Application for Terminal {
    type Executor = executor::Default;
    type Message = Message;
    type Theme = Theme;
    type Flags = Session;

    fn title(&self) -> String {
        String::from("Terminal")
    }

    fn theme(&self) -> Self::Theme {
        Theme::Dark
    }

    fn new(session: Session) -> (Self, Command<Message>) {
        (
            Self {
                session,

                command: String::new(),
                content: String::new(),
                scroll_id: scrollable::Id::unique(),
            },
            Command::none(),
        )
    }

    fn update(&mut self, msg: Message) -> Command<Message> {
        match msg {
            Message::CommandUpdated(cmd) => {
                self.command = cmd;
            }
            Message::CommandSubmitted => {
                self.content = format!("{0}\n{1}", self.content, self.command);
                self.command.clear();

                return scrollable::snap_to(
                    self.scroll_id.clone(),
                    scrollable::RelativeOffset::END,
                );
            }
        }

        Command::none()
    }

    fn view(&self) -> Element<Message> {
        column![
            scrollable(text(self.content.clone()))
                .width(Length::Fill)
                .height(Length::Fill)
                .id(self.scroll_id.clone()),
            text_input("Command", &self.command)
                .on_input(Message::CommandUpdated)
                .on_submit(Message::CommandSubmitted)
        ]
        .into()
    }
}
