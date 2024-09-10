use crate::{directory::Directory, history::HistoryRecord};
use std::{iter, time::SystemTime};

#[derive(Debug)]
pub struct Session {
    root: Directory,
    cwd: Vec<String>,
    history: Vec<HistoryRecord>,
}

#[derive(Debug)]
pub enum CommandExecutionError {
    CommandNotFound,
    DirectoryNotFound,
}

impl Session {
    pub fn new(root: Directory, history: Vec<HistoryRecord>) -> Self {
        Self {
            root,
            cwd: vec![],
            history,
        }
    }

    pub fn exec(&mut self, command: &str) -> Result<String, CommandExecutionError> {
        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        self.history.push((now, command.into()));

        let (command, argument) = command
            .split_once(char::is_whitespace)
            .unwrap_or((command, ""));

        match command {
            "ls" => Ok(self.exec_ls()),
            "cd" => self.exec_cd(argument).map(|_| self.cwd.join("/") + "/"),
            "history" => Ok(self.exec_history()),
            "du" => Ok(self.exec_du()),
            _ => Err(CommandExecutionError::CommandNotFound),
        }
    }

    pub fn get_history(&self) -> Vec<HistoryRecord> {
        self.history.clone()
    }

    fn get_cwd(&self) -> &Directory {
        self.root
            .get_path(&self.cwd.iter().map(AsRef::as_ref).collect::<Vec<_>>())
            .unwrap()
    }

    fn exec_ls(&self) -> String {
        let cwd = self.get_cwd();

        cwd.files
            .keys()
            .map(ToString::to_string)
            .chain(cwd.directories.keys().map(|key| key.to_string() + "/"))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn exec_cd(&mut self, path: &str) -> Result<(), CommandExecutionError> {
        if path == "/" {
            self.cwd = vec![];
        } else if path == ".." {
            if self.cwd.is_empty() {
                return Err(CommandExecutionError::DirectoryNotFound);
            }

            self.cwd.pop();
        } else {
            let cwd = self.get_cwd();
            let path = path.split("/").collect::<Vec<_>>();

            let _ = cwd
                .get_path(&path)
                .ok_or(CommandExecutionError::DirectoryNotFound)?;

            self.cwd
                .append(&mut path.iter().map(ToString::to_string).collect::<Vec<_>>());
        }

        Ok(())
    }

    fn exec_history(&self) -> String {
        self.history
            .iter()
            .rev()
            .take(500)
            .rev()
            .enumerate()
            .map(|(index, elem)| format!("{0} {1}", index, elem.1))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn exec_du(&self) -> String {
        let cwd = self.get_cwd();

        cwd.directories
            .iter()
            .map(|(name, dir)| (name, dir.get_total_filesize()))
            .map(|(name, size)| format!("{0} {1}/", size, name))
            .chain(iter::once(format!("{} .", cwd.get_total_filesize())))
            .collect::<Vec<_>>()
            .join("\n")
    }
}
