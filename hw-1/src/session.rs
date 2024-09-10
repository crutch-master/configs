use std::time::SystemTime;

use crate::directory::Directory;

type HistoryRecord = (u64, String);

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
    pub fn new(root: Directory) -> Self {
        Self {
            root,
            cwd: Vec::new(),
            history: Vec::new(),
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
}
