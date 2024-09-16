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

    pub fn exec_script(&mut self, script: &str) -> String {
        script
            .split('\n')
            .map(|cmd| match self.exec(cmd) {
                Ok(res) => res,
                Err(err) => format!("{:?}", err),
            })
            .collect::<Vec<_>>()
            .join("\n")
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
        let mut names = cwd
            .files
            .keys()
            .map(ToString::to_string)
            .chain(cwd.directories.keys().map(|key| key.to_string() + "/"))
            .collect::<Vec<_>>();

        names.sort();
        names.join("\n")
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::directory::File;
    use std::collections::HashMap;

    #[test]
    fn exec_non_existent() {
        let mut session = Session::new(Directory::default(), Vec::new());

        assert!(matches!(
            session.exec("this-command-should-not-exist").unwrap_err(),
            CommandExecutionError::CommandNotFound
        ));
    }

    #[test]
    fn get_history() {
        let command = "this-command-should-be-in-history";
        let mut session = Session::new(Directory::default(), Vec::new());

        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        let _ = session.exec(command);

        assert!(session.get_history()[0].0 == now);
        assert!(session.get_history()[0].1 == command);
    }

    #[test]
    fn get_cwd() {
        let cwd = vec!["this", "is", "path"];
        let mut directory = Directory::default();
        directory.get_or_create_path(&cwd);

        let mut session = Session::new(directory, Vec::new());
        session.cwd = cwd.iter().map(|str| String::from(*str)).collect();

        // This should not panic
        let _ = session.get_cwd();
    }

    #[test]
    fn ls_no_dirs() {
        let dir = Directory {
            files: HashMap::from([
                (String::from("file1"), File { size: 0 }),
                (String::from("file2"), File { size: 0 }),
            ]),
            directories: HashMap::default(),
        };

        let session = Session::new(dir, Vec::new());
        assert!(session.exec_ls() == "file1\nfile2");
    }

    #[test]
    fn ls_with_dirs() {
        let dir = Directory {
            files: HashMap::from([
                (String::from("file1"), File { size: 0 }),
                (String::from("file2"), File { size: 0 }),
            ]),
            directories: HashMap::from([(String::from("thisisdir"), Directory::default())]),
        };

        let session = Session::new(dir, Vec::new());
        assert!(session.exec_ls() == "file1\nfile2\nthisisdir/");
    }

    #[test]
    fn cd_no_dir() {
        let dir = Directory::default();
        let mut session = Session::new(dir, Vec::new());

        assert!(matches!(
            session.exec_cd("/this/does/not/exist"),
            Err(CommandExecutionError::DirectoryNotFound)
        ));
    }

    #[test]
    fn cd_with_dir() {
        let dir = Directory {
            files: HashMap::new(),
            directories: HashMap::from([(String::from("nested"), Directory::default())]),
        };
        let mut session = Session::new(dir, Vec::new());

        assert!(matches!(session.exec_cd("nested"), Ok(())));
        assert!(session.cwd[0] == "nested");
    }

    #[test]
    fn history_less_500() {
        let session = Session::new(
            Directory::default(),
            vec![(0, String::from("command1")), (0, String::from("command2"))],
        );

        assert!(session.exec_history() == "0 command1\n1 command2");
    }

    #[test]
    fn history_more_500() {
        let session = Session::new(
            Directory::default(),
            (0..600)
                .into_iter()
                .map(|index| (0, index.to_string()))
                .collect::<Vec<_>>(),
        );

        let expected = (0..500)
            .into_iter()
            .map(|index| format!("{0} {1}", index, index + 100))
            .collect::<Vec<_>>()
            .join("\n");

        assert!(session.exec_history() == expected);
    }
}
