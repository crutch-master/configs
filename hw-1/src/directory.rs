use std::{collections::HashMap, error, process::Command};

#[derive(Debug)]
pub struct File {
    pub size: u64,
}

#[derive(Debug, Default)]
pub struct Directory {
    pub files: HashMap<String, File>,
    pub directories: HashMap<String, Directory>,
}

impl Directory {
    pub fn load(archive_path: &str) -> Result<Self, Box<dyn error::Error>> {
        let output = Command::new("tar").arg("-tvf").arg(archive_path).output()?;
        let stdout = String::from_utf8(output.stdout)?;

        let mut root = Self::default();

        for file in stdout.trim().split("\n") {
            let parts = file.split_whitespace().rev().take(4).collect::<Vec<_>>();
            let path = parts.first().unwrap().split("/").collect::<Vec<_>>();

            if path.last().unwrap().is_empty() {
                root.get_or_create_path(&path[..path.len() - 2])
                    .directories
                    .insert(path[path.len() - 2].into(), Self::default());
            } else {
                root.get_or_create_path(&path[..path.len() - 1])
                    .files
                    .insert(
                        path[path.len() - 1].into(),
                        File {
                            size: parts.last().unwrap().parse().unwrap(),
                        },
                    );
            }
        }

        Ok(root)
    }

    pub fn get_path(&self, path: &[&str]) -> Option<&Self> {
        let part = match path.first() {
            Some(part) => *part,
            None => return Some(self),
        };

        self.directories
            .get(part)
            .and_then(|child| child.get_path(&path[1..]))
    }

    pub fn get_total_filesize(&self) -> u64 {
        self.files.values().map(|file| file.size).sum::<u64>()
            + self
                .directories
                .values()
                .map(|dir| dir.get_total_filesize())
                .sum::<u64>()
    }

    pub fn get_or_create_path(&mut self, path: &[&str]) -> &mut Self {
        let part = match path.first() {
            Some(part) => *part,
            None => return self,
        };

        if !self.directories.contains_key(part) {
            self.directories.insert(part.into(), Self::default());
        }

        self.directories
            .get_mut(part)
            .unwrap()
            .get_or_create_path(&path[1..])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_path() {
        let directory = Directory {
            files: HashMap::new(),
            directories: HashMap::from([(
                String::from("dir1"),
                Directory {
                    files: HashMap::new(),
                    directories: HashMap::new(),
                },
            )]),
        };

        assert!(directory.get_path(&["dir1"]).is_some());
    }

    #[test]
    fn get_total_size() {
        let directory = Directory {
            files: HashMap::from([(String::from("file1"), File { size: 13 })]),
            directories: HashMap::from([(
                String::from("dir"),
                Directory {
                    files: HashMap::from([(String::from("file2"), File { size: 14 })]),
                    directories: HashMap::new(),
                },
            )]),
        };

        assert!(directory.get_total_filesize() == 27);
    }

    #[test]
    fn get_or_create_path() {
        let mut directory = Directory::default();
        let _ = directory.get_or_create_path(&["a", "b"]);

        {
            let subdir = directory.get_or_create_path(&["a"]);
            assert!(subdir.directories.keys().collect::<Vec<_>>()[0] == "b");
        }

        assert!(directory.directories.keys().collect::<Vec<_>>()[0] == "a");
    }
}
