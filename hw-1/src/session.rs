use crate::directory::Directory;

#[derive(Debug)]
pub struct Session {
    root: Directory,
    work_dir_stack: Vec<Directory>,
}

#[derive(Debug)]
struct DirectoryNotFound;

impl Session {
    pub fn new(root: Directory) -> Self {
        Self {
            root: root.clone(),
            work_dir_stack: vec![root],
        }
    }

    fn exec_ls(&self) -> String {
        let cwd = self.work_dir_stack.last().unwrap();

        cwd.files
            .keys()
            .map(|key| key.to_string())
            .chain(cwd.directories.keys().map(|key| key.to_string() + "/"))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn exec_cd(&mut self, path: &str) -> Result<(), DirectoryNotFound> {
        if path == "/" {
            self.work_dir_stack = vec![self.root.clone()];
        } else if path == ".." {
            if self.work_dir_stack.len() <= 1 {
                return Err(DirectoryNotFound {});
            }

            self.work_dir_stack.pop();
        } else {
            let cwd = self.work_dir_stack.last().unwrap();

            self.work_dir_stack.push(
                cwd.get_path(&path.split("/").collect::<Vec<_>>())
                    .ok_or(DirectoryNotFound {})?
                    .clone(),
            );
        }

        Ok(())
    }
}
