use git2;

pub trait Commit: Sized {
    fn id(&self) -> String;
    fn summary(&self) -> Option<&str>;
    fn author_name(&self) -> Option<String>;
    fn time_seconds(&self) -> i64;
    fn parents(&self) -> impl IntoIterator<Item = Self>;
    fn parent(&self, i: usize) -> Result<Self, git2::Error>;
}

impl Commit for git2::Commit<'_> {
    fn id(&self) -> String {
        git2::Commit::id(self).to_string()
    }

    fn summary(&self) -> Option<&str> {
        git2::Commit::summary(self)
    }

    fn author_name(&self) -> Option<String> {
        self.author().name().map(String::from)
    }

    fn time_seconds(&self) -> i64 {
        self.time().seconds()
    }

    fn parents(&self) -> impl IntoIterator<Item = Self> {
        git2::Commit::parents(self)
    }

    fn parent(&self, i: usize) -> Result<Self, git2::Error> {
        git2::Commit::parent(self, i)
    }
}

pub trait Repository<'a, C: Commit> {
    fn head_commit(&'a self) -> Result<C, git2::Error>;
}

impl<'a> Repository<'a, git2::Commit<'a>> for git2::Repository {
    fn head_commit(&'a self) -> Result<git2::Commit<'a>, git2::Error> {
        self.head()?.peel_to_commit()
    }
}

pub mod test {
    use super::*;

    #[derive(Debug, Default, Clone, PartialEq, Eq)]
    pub struct CommitMock {
        pub id: String,
        pub summary: String,
        pub author: String,
        pub timestamp: i64,
        pub parents: Vec<CommitMock>,
    }

    impl Commit for CommitMock {
        fn id(&self) -> String {
            self.id.clone()
        }

        fn summary(&self) -> Option<&str> {
            Some(&self.summary)
        }

        fn author_name(&self) -> Option<String> {
            Some(self.author.clone())
        }

        fn time_seconds(&self) -> i64 {
            self.timestamp
        }

        fn parents(&self) -> impl IntoIterator<Item = Self> {
            self.parents.clone().into_iter()
        }

        fn parent(&self, i: usize) -> Result<Self, git2::Error> {
            Ok(self.parents[i].clone())
        }
    }

    #[derive(Debug, Default, Clone, PartialEq, Eq)]
    pub struct RepositoryMock {
        pub head_commit: CommitMock,
    }

    impl Repository<'_, CommitMock> for RepositoryMock {
        fn head_commit(&'_ self) -> Result<CommitMock, git2::Error> {
            Ok(self.head_commit.clone())
        }
    }
}
