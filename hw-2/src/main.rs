use git::{Commit, Repository};
use git2;
use std::{
    collections::HashSet,
    env::args,
    io::{self, Write},
    process::{Command, ExitStatus, Stdio},
};

mod git;

fn find_latest_commit<'a, C: Commit, R: Repository<'a, C>>(
    repo: &'a R,
    timestamp: i64,
) -> Result<C, git2::Error> {
    let mut commit = repo.head_commit()?;

    while commit.time_seconds() > timestamp {
        commit = commit.parent(0)?;
    }

    Ok(commit)
}

fn generate_graph_inner<C: Commit>(commit: &C, visited: &mut HashSet<String>) -> String {
    let id = commit.id();

    if visited.contains(&id) {
        return String::new();
    }

    visited.insert(id.clone());

    let mut result = format!(
        "class {} {{\n{}\n{}\n{}\n}}\n",
        id,
        commit.summary().unwrap_or_default(),
        commit.author_name().unwrap_or_default(),
        commit.time_seconds(),
    );

    for parent in commit.parents() {
        result += &format!("{} --> {}\n", id, parent.id().to_string());
        result += &generate_graph_inner(&parent, visited);
    }

    result
}

fn generate_graph<C: Commit>(commit: &C) -> String {
    format!(
        "classDiagram\n{}",
        generate_graph_inner(commit, &mut HashSet::new())
    )
}

fn run_mermaid(code: &str, output: &str) -> io::Result<ExitStatus> {
    let mut child = Command::new("mmdc")
        .args(&["-i", "-", "-o", output])
        .stdin(Stdio::piped())
        .spawn()?;
    child.stdin.as_ref().unwrap().write(code.as_bytes())?;
    child.wait()
}

fn main() -> Result<(), git2::Error> {
    let path = args().nth(1).expect("expected path to a repository");
    let timestamp = args()
        .nth(2)
        .expect("expected timestamp")
        .parse::<i64>()
        .expect("expected a number");
    let output = args().nth(3).expect("expected path to an output");

    let repo = git2::Repository::open(path)?;
    let commit = find_latest_commit(&repo, timestamp)?;
    let code = generate_graph(&commit);
    run_mermaid(&code, &output).expect("unable to generate the image");

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use git::test::{CommitMock, RepositoryMock};

    #[test]
    fn test_find_latest_commit() {
        let head = CommitMock {
            id: String::from("10"),
            timestamp: 10,
            parents: vec![CommitMock {
                id: String::from("5"),
                timestamp: 5,
                ..Default::default()
            }],
            ..Default::default()
        };
        let repo = RepositoryMock { head_commit: head };

        assert!(find_latest_commit(&repo, 5) == Ok(repo.head_commit.parents[0].clone()));
    }

    #[test]
    fn test_generate_graph() {
        let initial = CommitMock {
            id: String::from("initial"),
            ..Default::default()
        };

        let head = CommitMock {
            id: String::from("head"),
            parents: vec![
                CommitMock {
                    id: String::from("left"),
                    parents: vec![initial.clone()],
                    ..Default::default()
                },
                CommitMock {
                    id: String::from("left"),
                    parents: vec![initial.clone()],
                    ..Default::default()
                },
            ],
            ..Default::default()
        };

        let graph = generate_graph(&head);

        assert!(
            graph
                == "\
classDiagram
class head {


0
}
head --> left
class left {


0
}
left --> initial
class initial {


0
}
head --> left
"
        );
    }
}
