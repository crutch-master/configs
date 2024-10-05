use git2::{self, Commit, Oid, Repository};
use std::{
    collections::HashSet,
    env::args,
    io::{self, Write},
    process::{Command, ExitStatus, Stdio},
};

fn find_latest_commit(repo: &Repository, timestamp: i64) -> Result<Commit, git2::Error> {
    let mut commit = repo.head()?.peel_to_commit()?;

    while commit.time().seconds() > timestamp {
        commit = commit.parent(0)?;
    }

    Ok(commit)
}

fn generate_graph_inner(commit: &Commit, visited: &mut HashSet<Oid>) -> String {
    if visited.contains(&commit.id()) {
        return String::new();
    }

    visited.insert(commit.id());

    let id = commit.id().to_string();
    let mut result = format!(
        "class {} {{\n{}\n{}\n{}\n}}\n",
        id,
        commit.summary().unwrap_or_default(),
        commit.author().name().unwrap_or_default(),
        commit.time().seconds(),
    );

    for parent in commit.parents() {
        result += &format!("{} --> {}\n", id, parent.id().to_string());
        result += &generate_graph_inner(&parent, visited);
    }

    result
}

fn generate_graph(commit: &Commit) -> String {
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

    let repo = Repository::open(path)?;
    let commit = find_latest_commit(&repo, timestamp)?;
    let code = generate_graph(&commit);
    run_mermaid(&code, &output).expect("unable to generate the image");

    Ok(())
}
