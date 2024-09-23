use csv::{Reader, Writer};
use std::{
    fs::OpenOptions,
    io::{self, BufReader, BufWriter},
};

pub type HistoryRecord = (u64, String);

pub fn load_history(path: &str) -> io::Result<Vec<HistoryRecord>> {
    let file = OpenOptions::new().read(true).open(path)?;
    let mut reader = BufReader::new(&file);
    let mut csv_reader = Reader::from_reader(&mut reader);
    let mut history = vec![];

    for record in csv_reader.records() {
        let record = record?;

        history.push((
            record.get(0).unwrap().parse().unwrap(),
            record.get(1).unwrap().into(),
        ));
    }

    Ok(history)
}

pub fn dump_history(path: &str, history: &[HistoryRecord]) -> io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)?;
    let mut writer = BufWriter::new(&mut file);
    let mut csv_writer = Writer::from_writer(&mut writer);

    for record in history {
        csv_writer
            .write_record(&[record.0.to_string(), record.1.clone()])
            .unwrap();
    }

    csv_writer.flush()
}
