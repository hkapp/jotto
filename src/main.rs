fn main() {
    for w in read_words() {
        println!("{}", w);
    }
}

type Word = String;

fn read_words() -> Vec<Word> {
    let input_file = "data/words.csv";
    let mut csv_reader = csv::ReaderBuilder::new()
                            .delimiter(b'\t')
                            .from_path(input_file)
                            .unwrap();

    csv_reader.records()
        .map(|res| res.unwrap())
        .flat_map(|r| r.into_iter()
                        .map(|s| String::from(s))
                        .collect::<Vec<_>>()
                        .into_iter())
        .take(5)
        .collect()
}
