use fixedbitset::FixedBitSet as BitSet;
use std::collections::HashSet;

fn main() {
    let words = read_words();
    let bs = build_bitsets(&words);

    search(&words, &bs[..]);
}

type Word = String;

fn read_words() -> Vec<Word> {
    fn all_letters_distinct(s: &str) -> bool {
        let chars_used: HashSet<_> = s.chars().collect();
        chars_used.len() == s.len()
    }

    let input_file = "data/words.txt";
    std::fs::read_to_string(input_file)
                .unwrap()
                .lines()
                .filter(|s| s.len() == 5)
                .filter(|s| all_letters_distinct(s))
                .map(|s| String::from(s))
                .collect()
}

const NLETTERS: usize = 26;

fn all_ones_bitset(num_bits: usize) -> BitSet {
    let mut bs = BitSet::with_capacity(num_bits);
    bs.set_range(.., true);
    return bs;
}

fn char_to_bitset_index(c: char) -> usize {
    c as usize - 'a' as usize
}

fn build_bitsets(words: &[Word]) -> [BitSet; NLETTERS] {
    let sample_bitset = all_ones_bitset(words.len());
    let mut bitsets: [BitSet; NLETTERS] = vec![sample_bitset; NLETTERS]
                                            .try_into()
                                            .unwrap();

    // TODO use rayon here
    words.into_iter()
        .enumerate()
        .flat_map(|(i, w)|
            w.chars()
                .map(move |letter| (char_to_bitset_index(letter), i)))
        .for_each(|(bs_idx, w_idx)| bitsets[bs_idx].set(w_idx, false));

    return bitsets;
}

fn search(all_words: &[Word], letter_filters: &[BitSet]) {
    let all_candidates = all_ones_bitset(all_words.len());
    let mut stack = Vec::new();
    search_rec(&all_candidates, letter_filters, &mut stack, all_words);
}

fn search_rec(curr_candidates: &BitSet, letter_filters: &[BitSet], curr_words: &mut Vec<usize>, all_words: &[Word]) {
    #[allow(unused_parens)]
    let final_step = (curr_words.len() == 4);

    let mut rec_candidates = curr_candidates.clone();
    let mut last_i = 0;

    for i in curr_candidates.ones() {
        if final_step {
            // TODO assert that the solutions make sense
            curr_words.push(i);
            println!("Solution: {:?}", curr_words.iter().map(|widx| &all_words[*widx]).collect::<Vec<_>>());
            curr_words.pop();
        }
        else {
            // Recurse
            // Optimization: perform the filtering of previous words as we go
            rec_candidates.set_range(last_i..i, false);
            let new_candidates = filter_candidates(&rec_candidates, i, all_words, letter_filters);
            curr_words.push(i);
            search_rec(&new_candidates, letter_filters, curr_words, all_words);
            curr_words.pop();
        }
        last_i = i;
    }
}

fn filter_candidates(curr_candidates: &BitSet, new_word_idx: usize, all_words: &[Word], letter_filters: &[BitSet]) -> BitSet {
    let mut new_candidates = curr_candidates.clone();

    use std::ops::BitAndAssign;
    let new_word = &all_words[new_word_idx];
    new_word.chars()
        .map(|letter| {
            let bs_idx = char_to_bitset_index(letter);
            &letter_filters[bs_idx]
        })
        .for_each(|filter| new_candidates.bitand_assign(filter));

    return new_candidates;
}
