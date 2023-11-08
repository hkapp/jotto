use fixedbitset::FixedBitSet as BitSet;
use std::collections::HashSet;
use std::cmp::Ordering;

fn main() {
    let words = read_words();
    let bs = build_bitsets(&words);
    let mat_candidates = materialize_candidates(&words, &bs);

    search(&words, &mat_candidates);
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

// This basically acts as a graph in adjacency list format
type Neighbours = Vec<WordIdx>;
type WordIdx = usize;

fn materialize_candidates(words: &[Word], letter_filters: &[BitSet]) -> Vec<Neighbours> {
    words.into_iter()
        .enumerate()
        .map(|(i, w)| {
            let wbs = word_bitset(w, letter_filters);
            wbs.ones()
                .filter(|candidate| *candidate > i) // only looking at words after myself
                .collect()
        })
        .collect()
}

fn word_bitset(w: &Word, letter_filters: &[BitSet]) -> BitSet {
    let nwords = letter_filters[0].len(); // in bits
    let mut candidates = all_ones_bitset(nwords);

    use std::ops::BitAndAssign;
    w.chars()
        .map(|letter| {
            let bs_idx = char_to_bitset_index(letter);
            &letter_filters[bs_idx]
        })
        // TODO clone the first of these 5 bitsets and use that as the start
        .for_each(|filter| candidates.bitand_assign(filter));

    return candidates;
}

fn search(all_words: &[Word], mat_candidates: &[Neighbours]) {
    let all_candidates = (0..all_words.len()).collect::<Vec<_>>();
    let mut stack = Vec::new();
    search_rec(&all_candidates, mat_candidates, &mut stack, all_words);
}

// TODO: we can easily optimize avoiding having to materialize the first level if we implement our own Index that always returns the index
fn search_rec(curr_candidates: &Neighbours, mat_candidates: &[Neighbours], curr_words: &mut Vec<WordIdx>, all_words: &[Word]) {
    #[allow(unused_parens)]
    let final_step = (curr_words.len() == 4);
    let mut rec_candidates = Vec::new();

    for i in curr_candidates.into_iter().cloned() {
        if final_step {
            // TODO assert that the solutions make sense
            curr_words.push(i);
            println!("Solution: {:?}", curr_words.iter().map(|widx| &all_words[*widx]).collect::<Vec<_>>());
            curr_words.pop();
        }
        else {
            // Recurse
            merge_sorted(curr_candidates, &mat_candidates[i], &mut rec_candidates);
            curr_words.push(i);
            search_rec(&rec_candidates, mat_candidates, curr_words, all_words);
            curr_words.pop();
        }
    }
}

// Merge two sorted slices
// These must be in ascending order
fn merge_sorted<T: Ord + Clone>(left: &[T], right: &[T], result: &mut Vec<T>) {
    let mut left_idx = 0;
    let mut right_idx = 0;
    result.clear();

    // Any elements still present on either side when the other side is done cannot be in the result set
    while left_idx < left.len() && right_idx < right.len() {
        let left_val  = &left[left_idx];
        let right_val = &right[right_idx];

        // Case 1: left == right
        if left_val == right_val {
            result.push(left_val.clone());
            left_idx += 1;
            right_idx += 1;
        }
        // Case 2: left > right
        // Move forward on the right
        // Explanation: ascending order -> the next elements on the left are all greater than the current one
        else if left_val > right_val {
            right_idx += 1;
        }
        // Opposite of case 2
        else /* left_val < right_val */ {
            left_idx += 1;
        }
    }
}
