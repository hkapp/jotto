use fixedbitset::FixedBitSet as BitSet;
use std::collections::{HashSet, HashMap};
use std::time::Instant;

fn main() {
    fn time<F: FnOnce() -> T, T>(phase: &str, f: F) -> T {
        let start = Instant::now();
        let res = f();
        let duration = start.elapsed();
        println!("{}: {:?}", phase, duration);
        res
    }

    let words = time("read_words", || read_words());
    //println!("Before anagram removal: {} words", words.len());

    let (red_words, anagrams) = time("remove_anagrams", || remove_anagrams(words));
    //println!("After anagram removal: {} words", red_words.len());

    let bs = time("build_bitsets", || build_bitsets(&red_words));
    let mat_candidates = time("materialize_candidates", || materialize_candidates(&red_words, &bs));

    let answer = time("search", || search(red_words, mat_candidates, anagrams));
    println!("Number of solutions: {}", answer);
    assert_eq!(answer, 831);
}

type Word = String;

fn read_words() -> Vec<Word> {
    fn all_letters_distinct(s: &str) -> bool {
        // TODO this can be improved with word_as_bits()
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

type Anagrams = HashMap<WordBits, Vec<Word>>;
type WordBits = u32;  // 1 bit per letter

fn remove_anagrams(words: Vec<Word>) -> (Vec<Word>, Anagrams) {
    // 1. Convert the vector of words into a hashmap from bit format to word(s)
    let mut anagrams = HashMap::new();
    for w in words {
        let word_bits = word_as_bits(&w);
        match anagrams.get_mut(&word_bits) {
            None => {
                anagrams.insert(word_bits, vec![w]);
            }
            Some(v) => {
                v.push(w);
            }
        }
    }

    // 2. Pick one word from each "bucket" to form the new word vector
    let red_words = anagrams.values()
                        .map(|v| v.first().unwrap().clone())
                        .collect::<Vec<_>>();

    return (red_words, anagrams);
}

fn word_as_bits(word: &Word) -> WordBits {
    fn set_bit(bits: &mut WordBits, letter: char) {
        let bit_pos = letter_index(letter);
        let mask = 1 << bit_pos;
        *bits |= mask;
    }

    let mut bits = 0;
    word.chars()
        .for_each(|letter| set_bit(&mut bits, letter));

    return bits;
}

const NLETTERS: usize = 26;

fn all_ones_bitset(num_bits: usize) -> BitSet {
    let mut bs = BitSet::with_capacity(num_bits);
    bs.set_range(.., true);
    return bs;
}

fn letter_index(c: char) -> usize {
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
                .map(move |letter| (letter_index(letter), i)))
        .for_each(|(bs_idx, w_idx)| bitsets[bs_idx].set(w_idx, false));

    return bitsets;
}

// This basically acts as a graph in adjacency list format
type Neighbours = BitSet;

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
            let bs_idx = letter_index(letter);
            &letter_filters[bs_idx]
        })
        // TODO clone the first of these 5 bitsets and use that as the start
        .for_each(|filter| candidates.bitand_assign(filter));

    return candidates;
}

struct Resources {
    words:          Vec<Word>,
    mat_candidates: Vec<Neighbours>,
    anagrams:       Anagrams,
}

fn search(all_words: Vec<Word>, mat_candidates: Vec<Neighbours>, anagrams: Anagrams) -> usize {
    let rsc = Resources {
        words: all_words,
        mat_candidates,
        anagrams
    };

    use rayon::prelude::*;

    let nwords = rsc.words.len();
    (0..nwords).into_par_iter()
        .map(|word_idx| {
            let candidates = &rsc.mat_candidates[word_idx];
            let mut stack = vec![word_idx];
            let mut nsolutions = 0;
            search_rec(candidates, &mut stack, &mut nsolutions, &rsc);
            nsolutions
        })
        .sum()
}

// TODO: we can easily optimize avoiding having to materialize the first level if we implement our own Index that always returns the index
fn search_rec(curr_candidates: &Neighbours, curr_words: &mut Vec<usize>, nsolutions: &mut usize, rsc: &Resources) {
    #[allow(unused_parens)]
    let final_step = (curr_words.len() == 4);

    for i in curr_candidates.ones() {
        if final_step {
            // TODO assert that the solutions make sense
            curr_words.push(i);
            solution(&curr_words, rsc, nsolutions);
            curr_words.pop();
        }
        else {
            // Recurse
            let rec_candidates = curr_candidates & &rsc.mat_candidates[i];
            curr_words.push(i);
            search_rec(&rec_candidates, curr_words, nsolutions, rsc);
            curr_words.pop();
        }
    }
}

fn solution(solution: &[usize], rsc: &Resources, nsolutions: &mut usize) {
    // Build all the permutations using the anagrams
    // This is all very reminiscent of the initial algorithm...
    fn permutations<'a>(resolved_words: &mut Vec<&'a Word>, nsolutions: &mut usize, init_solution: &[usize], rsc: &'a Resources) {
        let curr_level = resolved_words.len();
        #[allow(unused_parens)]
        let final_step = (curr_level == (init_solution.len() - 1));
        let word_index = init_solution[curr_level];
        let word = &rsc.words[word_index];
        let word_bits = word_as_bits(word);
        let word_anagrams = rsc.anagrams.get(&word_bits).unwrap();
        for word_collision in word_anagrams {
            resolved_words.push(word_collision);
            if final_step {
                //println!("Solution: {:?}", resolved_words);
                *nsolutions += 1;
            }
            else {
                permutations(resolved_words, nsolutions, init_solution, rsc);
            }
            resolved_words.pop();
        }
    }

    let mut v = Vec::new();
    permutations(&mut v, nsolutions, solution, rsc);
}
