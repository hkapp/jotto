use fixedbitset::FixedBitSet as BitSet;
use std::collections::{HashSet, HashMap};

fn main() {
    let words = read_words();
    println!("Before anagram removal: {} words", words.len());

    let (red_words, anagrams) = remove_anagrams(words);
    println!("After anagram removal: {} words", red_words.len());

    let bs = build_bitsets(&red_words);

    search(red_words, bs, anagrams);
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

struct Resources {
    words:          Vec<Word>,
    letter_filters: [BitSet; NLETTERS],
    anagrams:       Anagrams,
}

fn search(all_words: Vec<Word>, letter_filters: [BitSet; NLETTERS], anagrams: Anagrams) {
    let rsc = Resources {
        words: all_words,
        letter_filters,
        anagrams
    };

    let all_candidates = all_ones_bitset(rsc.words.len());
    let mut stack = Vec::new();
    search_rec(&all_candidates, &mut stack, &rsc);
}

fn search_rec(curr_candidates: &BitSet, curr_words: &mut Vec<usize>, rsc: &Resources) {
    #[allow(unused_parens)]
    let final_step = (curr_words.len() == 4);

    let mut rec_candidates = curr_candidates.clone();
    let mut last_i = 0;

    for i in curr_candidates.ones() {
        if final_step {
            // TODO assert that the solutions make sense
            curr_words.push(i);
            solution(&curr_words, rsc);
            curr_words.pop();
        }
        else {
            // Recurse
            // Optimization: perform the filtering of previous words as we go
            rec_candidates.set_range(last_i..i, false);
            let new_candidates = filter_candidates(&rec_candidates, i, rsc);
            curr_words.push(i);
            search_rec(&new_candidates, curr_words, rsc);
            curr_words.pop();
        }
        last_i = i;
    }
}

fn filter_candidates(curr_candidates: &BitSet, new_word_idx: usize, rsc: &Resources) -> BitSet {
    let mut new_candidates = curr_candidates.clone();

    use std::ops::BitAndAssign;
    let new_word = &rsc.words[new_word_idx];
    new_word.chars()
        .map(|letter| {
            let bs_idx = letter_index(letter);
            &rsc.letter_filters[bs_idx]
        })
        .for_each(|filter| new_candidates.bitand_assign(filter));

    return new_candidates;
}

fn solution(solution: &[usize], rsc: &Resources) {
    // Build all the permutations using the anagrams
    // This is all very reminiscent of the initial algorithm...
    fn permutations<'a>(resolved_words: &mut Vec<&'a Word>, init_solution: &[usize], rsc: &'a Resources) {
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
                println!("Solution: {:?}", resolved_words);
            }
            else {
                permutations(resolved_words, init_solution, rsc);
            }
            resolved_words.pop();
        }
    }

    let mut v = Vec::new();
    permutations(&mut v, solution, rsc);
}
