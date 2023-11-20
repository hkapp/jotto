import qualified Data.Set as Set
import Control.Monad(join)
import System.IO.Unsafe(unsafePerformIO)
import Data.Char(isLetter)
import Data.Functor((<&>))
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Map as Map
import Data.List (tails)

main =
  do
    allWords <- readAll
    let (consideredWords, anagrams) = preprocess allWords
    let solutionSubset = search consideredWords
    let solutions = solutionSubset >>= permutations anagrams
    print solutions
    print $ length solutions

readAll :: IO [String]
readAll =
  do
    let inputFile = "../data/words.txt"
    fileContent <- readFile inputFile
    let fileLines = lines fileContent
    -- Remove '\r' characters in every line
    let actualWords = fileLines <&> (filter isLetter)
    return $ take 100000 actualWords

type Anagrams = Map (Set Char) [String]

preprocess :: [String] -> ([String], Anagrams)
preprocess =
  let
    fiveLetters w = length w == 5
    noDup w = length (Set.fromList w) == 5
  in
    reduceAnagrams . filter noDup . filter fiveLetters

reduceAnagrams :: [String] -> ([String], Anagrams)
reduceAnagrams allWords =
  let
    anagrams = groupIntoMap Set.fromList allWords
    reducedWords = head <$> Map.elems anagrams
  in
    (reducedWords, anagrams)

groupIntoMap :: (Ord k) => (a -> k) -> [a] -> Map k [a]
groupIntoMap f = Map.fromListWith (++) . map (\x -> (f x, [x]))

search :: [String] -> [[String]]
search ws = searchRec ws []

searchRec :: [String] -> [String] -> [[String]]

searchRec _ currSol | length currSol == 5 = [currSol]

searchRec allWords currSol =
  let
    candidates = filter validCandidate (tails allWords)

    validCandidate []    = False
    validCandidate (w:_) = all (flip Set.notMember currLetters) w

    currLetters = Set.fromList $ flatten currSol
  in
    candidates >>= (\(w:remWords) -> searchRec remWords (w:currSol))

flatten = join

permutations :: Anagrams -> [String] -> [[String]]
permutations _        []     = [[]]
permutations anagrams (w:ws) = (anagrams ! (Set.fromList w)) >>= (\q -> map ((:) q) (permutations anagrams ws))

debug x y =
  unsafePerformIO $
    do
      print x
      return y
