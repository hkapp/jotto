import qualified Data.Set as Set
import Control.Monad(join)
import System.IO.Unsafe(unsafePerformIO)
import Data.Char(isLetter)
import Data.Functor((<&>))

main =
  do
    allWords <- readAll
    let solutions = search $ preprocess allWords
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
    return $ take 40000 actualWords

preprocess :: [String] -> [String]
preprocess =
  let
    fiveLetters w = length w == 5
    noDup w = length (Set.fromList w) == 5
  in
    filter noDup . filter fiveLetters

search :: [String] -> [[String]]
search ws = searchRec ws []

searchRec :: [String] -> [String] -> [[String]]

searchRec _ currSol | length currSol == 5 = [currSol]

searchRec allWords currSol =
  let
    candidates = filter validCandidate allWords
    validCandidate = all (flip Set.notMember currLetters)
    currLetters = Set.fromList $ flatten currSol
  in
    candidates >>= (\w -> searchRec allWords (w:currSol))

flatten = join

debug x y =
  unsafePerformIO $
    do
      print x
      return y
