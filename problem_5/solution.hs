import Data.List
import Data.Maybe (isJust)

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _   = False

has3Vowels :: String -> Bool
has3Vowels = (>= 3) . countVowels
    where countVowels :: String -> Integer
          countVowels = foldl (\acc ch -> if isVowel ch then acc + 1 else acc) 0

hasDoubleLetter :: String -> Bool
hasDoubleLetter = isJust . find (\x -> length x > 1) . group

hasNoProhibited :: String -> Bool
hasNoProhibited s = not $
                    or [ "ab" `isInfixOf` s
                       , "cd" `isInfixOf` s
                       , "pq" `isInfixOf` s
                       , "xy" `isInfixOf` s ]

isNice :: String -> Bool
isNice s = has3Vowels s &&
           hasDoubleLetter s &&
           hasNoProhibited s

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ length
          $ filter isNice
          $ lines input
