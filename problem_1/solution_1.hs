import Data.Char (isSpace)

main :: IO ()
main = let upOrDown '(' a = a + 1
           upOrDown ')' a = a - 1
       in do
        string <- readFile "input.txt"
        print $ foldr upOrDown (0 :: Integer)
              $ takeWhile (not . isSpace) string
