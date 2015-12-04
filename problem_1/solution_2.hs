import Data.Char (isSpace)

stop :: Integer -> Integer -> String -> Integer
stop n (-1) _ = n
stop n acc ('(':xs) = stop (n + 1) (acc + 1) xs
stop n acc (')':xs) = stop (n + 1) (acc - 1) xs

main :: IO ()
main = do
    string <- readFile "input.txt"
    print $ stop 0 0
          $ takeWhile (not . isSpace) string
