

floorNum :: String -> Integer
floorNum = let up_or_down '(' = (+ 1) .
               up_or_down ')' = (- 1) .
            in foldr up_or_down 0

main :: IO ()
main = do
    string <- getLine
    print $ floorNum string
