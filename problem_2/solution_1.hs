data Present = Present { leng :: !Integer
                       , width :: !Integer
                       , height :: !Integer }
                deriving Show

parse :: String -> Present
parse s = Present { leng = l, width = w, height = h }
    where [l, w, h] = map read $ splitBy 'x' s
          splitBy delimiter = foldr f [[]]
                where f c r@(x:xs) | c == delimiter = [] : r
                                   | otherwise = (c : x) : xs


paper :: Present -> Integer
paper Present { leng = l, width = w, height = h} =
    let sides = [l * w, w * h, l * h]
        slack = minimum sides
    in slack + sum (sides ++ sides)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sum
          $ map ( paper . parse )
          $ lines input
