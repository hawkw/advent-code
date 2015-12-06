import PresentUtil

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
