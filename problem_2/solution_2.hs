import PresentUtil

ribbon :: Present -> Integer
ribbon Present { leng = l, width = w, height = h } = volume + minimum perims
    where perims = [ (2 * l) + (2 * w)
                   , (2 * w) + (2 * h)
                   , (2 * h) + (2 * l) ]
          volume = l * w * h

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sum
          $ map ( ribbon . parse )
          $ lines input
