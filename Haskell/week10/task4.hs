main :: IO()
main = do
    print $ primesInRange 1 100 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
    print $ primesInRange 100 1 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

primesInRange :: Int -> Int -> [Int]
primesInRange x y = [i | i <- [(min x y)..(max x y)], i /= 1 && null [ z | z <- [2..i - 1], mod i z == 0]]
