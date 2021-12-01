import Data.List

main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True
    
areAmicable :: Int -> Int -> Bool
areAmicable x y = (x == (sum (filter (\z -> y `mod` z == 0) [1..(y-1)]))) || (y == (sum (filter (\z -> y `mod` z == 0) [1..(x-1)])))