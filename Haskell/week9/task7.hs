import Data.List

main :: IO()
main = do
    print $ perfect 1 == False
    print $ perfect 6 == True
    print $ perfect 495 == False
    print $ perfect 33550336 == True
    print $ perfect 34355544064 == False

-- perfect :: Int -> Bool
-- perfect x = x == sum (filter (\z -> mod x z == 0) [1..x-1])

perfect :: Int -> Bool
perfect x = x == (sum $ nub $ filter (\ y -> y < x && mod x y == 0) $ map product (subsequences $ getDivisors x 2))

getDivisors :: Int -> Int -> [Int]
getDivisors 0 _ = []
getDivisors x current
 | current >= upperBound = [x]
 | mod x current == 0 = current : getDivisors (div x current) current
 | otherwise = getDivisors x (current + 1)
 where upperBound = (ceiling $ sqrt $ fromIntegral x)