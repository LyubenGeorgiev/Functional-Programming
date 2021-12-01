import Data.List

main :: IO()
main = do
    print $ perfect 1 == False
    print $ perfect 6 == True
    print $ perfect 495 == False
    print $ perfect 33550336 == True

perfect :: Int -> Bool
perfect x = x == (sum (filter (\z -> (mod x z) == 0) [1..(x-1)]))