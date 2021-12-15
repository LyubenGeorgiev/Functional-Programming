import Data.List

main :: IO()
main = do
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 5] == (True, 1)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 7] == (True, 3)
    print $ isImage [4, 5, 6, 7] [1, 2, 3, 4] == (True, -3)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 6] == (False, 0)
    print $ isImage [1, 2] [-1, -2] == (False, 0)
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 4] == (False, 0)

isImage :: [Int] -> [Int] -> (Bool, Int)
isImage [] _ = (False, 0)
isImage _ [] = (False, 0)
isImage xs ys
 | length n == 1 = (True, head n)
 | otherwise = (False, 0)
     where n = nub $ zipWith (-) ys xs