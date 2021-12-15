main :: IO()
main = do
    print $ combine [(1, 2), (1, 2)] == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)

combine :: [(Int, Int)] -> (Int, Int)
combine [] = error "Nothing to combine"
combine xs = (combineMin xs, combineMax xs)

combineMin :: [(Int, Int)] -> Int
combineMin = read . foldl (\ res (x, y) -> res ++ show (min x y)) ""

combineMax :: [(Int, Int)] -> Int
combineMax = read . foldl (\ res (x, y) -> res ++ show (max x y)) ""