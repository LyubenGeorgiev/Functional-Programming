main :: IO()
main = do
    print $ combine [(1, 2), (1, 2)] == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)

combine :: [(Int, Int)] -> (Int, Int)
combine [] = error "Nothing to combine"
combine xs = foldl (\ (resL, resR) (x, y) -> (resL*10 + (min x y), resR*10 + (max x y))) (0, 0) xs