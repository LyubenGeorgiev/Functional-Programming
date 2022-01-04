main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2

getOddCompositionValue :: [(Int -> Int)] -> (Int -> Int)
getOddCompositionValue fs = (\ x -> fst $ foldl (\ (res,w) f -> if w then (f res, False) else (res, True)) (x, True) fs)