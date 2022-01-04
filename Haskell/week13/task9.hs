main :: IO()
main = do
    print $ (g [2.7, 3.0 ..]) 2.2 3 == -0.4399999999999998

g :: (Num a) => [a] -> (a -> Int -> a)
g xs = (\ x y -> product $ take y $ zipWith (-) (repeat x) xs)