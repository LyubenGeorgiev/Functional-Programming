main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [3, 4, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]


listLeaves :: (Eq a) => [(a,a,a)] -> [a]
listLeaves [] = []
listLeaves xs = filter (not . flip elem notLeaves) $ concatMap (\ (x,y,z) -> [x, y, z]) xs
 where notLeaves = map (\ (x,_,_) -> x) xs