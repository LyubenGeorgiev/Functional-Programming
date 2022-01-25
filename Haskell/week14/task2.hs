main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

tree :: BTree
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

convert :: BTree -> BTree
convert Nil = Nil
convert t = convertHelper t
 where
     convertHelper :: BTree -> BTree
     convertHelper Nil = Nil
     convertHelper (Node val left right) = (Node (notLessSum t val) (convertHelper left) $ convertHelper right)
     notLessSum :: BTree -> Int -> Int
     notLessSum Nil _ = 0
     notLessSum (Node val left right) lowerBound
      | val >= lowerBound = val + notLessSum left lowerBound + notLessSum right lowerBound
      | otherwise = notLessSum left lowerBound + notLessSum right lowerBound