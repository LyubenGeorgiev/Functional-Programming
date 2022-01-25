main :: IO()
main = do
    print $ toBinaryIndexed tree == Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

tree :: BTree Int
tree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

toBinaryIndexed :: (Eq a) => BTree a -> BTree (a,Int)
toBinaryIndexed Nil = Nil
toBinaryIndexed t = helper t
 where
     values :: BTree a -> [a]
     values Nil = []
     values (Node val left right) = values left ++ [val] ++ values right
     storedValues = values t
     getIndex :: (Eq a) => a -> [a] -> Int
     getIndex _ [] = -1
     getIndex y (x:xs)
      | y == x = 0
      | otherwise = 1 + getIndex y xs
     helper Nil = Nil
     helper (Node x left right) = (Node (x,getIndex x storedValues) (helper left) (helper right))