main :: IO()
main = do
    print $ constructMaxBTree [3, 2, 1, 6, 0, 5] == t2

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

t2 = Node 6 (Node 3 Nil (Node 2 Nil (Node 1 Nil Nil))) (Node 5 (Node 0 Nil Nil) Nil)

constructMaxBTree :: [Int] -> BTree
constructMaxBTree [] = Nil
constructMaxBTree xs = helper $ maxSplit xs
 where
     safeTail :: [Int] -> [Int]
     safeTail ys = if null ys then [] else tail ys
     maxSplit :: [Int] -> (Int,[Int],[Int])
     maxSplit ys = let m = maximum ys in (m, takeWhile (/=m) ys, safeTail $ dropWhile (/=m) ys)
     helper :: (Int,[Int],[Int]) -> BTree
     helper (x,[],[]) = Node x Nil Nil
     helper (x,ls,[]) = Node x (helper $ maxSplit ls) Nil
     helper (x,[],rs) = Node x Nil (helper $ maxSplit rs)
     helper (x,ls,rs) = Node x (helper $ maxSplit ls) (helper $ maxSplit rs)