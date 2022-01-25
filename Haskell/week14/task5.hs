import Data.List

main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

data BTree = Nil | Node Int BTree BTree
 deriving (Show)

numberBTree :: BTree
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

levelSum :: BTree -> Int -> Int
levelSum tree lvl = sum $ getLevel tree lvl

getLevel :: BTree -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node x _ _) 0 = [x]
getLevel (Node x left right) level = getLevel left (level - 1) ++ getLevel right (level - 1)

cone :: BTree -> Bool
cone tree = let t = map (levelSum tree) $ takeWhile (not . null . (getLevel tree)) [0..] in t == (sort t)