main :: IO()
main = do
    print $ isGracefull t1 == True -- t1 = A
    print $ isGracefull t2 == True -- t2 = B
    print $ isGracefull t3 == False -- t3 = C

data Tree = Nil | Node Int [Tree]
 deriving (Show)

t1 = Node 1 [Node 3 [], Node 5 [], Node 7 [], Node 9 []]
t2 = Node 7 [Node 3 [Node 9 [Node 5 [], Node 1 []]]]
t3 = Node 1 [Node 3 [], Node 5 [Node 42 []], Node 7 [], Node 9 []]

isGracefull :: Tree -> Bool
isGracefull Nil = True
isGracefull (Node _ []) = True
isGracefull (Node val succ) = all (\ (Node x _) -> mod (abs val - x) 2 == 0) succ && (all (==True) $ map isGracefull succ)