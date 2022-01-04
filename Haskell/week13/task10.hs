main :: IO()
main = do
    print $ orderedTree t1 == True
    print $ orderedTree t2 == False


data BTree = NullT | Node (Float,Float) BTree BTree

t1 = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT)
                                       (Node (4.0,9.0) NullT NullT))
                       (Node (2.0,12.0) NullT
                                        (Node (1.0,15.0) NullT NullT))

t2 = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT)
                                     (Node (7.0,9.0) NullT NullT))
                     (Node (2.0,12.0) NullT
                                      (Node (1.0,15.0) NullT NullT))

orderedTree :: BTree -> Bool
orderedTree NullT = True
orderedTree (Node (x, y) NullT NullT) = True
orderedTree (Node (x, y) NullT nodeRight@(Node (u, v) _ _)) = u < x && y < v && orderedTree nodeRight
orderedTree (Node (x, y) nodeLeft@(Node (u, v) _ _) NullT) = x < u && v < y && orderedTree nodeLeft
orderedTree (Node (x, y) nodeLeft@(Node (u, v) _ _) nodeRight@(Node (c, d) _ _)) = x < u && v < y && c < x && y < d && orderedTree nodeLeft && orderedTree nodeRight