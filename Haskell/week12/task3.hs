main :: IO()
main = do
    print $ colourBTree
    print $ highest Red colourBTree == 4
    print $ highest Green colourBTree == 3
    print $ highest Blue colourBTree == 4

data Color = Red | Green | Blue
 deriving (Show, Eq)
data BTree = Nil | Node Color BTree BTree
 deriving (Show)

colourBTree = (Node Blue (Node Green (Node Blue (Node Red Nil Nil) Nil) (Node Blue Nil Nil)) (Node Red (Node Green (Node Blue Nil Nil) Nil) (Node Red Nil Nil)))

getLevel :: BTree -> Int -> [Color]
getLevel Nil _ = []
getLevel (Node x _ _) 0 = [x]
getLevel (Node x left right) level = getLevel left (level - 1) ++ getLevel right (level - 1)

highest :: Color -> BTree -> Int
highest color tree = snd $ last $ filter ((elem color) . fst) $ takeWhile (not . null . fst) [(getLevel tree i, i+1) | i <- [0..]]