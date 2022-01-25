import Data.List

main :: IO()
main = do
    print $ getSunk database == [("Guadalcanal",["Kirishima"]),("North Atlantic",["Bismarck","Hood"]),("North Cape",["Schamhorst"]),("Surigao Strait",["Fuso","Yamashiro"])]
    print $ inBattleAfterDamaged database == ["California","Prince of Wales"]

    print $ grandchildrenIncreased t1 == True
    print $ grandchildrenIncreased t2 == False


type Name = String
type Date = String
type Class = String
type Result = String
type Launched = Int
data Battle = Battle Name Date
 deriving (Show, Eq)
data Ship = Ship Name Class Launched
 deriving (Show)
data Outcome = Outcome Name Name Result
 deriving (Show)
type Database = ([Outcome], [Battle], [Ship])

-- Probably not a good idea to override it like that but it works fine in the current code
instance Eq Outcome where
  (Outcome x _ _) == (Outcome y _ _) = x == y

-- Need this so I can sort the battles by date
instance Ord Battle where
  (Battle _ date1) <= (Battle _ date2) = cmp (parse date1) (parse date2)
    where
        parse :: String -> [Int]
        parse d = map (read) $ split '-' d
        cmp :: [Int] -> [Int] -> Bool
        cmp d1 d2 = d1 == d2 || (null $ takeWhile (==False) $ zipWith (<) d1 d2)
        split :: (Eq a) => a -> [a] -> [[a]]
        split d [] = []
        split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s


outcomes :: [Outcome]
outcomes = [ Outcome "Bismarck" "North Atlantic" "sunk", Outcome "California" "Surigao Strait" "ok", Outcome "Duke of York" "North Cape" "ok", Outcome "Fuso" "Surigao Strait" "sunk", Outcome "Hood" "North Atlantic" "sunk", Outcome "King George V" "North Atlantic" "ok", Outcome "Kirishima" "Guadalcanal" "sunk", Outcome "Prince of Wales" "North Atlantic" "damaged", Outcome "Rodney" "North Atlantic" "ok", Outcome "Schamhorst" "North Cape" "sunk", Outcome "South Dakota" "Guadalcanal" "damaged", Outcome "Tennessee" "Surigao Strait" "ok", Outcome "Washington" "Guadalcanal" "ok", Outcome "Prince of Wales" "Guadalcanal" "ok", Outcome "West Virginia" "Surigao Strait" "ok", Outcome "Yamashiro" "Surigao Strait" "sunk", Outcome "California" "Guadalcanal" "damaged" ]

battles :: [Battle]
battles = [ Battle "Guadalcanal" "1942-11-15", Battle "North Atlantic" "1941-05-25", Battle "North Cape" "1943-12-26", Battle "Surigao Strait" "1944-10-25" ]

ships :: [Ship]
ships = [ Ship "California" "Tennessee" 1921, Ship "Haruna" "Kongo" 1916, Ship "Hiei" "Kongo" 1914, Ship "Iowa" "Iowa" 1943, Ship "Kirishima" "Kongo" 1915, Ship "Kongo" "Kongo" 1913, Ship "Missouri" "Iowa" 1944, Ship "Musashi" "Yamato" 1942, Ship "New Jersey" "Iowa" 1943, Ship "North Carolina" "North Carolina" 1941, Ship "Ramillies" "Revenge" 1917, Ship "Renown" "Renown" 1916, Ship "Repulse" "Renown" 1916, Ship "Resolution" "Renown" 1916, Ship "Revenge" "Revenge" 1916, Ship "Royal Oak" "Revenge" 1916, Ship "Royal Sovereign" "Revenge" 1916, Ship "Tennessee" "Tennessee" 1920, Ship "Washington" "North Carolina" 1941, Ship "Wisconsin" "Iowa" 1944, Ship "Yamato" "Yamato" 1941, Ship "Yamashiro" "Yamato" 1947, Ship "South Dakota" "North Carolina" 1941, Ship "Bismarck" "North Carolina" 1911, Ship "Duke of York" "Renown" 1916, Ship "Fuso" "Iowa" 1940, Ship "Hood" "Iowa" 1942, Ship "Rodney" "Yamato" 1915, Ship "Yanashiro" "Yamato" 1918, Ship  "Schamhorst" "North Carolina" 1917, Ship "Prince of Wales" "North Carolina" 1937, Ship "King George V" "Iowa" 1942, Ship "West Virginia" "Iowa" 1942 ]

database :: Database
database = (outcomes, battles, ships)

getSunk :: Database -> [(Name, [Name])]
getSunk (o,b,_) = map (\ (Battle name _) -> (name, map (\ (Outcome ship _ _) -> ship) $ filter (\ (Outcome ship battle result) -> name == battle && result == "sunk") o)) b

inBattleAfterDamaged :: Database -> [Name]
inBattleAfterDamaged (o,b,s) = sort $ nub $ solve possible
 where
     possible = concatMap (map (\ (Outcome n _ s) -> (n, s))) $ map (filter (not . (flip elem toFilter))) separated
     separated = map (\ (Battle bName _) -> filter (\ (Outcome sName oBName oRes) -> bName == oBName) o) $ sort b
     toFilter = unique $ concat separated
     solve :: [(Name,Result)] -> [Name]
     solve [] = []
     solve (t@(x,y):xs) = if y == "damaged" && (not $ null $ filter (\ (x1,y1) -> x1 == x) xs) then x : solve xs else solve xs
     unique :: (Eq a) => [a] -> [a]
     unique [] = []
     unique (x:xs)
      | elem x xs = unique $ filter (/= x) xs
      | otherwise = x : (unique $ filter (/= x) xs)

data BTree = Nil | Node Int BTree BTree
 deriving (Show)

t1 :: BTree
t1 = Node 1 (Node (-1) (Node 2 Nil Nil) (Node 2 (Node 0 Nil Nil) Nil)) (Node (-1) Nil Nil)

t2 :: BTree
t2 = Node 1 (Node 2 (Node 1 Nil Nil) (Node 1 (Node 10 Nil Nil) Nil)) (Node 3 Nil Nil)


grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Nil = True
grandchildrenIncreased (Node val left right) = checkChildren left && checkChildren right && grandchildrenIncreased left && grandchildrenIncreased right
 where
     checkChildren :: BTree -> Bool
     checkChildren Nil = True
     checkChildren (Node _ Nil Nil) = True
     checkChildren (Node _ Nil (Node v _ _)) = v > val
     checkChildren (Node _ (Node v _ _) Nil) = v > val
     checkChildren (Node _ (Node v1 _ _) (Node v2 _ _)) = v1 > val && v2 > val