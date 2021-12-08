import Data.Char

main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD"

reduceStr :: [Char] -> [Char]
reduceStr str
 | str == reduceOnce str = str
 | otherwise = reduceStr $ reduceOnce str
    where
     reduceOnce :: [Char] -> [Char]
     reduceOnce [] = []
     reduceOnce [x] = [x]
     reduceOnce (x:y:xs)
      | (isLower x && isUpper y && x == toLower y) || (isUpper x && isLower y && toLower x == y) = reduceOnce xs
      | otherwise = x : reduceOnce (y : xs)