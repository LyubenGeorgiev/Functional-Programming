import Data.List

main :: IO()
main = do
    print $ willItFly [1, 4, 2, 3] == True
    print $ willItFly [1, 4, 2, -1, 6] == False

    print $ formatDuration 0 == "now"
    print $ formatDuration 62 == "1 minute and 2 seconds"
    print $ formatDuration 3600 == "1 hour"
    print $ formatDuration 3662 == "1 hour, 1 minute and 2 seconds"

    print $ (formatDuration 100000) == "1 day, 3 hours, 46 minutes and 40 seconds"
    print $ (formatDuration 200000) == "2 days, 7 hours, 33 minutes and 20 seconds"
    print $ (formatDuration 32000000) == "1 year, 5 days, 8 hours, 53 minutes and 20 seconds"
    print $ (formatDuration 68000000) == "2 years, 57 days, 53 minutes and 20 seconds"
    print $ (formatDuration 67999980) == "2 years, 57 days and 53 minutes"
    print $ (formatDuration 67809200) == "2 years, 54 days, 19 hours, 53 minutes and 20 seconds"
    print $ (formatDuration 107806020) == "3 years, 152 days, 18 hours and 7 minutes"


willItFly :: [Int] -> Bool
willItFly [] = True
willItFly [_] = True
willItFly xs = [1..(length xs - 1)] == (sort $ zipWith (\ x y -> abs(x-y)) (init xs) (tail xs))

formatDuration :: Int -> String
formatDuration x
 | x < 0 = "Negatve time given!"
 | otherwise = helper timesList
 where
     correctStrings = [" year", " day", " hour", " minute", " second"]

     times = (div x 31536000) : (div (mod x 31536000) 86400) : (div (mod (mod x 31536000) 86400) 3600) : 
             (div (mod (mod (mod x 31536000) 86400) 3600) 60) : (mod (mod (mod (mod x 31536000) 86400) 3600) 60) : []

     timesList = filter (/="") $ map (\ (x, str) -> case () of
      _ | x == 0    -> ""
        | x == 1    -> show 1 ++ str
        | otherwise -> show x ++ str ++ "s") $ zip times correctStrings

     helper :: [String] -> String
     helper (u:v:t:xs) = u ++ ", " ++ helper (v:t:xs)
     helper (v:t:xs) = v ++ " and " ++ t
     helper (t:xs) = t
     helper _ = "now"