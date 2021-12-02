import Data.List

main :: IO()
main = do
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True

isPrimeG :: Int -> Bool
isPrimeG x = x /= 1 && helper 2
 where
     helper :: Int -> Bool
     helper current = current >= x || mod x current /= 0 && helper (current + 1)

isPrimeLC :: Int -> Bool
isPrimeLC x = x /= 1 && null (filter (\z -> mod x z == 0) [2..x-1])