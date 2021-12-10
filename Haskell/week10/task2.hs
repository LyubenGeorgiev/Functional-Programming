import Data.Char

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [ i | i <- [1..], isPrime i && contains i d]

isPrime :: Int -> Bool
isPrime k = k /= 1 && null [ x | x <- [2..k - 1], mod k x == 0]

contains :: Int -> Int -> Bool
contains n d = elem (intToDigit d) (show n)