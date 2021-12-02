main :: IO()
main = do
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False

reverseNum :: Int -> Int
reverseNum 0 = 0
reverseNum n = mod n 10 * 10 ^ (floor . logBase 10 . fromIntegral) n + reverseNum (div n 10)

isPalindrome :: Int -> Bool
isPalindrome x = x == reverseNum x