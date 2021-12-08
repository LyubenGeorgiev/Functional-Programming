main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6

reverseOrdSuff :: Int -> Int
reverseOrdSuff x = helper (div x 10) (mod x 10)
 where
     helper :: Int -> Int -> Int
     helper 0 res = res
     helper leftOver res
      | mod leftOver 10 > mod res 10 = helper (div leftOver 10) (res * 10 + (mod leftOver 10))
      | otherwise = res