main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True

type Rat = (Int, Int)

normalize :: Rat -> Rat
normalize (x, 0) = error "can't divide by zero"
normalize (x, y) = let d = gcd x y in (div x d, div y d)

sumRats :: Rat -> Rat -> Rat
sumRats (_, 0) _ = error "can't divide by zero"
sumRats _ (_, 0) = error "can't divide by zero"
sumRats (x1, y1) (x2, y2) = normalize (x1*y2 + x2*y1, y1*y2)

multiplyRats :: Rat -> Rat -> Rat
multiplyRats (x1, y1) (x2, y2) = normalize (x1*x2, y1*y2)

divideRats :: Rat -> Rat -> Rat
divideRats (x1, y1) (x2, y2) = multiplyRats (x1, y1) (y2, x2)

areEqual :: Rat -> Rat -> Bool
areEqual (x1, y1) (x2, y2) = x3 == x4 && y3 == y4
 where
     (x3, y3) = normalize (x1, y1)
     (x4, y4) = normalize (x2, y2)