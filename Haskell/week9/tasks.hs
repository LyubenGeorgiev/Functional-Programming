import Data.List
import System.IO

myMin x y =
    if x < y
        then x
        else y

myMax x y =
    if x > y
        then x
        else y

average x y = (x + y) / 2

sumCubes x y = x^3 + y^3

-- solve without using lists;
inside a b x = x >= (myMin a b) && x <= myMax a b
-- solve with lists and where-clause;

-- solve using a lambda of x <- how would this be done in Racket?.


-- print $ average 5 6 == 5.5
-- print $ sumCubes 5 1 == 126
-- print $ sumCubes 10 50 == 126000
-- print $ inside 5 1 4 == True
-- print $ inside 10 50 20 == True
-- print $ inside 10 50 1 == False

factList = 1 : zipWith (*) [2..] factList
-- print $ fact 11 == 39916800
-- print $ factXs 11 == 39916800
-- print $ factIter 11 == 39916800


-- print $ fib 11 == 89
-- print $ fibIter 11 == 89
-- print $ fibIter 110 == 43566776258854844738105
fibList = 1 : 1 : [a + b | (a, b) <- zip fibList (tail fibList)]