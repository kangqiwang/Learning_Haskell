import Prelude
main=do
enumFromTo :: Int -> Int -> [Int] -- construct a list of integers from m to n
enumFromTo m n 
| m > n = [] 
| m <= n = m : enumFromTo (m+1) n
factorial :: Int -> Int -- as enum, but multiply instead of cons
factorial n = fact 1 n
 where -- to introduce helper function, etc
	fact :: Int -> Int -> Int
	fact m n | m > n = 1 | m <= n = m * fact (m+1) n
enumFrom :: Int -> [Int] -- Count forever!
enumFrom m = m : enumFrom (m+1)