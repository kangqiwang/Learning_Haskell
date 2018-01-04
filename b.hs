{-
how to run 
:ghci
:load b.hs
solve [[a]]

---------[[a]] is the matrix you need to input,will output the answer
the ,matrix must be the N*N matrix
-}
------------------------------------------------------------------------
import Control.Monad (replicateM)
---caculate the formula and get max
solveXHX :: [[Float]] -> [Int] -> Float
solveXHX h x = solveXHXHlpr x h 0 0 (length x) 0
  where    
    solveXHXHlpr :: [Int] -> [[Float]] -> Int -> Int -> Int -> Float -> Float    
    solveXHXHlpr x h i j n acc = 
         case i == n-1 && j == n-1 of 
            True   -> acc + (h!!i!!j) * fromIntegral (x!!i) * fromIntegral (x!!j)
            _      -> case i < n-1 of 
                True -> solveXHXHlpr x h (i+1) j n (acc + (h!!i!!j) * fromIntegral (x!!i) * fromIntegral (x!!j))
                _    -> solveXHXHlpr x h 0 (j+1) n (acc + (h!!i!!j) * fromIntegral (x!!i) * fromIntegral (x!!j))   

--getXs is for [0,1....] matrix
--you can debug the function
getXs :: Int -> [[Int]]
getXs n = replicateM n [0,1]  

--index for helping find the max and get index ,return the answer
findIndex :: (Eq a) => [a] -> a -> Int
findIndex l x = findIndexHlpr l x 0
   where
    findIndexHlpr :: (Eq a) => [a] -> a -> Int -> Int
    findIndexHlpr [] _ _      = -1
    findIndexHlpr (h:t) x acc = 
                case x == h of
                   True -> acc 
                   _    -> findIndexHlpr t x (acc+1)
    
--solver 
solve :: [[Float]] -> [Int]
solve h = case m == n of
        False -> error "H matrix is not a square matrix!" 
        _     -> xs!!(findIndex rs (maximum rs))
       where 
          m  = length h
          n  = length (h!!0)    
          xs = getXs n    
          rs = map (solveXHX h) xs          