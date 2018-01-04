{-
how to input the martix:
solver [[a]]
-}

import Data.Matrix

import Control.Monad

--get maxtrix
--getH :: [[a]] -> Matrix a
getH a = fromLists a

--getMax as list,
--getMax :: [[Float]] -> [Float]
getMax a = map (multimatrix matrixh ) ( getX $ length a)
		where
			matrixh = getH a
-- first function to get input and output
--solver :: [[Float]] -> [Int]
solver a n = (getX n )!! (snd $ maximum $ zip  (getMax a) [0 .. ] )
--multimatrix :: Matrix ->[b] -> Matrix
multimatrix matrixh xo =  getmat(xt * matrixh * xy)
	where
		xt=  fromList 1  (length xo) xo
		xy= transpose $ fromList 1  (length xo) xo
--convert the matrix to int
getmat=getElem 1 1
--get the list of the [0,1]
--getX :: Int -> [[Int]]
getX n = replicateM n [0,1]

main = do
	
	putStrLn "Please, input your N*N martix"
	
	