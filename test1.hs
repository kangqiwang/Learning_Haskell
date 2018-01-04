import Data.Matrix
import Control.Monad

--getX :: Int -> Matrix Integer
--getX n y =submatrix 1 n y y $ fromLists

getlist n=  replicateM n [0,1]

--maxn n xs = map snd . take n . reverse . sort $ zip xs [0..]


maxIndex list = list !!(  snd $ maximum $ zip list [0 .. ] :: Int)

--getanswer n list = list !! n

--slove a = a !! maxIndex a

--getX n y = map (f y) $ getlist n
	--where f ns = ns !!(y-1)
	--where
		--colsta=loop acc (1: n)
		--colend=loop acc (1: n)

--getfirst :: [Int] ->Int
--getfirst (x:xs) = map (getfirst) Int



--map(fromLists) doublelist
	--		where doublelist = 

