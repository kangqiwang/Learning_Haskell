findMaxIndex :: [[Int]] -> (Int, Int)
findMaxIndex m = snd $ maximum $ concat $ zipWith (\i -> flip zip (zip [i,i..] [0..])) [0..] m