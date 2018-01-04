import Data.Matrix
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = ((xs !! (n-1)), [ x | (i, x) <- zip [1..] xs, i /= n ])

removeAt' :: Int -> [a] -> (Maybe a, [a]) -- safe (using Just/Nothing)
removeAt' _ [] = (Nothing, [])
removeAt' 1 (x:xs) = (Just x, xs)
removeAt' k (x:xs) = let (a, r) = removeAt' (k - 1) xs in (a, x:r)

--combs :: Int -> [a] -> [[a]]
--combs 0 _  = [ [] ]
--combs n xs = [ fromLists (xs !! i : x) | i <- [0..(length xs)-1],
 --                            x <- combs (n-1) (drop (i+1) xs) ]

-- For the countdown problem, see the code from the lecture

queens :: Int -> [[Int]]
queens n = filter test (generate n)
    where generate 0      = [[]]
          generate k      = [q : qs | q <- [1..n], qs <- generate (k-1)]
          test []         = True
          test (q:qs)     = isSafe q qs && test qs
          isSafe   try qs = not (try `elem` qs || sameDiag try qs)
          sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs

queens' :: Int -> [[Int]]
queens' n = map rev $ queensh n
    where queensh 0       = [[]]
          queensh k       = [q:qs | qs <- queensh (k-1), q <- [1..n], isSafe q qs]
          isSafe   try qs = not (try `elem` qs || sameDiag try qs)
          sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs

subs :: [a] -> [[a]]
subs [] = [ [] ]
subs (x:xs) = yss ++ map (x:) yss
	where yss = subs xs
	
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [ [] ]
perms (x:xs) = concat (map (interleave x) (perms xs))
{-
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, -- all number sequences
						e <- exprs ns', -- all expressions
						eval e == [n]] -}