import Data.List

data Matrix a = Matrix [[a]]
              deriving (Eq)

multi :: Num a => [[a]] -> [[a]] -> [[a]]
multi = zipWith (zipWith (*))

instance Num a => Num (Matrix a) where
    (Matrix a) * (Matrix b) = Matrix $ multi a b
    (-)                     = undefined
    (Matrix a) + (Matrix b) = undefined
    negate                  = undefined
    abs                     = undefined
    signum                  = undefined
    fromInteger             = undefined

instance Show a => Show (Matrix a) where
    show (Matrix a) = intercalate "\n" $ map (intercalate " " . map show) a