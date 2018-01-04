import qualified Data.Vector as V
data Matrix a = M {
   nrows     :: {-# UNPACK #-} !Int -- ^ Number of rows.
 , ncols     :: {-# UNPACK #-} !Int -- ^ Number of columns.
 , rowOffset :: {-# UNPACK #-} !Int
 , colOffset :: {-# UNPACK #-} !Int
 , vcols     :: {-# UNPACK #-} !Int -- ^ Number of columns of the matrix without offset
 , mvect     :: V.Vector a          -- ^ Content of the matrix as a plain vector.
   }
transpose :: Matrix a -> Matrix a
transpose m = matrix (ncols m) (nrows m) $ \(i,j) -> m ! (j,i)
