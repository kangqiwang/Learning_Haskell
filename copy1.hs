import System.IO


--inputList [] = []
--inputList(x:xs) = 1:inputList xs


getList n = [1..n]
--ns是输入的矩阵尺寸

xList [] = [[]]
xList(x:xs) = map (1:) yss ++ map (0:) yss --list all possiblities of x list
   where yss = xList xs

data Matrix = H Int Int Double deriving (Show)--define the matrix type 

getL (H l k value) = l
getK (H l k value) = k
getValue (H l k value) = value

zeroMatrix 0 = []
zeroMatrix n = [(H l k value) | l<-[1..n] , k<-[1..n], value<-[0]]--define a matrix that all elements are 0


--valueMatrix (x:xs) (y:ys) = (x+y):valueMatrix xs ys
valueMatrix _ [] = []
valueMatrix [] _ = []
valueMatrix (x:xs) (y:ys) = (H (getL) (getK) x) : valueMatrix xs ys
--xs是赋值的数组，ys是0数组

getMatrix [] = []
getMatrix xs = valueMatrix xs ys
     where ys = zeroMatrix n
--xs 就是最后有坐标的赋值数组 


main = do 
     putStrLn "Hello! please input size number N of matrix:(N must be a int number) "
     a<-getLine
     let b = xList $ getList $ read a + 0 --all possiblities of x list
     --let c = zeroMa trix $ read a + 0-- c是一个0数组
     putStrLn "Hello! please input a N*N size matrix:"
     putStrLn "For example: it is a 2*2 matrix, just input [1,-2,0,1]"
     putStrLn "Or, it is a 3*3 matrix, input as [1,-2,0,1,1,-2,0,1,3]"
     --d<-getLine
     --let e = 
     --let f = 
     


     return(b)