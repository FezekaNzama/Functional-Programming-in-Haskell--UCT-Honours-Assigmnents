data Op = Add | Mul
    deriving Show
data Expr = Val Int |App Op Expr Expr
    deriving Show

splitAction 0 xs = []
splitAction n xs = splitAction (n-1) xs ++ [(take n xs, drop n xs)] 
split xs = splitAction (length xs-1) xs

combine l r = [ App o l r | o <- [Add, Mul]]

exprs:: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns
            , l <- exprs ls
            , r <- exprs rs
            , e <- combine l r]


