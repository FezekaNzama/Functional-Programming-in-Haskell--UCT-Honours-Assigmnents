data Op = Add | Mul
    deriving Show
data Expr = Val Int |App Op Expr Expr
    deriving Show

delete :: Int -> [Int] -> [Int]
delete n [] = []
delete n (x:xs) | x == n = xs
                | otherwise = [x] ++ delete n xs

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms ps = [x:xs | x<-ps, xs <-perms (delete x ps)]

eval:: Expr -> Int
eval (Val n) = n
eval (App Add x y) = eval x + eval y
eval (App Mul x y) = eval x * eval y

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

solveStep ns = [exprs x | x <- perms ns]

solve ns n = [x | x <- concat (solveStep ns), eval(x)==n]