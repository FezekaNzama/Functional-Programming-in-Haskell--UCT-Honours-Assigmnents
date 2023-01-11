data Op = Add | Mul
data Expr = Val Int |App Op Expr Expr

eval :: Expr -> Int
values :: Expr -> [Int]

eval (Val n) = n
eval (App Add x y) = eval x + eval y
eval (App Mul x y) = eval x * eval y

values (Val n) = [n]
values (App _ x y) = values x ++ values y
