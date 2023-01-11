f:: String -> String

checkEquality x y | x==y = "a"
                  | otherwise = "b"

f [] = "a"
f ns |  length(ns)==1 = "a"
     |  checkEquality (head ns) (last ns) == "a" = f (tail(init ns))
     |  otherwise = "b"