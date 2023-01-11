gf::(Int->Int) -> [Int]->[Int]

gf fn [] = []
gf fn [x] = [fn x]
gf fn (x:y:xs) = [fn x]++ [y] ++ gf fn xs
