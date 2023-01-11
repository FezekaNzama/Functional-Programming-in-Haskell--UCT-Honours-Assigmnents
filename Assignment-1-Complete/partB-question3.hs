delete :: Int -> [Int] -> [Int]

delete n [] = []
delete n (x:xs) | x == n = xs
                | otherwise = [x] ++ delete n xs

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms ps = [x:xs | x<-ps, xs <-perms (delete x ps)]



