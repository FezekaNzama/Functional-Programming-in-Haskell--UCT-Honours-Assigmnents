delete :: Int -> [Int] -> [Int]

delete n [] = []
delete n (x:xs) | x == n = xs
                | otherwise = [x] ++ delete n xs