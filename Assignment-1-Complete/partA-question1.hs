product1 :: [Int] -> Int

product1 [] = 1
product1 ns = head ns * product(tail ns)
