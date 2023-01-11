last1 :: [a] -> a

last1 ns | length ns>1 = last1 (tail ns)
         | length ns == 1 = head ns
        