splitAction 0 xs = []
splitAction n xs = splitAction (n-1) xs ++ [(take n xs, drop n xs)] 

split xs = splitAction (length xs-1) xs

