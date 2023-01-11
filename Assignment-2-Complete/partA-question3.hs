sort:: [Int]->[Int]

sort [] = []
sort (x:xs) = sort lower ++ [x] ++ sort higher
                where 
                    lower = [y|y<-xs, y<=x]
                    higher = [y|y<-xs, y>x]