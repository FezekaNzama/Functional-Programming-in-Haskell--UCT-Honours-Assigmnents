halve ns | even(length ns ) = (fronthalf ns (div(length ns) 2), backhalf ns (div(length ns) 2))

fronthalf ns 0 = []
fronthalf ns a = [head ns] ++ fronthalf (tail ns) (a-1) 

backhalf ns 0 = []
backhalf ns a = backhalf (init ns) (a-1) ++ [last ns]