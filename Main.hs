funProfit :: [Int] -> Int
funProfit []     = 0
funProfit (x:xs) = funAux (x, 0, (x:xs))

funAux :: (Int, Int, [Int]) -> Int                   
funAux (_ , profit    ,[]) = profit                     
funAux (y, profit, (x:xs)) = funAux ((min x y), (max (x - y) profit), xs)

main = interact $ show . funProfit . map read .  words




