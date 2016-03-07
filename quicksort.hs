quicksort :: [a] -> [a]
quicksort [] = []
quicksort (x:n) = quicksort [a | a <- n, a <= x] ++ [x] ++ quicksort [b | b <- n, b > x]
