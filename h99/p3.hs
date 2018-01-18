elementAt :: [a] -> Integer -> a
elementAt [] x = error "not"
elementAt (x:xs) 1 = x
elementAt (_:xs) n
    | n <= 0 = error "sb"
    | otherwise = elementAt xs (n-1)

