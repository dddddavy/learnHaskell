myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = (myLength xs) + 1
