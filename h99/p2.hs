myButLast :: [a] -> a
myButLast [] = error "element dismatch"
myButLast [x] = error "element dismatch"
myButLast [x, y] = x
myButLast (_:xs) = myButLast xs
