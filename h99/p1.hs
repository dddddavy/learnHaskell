myLast :: [a] -> a
myLast [] = error "No match"
myLast [x] = x
myLast (_:xs) = myLast xs

