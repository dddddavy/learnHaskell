data Node = Single Char | Multiple Int Char
    deriving Show

decodeModified :: [Node] -> String
decodeModified [] = []
decodeModified ((Single x):xs) = x: decodeModified xs
decodeModified ((Multiple i x):xs) = (take i (repeat x)) ++ decodeModified xs
