data Node = Single Char | Multiple Int Char
    deriving Show

encodeModified :: String -> [Node]
encodeModified [] = []
encodeModified (x:xs) = genNode (takeWhile (==x) (x:xs)) : encodeModified (dropWhile (==x) xs)

genNode :: String -> Node
genNode p@(x:xs)
    | length xs == 0 = Single x
    | otherwise = Multiple (length p) x

