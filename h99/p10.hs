encode :: String -> [(Int, Char)]
encode [] = []
encode (x:xs) = (length (takeWhile (==x) (x:xs)), x) : encode (dropWhile (==x) xs)
