pack :: String -> [String]
pack [] = []
pack (x:xs) = (takeWhile (==x) (x:xs)) : pack (dropWhile (==x) xs)
