-- skips

elementAt :: Int -> [a] -> a
elementAt 1 (x:xs) = x
elementAt n (_:xs) = elementAt (n-1) xs

getAllKth :: Int -> [a] -> [a]
getAllKth k xs
    | (length xs) < k = []
    | otherwise = (elementAt k xs): getAllKth k (drop k xs)

skips :: [a] -> [[a]]
skips [] = []
skips xs = skipsCall 1 (length xs) xs

skipsCall :: Int -> Int -> [a] -> [[a]]
skipsCall l r xs
    | l > r = []
    | otherwise = (getAllKth l xs): skipsCall (l+1) r xs


-- localMaxima

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
    | (a < b) && (b > c) = b: localMaxima (b:c:xs)
    | otherwise = localMaxima (b:c:xs)
localMaxima _ = []


-- histogram
countNum :: Integer -> [Integer] -> Integer
countNum _ [] = 0
countNum a (x:xs)
    | a == x = 1 + countNum a xs
    | otherwise = countNum a xs

countAllNum :: Integer -> [Integer] -> [Integer]
countAllNum 9 xs = [countNum 9 xs]
countAllNum n xs = (countNum n xs): countAllNum (n+1) xs

genLine :: [Integer] -> String
genLine xs
    | null xs = []
    | otherwise = (if head xs >= 1 then '*' else ' '): genLine (drop 1 xs)

reduceOne :: Integer -> Integer
reduceOne 0 = 0
reduceOne n = n - 1

generateString :: [Integer] -> String
generateString xs
    | sum xs == 0 = []
    | otherwise = (generateString (map reduceOne xs)) ++ "\n" ++ (genLine xs)

histogram :: [Integer] -> String
histogram xs = 
    (generateString (countAllNum 0 xs)) ++ "\n0123456789\n"

