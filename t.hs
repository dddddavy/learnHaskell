main = putStrLn "hello world" >> (readLn >>= (\n -> putStrLn(show (n+1))))

data D = C {field1 :: Int, field2 :: Int, field3 :: Int}
    deriving Show

d :: D

d = C {field1 = 1, field2 = 2, field3 = 2}



