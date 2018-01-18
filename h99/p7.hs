data NestedList a = Elem a | List [NestedList a]

flatten :: (NestedList a) -> [a]
flatten (Elem n) = [n]
flatten (List [x]) = (flatten x)
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
