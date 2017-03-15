import Test.QuickCheck
wurble::Bool -> Int
wurble b = 3

frob:: Int -> String -> Bool
frob i s = True
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs  = [y | y <- xs, y <= x]
          rhs  = [z | z <- xs, z > x]
 

f2 [] = []
f2 (x:xs) = x + 1 : f2 xs

f3 [] = []
f3 (x:xs)
    | even x = f3 xs
    | otherwise = x : f3 xs

f4 [] = []
f4 (x:xs)
    | even x = div x 2 : f4 xs
    | otherwise = f4 xs

f5 [] = [] 
f5 (x:xs) 
    | even y = y : f5 xs
    | otherwise = f5 xs
    where
    	y = div x 2
f6 [] = Nothing
f6 (x: _) = Just x

f7 Nothing = 0
f7 (Just n) = n

f8 [] = False
f8 _ = True

f9 [] = []
f9 (x : xs) = xs : x


dup xs = [y | x <- xs, y <- [x, x]]
dup2:: [a] -> [a]
dup2 = concatMap (\x -> [x, x])
dup3:: [a] -> [a]
dup3 = concatMap (replicate 2)
isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)
isOrdered _ = True
prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered = isOrdered . qsort
main = quickCheck prop_revapp