import Debug.Trace

dropList _ [] = []
dropList 0 xs = xs
dropList n xs
    | n > 0 = dropList (n-1) (tail xs) 
    | otherwise = xs -- If negative number, return original.

splitLists _ [] = ([], [])
splitLists n (x:xs)
    | n < 0 = ([], []) -- Handle negative case
    | n == 0 = ([x], xs) -- Make first element apart of left if 0
    | otherwise = (\ (left, right) -> (x:left,  right)) -- Lambda to keep left and right in scope, add head to left each recursion
    (splitLists (n-1) xs) -- Recursive step

zipLists [] [] = [(), ()]
