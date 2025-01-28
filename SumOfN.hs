import Debug.Trace

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
sumOfN :: (Eq t, Num t) => t -> t
sumOfN 0 = 0
sumOfN n = n + sumOfN(n-1)

add x y = x + y

inc x = x + 1

len [] = 0
len (x:xs) = 1 + len xs

len1 xs = if null xs 
    then 0 
    else 1 + len1 (tail xs)

len2 xs 
    | null xs = 0
    | otherwise = 1 + len2(tail xs)

add1 x y = x + y

incMaker x = (\ y -> x + y)
inc1 = incMaker 1

incList xs = map inc1 xs

evenList n = n mod 2 == 0

filterEven xs = filter evenList xs

sumList xs = foldr add1 0 xs

mul1 x y | trace ("Mul called with: " ++ show x ++ " " ++ show y) False = undefined
mul1 1 x = x
mul1 x 1 = x
mul1 x y = x + mul1 x (y - 1)