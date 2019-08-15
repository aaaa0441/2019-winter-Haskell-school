-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0      = []
  | x < 10      = [x]
  | otherwise   = toDigits(x `div` 10) ++ [x `mod` 10]

reverseIntegers :: [Integer] -> [Integer]
reverseIntegers []       = []
reverseIntegers (x:[])   = [x]
reverseIntegers (x:y)    = reverseIntegers(y) ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverseIntegers (toDigits x)


-- Exercise 2
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft []         = []
doubleEveryOtherFromLeft (x:[])     = [x]
doubleEveryOtherFromLeft (x:y:[])   = [x, y*2]
doubleEveryOtherFromLeft (x:y:zs) = doubleEveryOtherFromLeft([x,y]) ++ doubleEveryOtherFromLeft(zs)
-- TODO: try to use `cycle` & `zipWith` instead

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverseIntegers (doubleEveryOtherFromLeft (reverseIntegers x))


-- Exercise 3
flatten :: [[Integer]] -> [Integer]
flatten []          = []
flatten [[]]        = []
flatten ((x:[]):[]) = [x]
flatten ((x:ys):[]) = x : flatten [ys]
flatten (x:ys)      = flatten [x] ++ flatten ys

sumDigits :: [Integer] -> Integer
sumDigits x = sum . flatten $ map toDigits x


-- Exercise 4
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther $ toDigits x) `mod` 10 == 0
