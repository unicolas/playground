module Course.Hw01.Validator (validate) where

-- | Converts positive Integers to a list of digits.
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- | Converts positive Integers to a reversed list of digits.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- | Double the value of every second digit beginning from the right.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = reverse $ leftDoubleEveryOther $ reverse lst

-- | Double the value of every second digit beginning from the left.
leftDoubleEveryOther :: [Integer] -> [Integer]
leftDoubleEveryOther [] = []
leftDoubleEveryOther [x] = [x]
leftDoubleEveryOther (x:y:xs) = x : y * 2 : leftDoubleEveryOther xs

-- | Calculates the sum of all digits.
sumDigits :: [Integer] -> Integer
sumDigits lst = sum $ map (sum . toDigits) lst

-- | Indicates whether an Integer could be a valid credit card number.
validate :: Integer -> Bool
validate n = checksum n == 0
  where
    checksum x = sumDigits (doubleEveryOther $ toDigits x) `mod` 10
