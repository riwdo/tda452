import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1
-- Answer: k+1 steps

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k == 0 = 1
power1 n k | k < 0 = error "power: negative argument"
power1 n k | k > 0 = product (replicate (fromInteger k) n)

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n k | k == 0 = 1
power2 n k = if even k then power2 (n*n) (div k 2) else n * (power2 n (k-1))


-- Part 4
-- A
-- Test cases: test against arbitrary letter because we don't wan't the program to work for letters.
-- Test against float and decimal numbers to see that it doesn't work for those.
-- Since our functions are not defined for negative inputs we wan't to assert this by feeding negative numbers to check that an error occurs.
-- Finally test for a few positive numbers and compare the results between the three functions to assert that the answer is correct.
-- B

prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (n^absK == power1 n absK) && (n^absK == power2 n absK)
  where absK = abs k
-- C
