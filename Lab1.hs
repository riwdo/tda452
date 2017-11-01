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
-- We will test for a few K such as k = 0 to test our guards. We will then test for k's up to around 10 to get quite a good certainty for our power functions.
-- B

prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n absK == power1 n absK) && (power n absK == power2 n absK)
  where absK = abs k
-- C
test_cases = [(prop_powers 4 0) , (prop_powers 4 1), (prop_powers 4 2),(prop_powers 4 3) , (prop_powers 4 4), (prop_powers 4 5),(prop_powers 4 6) , (prop_powers 4 7), (prop_powers 4 8),(prop_powers 4 9) , (prop_powers 4 10), (prop_powers 4 11)]
prop i = i
test = map prop test_cases
