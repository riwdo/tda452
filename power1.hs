power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

power1 n k | k < 0 = error "power: negative argument"
power1 n k | k > 0 = product (replicate k n)

power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k = if even k then power2 (n*n) (div k 2) else n * (power2 (n) (k-1))
