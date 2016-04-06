import Test.QuickCheck

-- power function way ONE
power :: Integer -> Integer -> Integer
power n k  | k<0 = error "power : negative argument"
power n 0        = 1
power n k        = n*power n (k-1) 

-- a function to get steps in calculating a power
powerSteps :: Integer -> Integer -> Integer
powerSteps n k | k<0   = error " steps : negative argument"
powerSteps n k         = k+1

-- calculate a power by using a product function
power1 :: Integer -> Integer -> Integer
power1 n k | k<0      = error "power1 : negative argument"
power1 n 0            = 1
power1 n k            = product (replicate (fromInteger k) n)

-- calculate power by verifyng if a k-value is even or odd
power2 :: Integer -> Integer -> Integer
power2 n k | k<0       = error "power2 : negative argument"
power2 n 0             = 1
power2 n 1             = n
power2 n k | even k    = power2 (n*n) (div k 2)
           | otherwise = n*power2 n (k-1)


-- a function to test the power functions
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = k>0 ==> power n k == power1 n k && power n k == power2 n k