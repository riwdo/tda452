import Prelude hiding ((++),length,sum,reverse,take,drop,splitAt,zip,unzip)
import qualified Prelude

import Test.QuickCheck

null :: [a] -> Bool
null [] = True
null _ = False

length [] = 0
length (x:xs) = length xs + 1



sum :: Num a => [a] -> a
sum [] = 0;
sum (n:ns) = n + sum ns

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x: (xs++ys)

cons x xs = x:xs
snoc xs x = xs ++ [x]

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

take :: Int -> [a] -> [a]
take 0 xs = []
take n [] = []
take n (x:xs) = x:take (n-1) xs

--len

--sum

--(++) :: [a] -> [a] -> [a]
--

--cons x xs =
--snoc xs x =

--reverse :: [a] -> [a]
-- complexity? how to make it more efficient?

-- | Take the first n elements of a list
--take :: Int -> [a] -> [a]


--prop_take n xs =


-- | Discard the first n elements of a list
--drop :: Int -> [a] -> [a]

--prop_take_drop n xs =

--nonprop_take_drop n xs =


-- | splitAt n xs = (take n xs,drop n xs)
--splitAt :: Int -> [a] -> ([a],[a])

-- | Combine a pair of list into a list of pairs
--zip :: [a] -> [b] -> [(a,b)]

-- | Split a list of pairs into a pair of lists
--unzip :: [(a,b)] -> ([a],[b])


--prop_zip_unzip :: (Eq a,Num a) => [(a,a)] -> Bool
--prop_zip_unzip xys =

--prop_unzip_zip xs ys


-- | "Quicksort"
--qsort :: Ord a => [a] -> [a]


-- | insert a new element at the right position in a sorted list
--insert :: Ord a => a -> [a] -> [a]

--prop_insert x xs

-- | Insertion sort (sort a list by using insert)
--isort :: Ord a => [a] -> [a]


--prop_qsort xs =
