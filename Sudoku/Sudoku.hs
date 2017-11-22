import Test.QuickCheck

-------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 9,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 :: Sudoku
example2 =
        Sudoku
          [ [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
          , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
          , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
          , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
          , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
          , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
          , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
          , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
          ]
      where
        n = Nothing
        j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku sudoku) = length (sudoku) == 9 && row' sudoku


row' :: [[Maybe Int]] -> Bool
row' (x:xs) = element' x && length x == 9

element' :: [Maybe Int] -> Bool
element' (x:xs) | x > (Just 0) && x < (Just 10) = True && element' xs
                | x == Nothing = True && element' xs
                | otherwise = False


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku sudoku) = length (sudoku) == 9 && checkRows sudoku

checkRows :: [[Maybe Int]] -> Bool
checkRows (x:xs) = checkElement x && length x == 9

checkElement :: [Maybe Int] -> Bool
checkElement [x] = error "element be empty"
checkElement (x:xs) | x > (Just 0) && x < (Just 10) = True && checkElement (tail xs)
                | otherwise = False

-------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku sudoku) = do putStrLn(formatSudoku sudoku)

formatSudoku :: [[Maybe Int]] -> String
formatSudoku [] = ""
formatSudoku (x:xs) = getElement x ++  "\n" ++ formatSudoku xs

getElement :: [Maybe Int] -> String
getElement [] = ""
getElement (x:xs) | x > (j 0) && x < (j 10) = case x of Just x -> show x ++ getElement xs
                  | x == n = "." ++ getElement xs
                  where n = Nothing
                        j = Just
-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
      content <- readFile path
      checkContent content

checkContent :: Sudoku -> IO Sudoku
checkContent sudoku
          | isSudoku sudoku = return sudoku
          | otherwise = "no sudoku here"

-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-------------------------------------------------------------------------
