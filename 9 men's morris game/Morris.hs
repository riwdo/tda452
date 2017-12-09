import Data.Maybe
-- Define Morris board

data HandMan = Empty | Add Man HandMan deriving (Show, Eq)

data Man = Man deriving (Show, Eq)

newtype Morris = Morris {rows :: [[Maybe Man]]}
  deriving (Show, Eq)

startingMorris = Morris [[n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]
                        ,[n,n,n,n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]]
                        where n = Nothing
                              m = Man

                        
