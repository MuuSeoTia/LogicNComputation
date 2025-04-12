{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import GHC.Generics
import Data.Bool (Bool)
import Data.List (sort, deleteBy)
import Prelude 
import qualified Data.ByteString.Lazy as BL

-- B1

-- property for testing if a sorted list maintains the same length as its original list
prop_sort :: (Ord a) => [a] -> Bool
prop_sort list = length list == length (sort list)

-- helper function to check if an element is already in a given list 
isMember :: Eq a => a -> [a] -> Bool
isMember _ [] = False
isMember n (x:list)
   | n == x = True
   | otherwise = isMember n list

-- function which appends a given element to list if its not already in it
append :: Eq a => [a] -> a -> [a]
append list element = 
    if isMember element list then list
    else list ++ [element]

-- property which checks if appending an element to a list does not decrease it's size
prop_append :: Eq a => [a] -> a -> Bool
prop_append list e = 
    length (append list e) >= length list

-- B2

-- student structure
data Student = Student 
    {name :: String,
     interests :: [String],
     address :: String
    } deriving (Eq, Generic, Show, ToJSON, FromJSON)

-- Map from student ID to Student
newtype StudentDB = StudentDB { unStudentDB :: [(String, Student)] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Arbitrary Student where
  arbitrary = Student <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary StudentDB where
  arbitrary = StudentDB <$> listOf entry
    where entry = do
            sid <- listOf1 (elements ['a'..'z'])
            student <- arbitrary
            return (sid, student)

-- function to serialize StudentDB
serialize :: StudentDB -> BL.ByteString
serialize = encode

-- function to deseralize 
deserialize :: BL.ByteString -> Maybe StudentDB
deserialize = decode

-- property to check if serialization and deserialization is working properly
prop_json :: StudentDB -> Bool
prop_json student = 
    Just student == (deserialize . serialize) student

-- test suite (this all counts as one test though based on the number of OKs, (you can see all the properties being tested in the terminal)
main :: IO ()
main = do
     quickCheck (prop_sort :: [Int] -> Bool)
     quickCheck (prop_sort :: [String] -> Bool)
     quickCheck (prop_append :: [Int] -> Int -> Bool)
     quickCheck (prop_append :: [String] -> String -> Bool)
     quickCheck (prop_append :: [Bool] -> Bool -> Bool)
     quickCheck (prop_json :: StudentDB -> Bool)