{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import GHC.Generics
import GHC.Enum
import Data.Bool (Bool(..))
import Data.List (sort, deleteBy)
import Prelude 
import qualified Data.ByteString.Lazy as BL
import GHC.Enum (Bounded(minBound, maxBound))
import Data.Text.IO (putStrLn)

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

-- B3
-- Color type data definitions

data NamedColor
    = Red | Blue | Green | Yellow | Purple | Black
    deriving(Show, Enum, Eq, Bounded)

data Color
  = Named NamedColor
  | RGB Int Int Int
  | CMYK Int Int Int Int
  deriving (Eq, Show)


packColor :: Color -> [Int]
packColor(RGB r g b) = 0: [r, g, b]
packColor (CMYK c m y k) = 1 : [c, m, y, k]
packColor (Named c) = 2: [fromEnum c]

unpackColor :: [Int] -> Either String Color
unpackColor (0 : [n])
  | n >= fromEnum (minBound :: NamedColor) && n <= fromEnum (maxBound :: NamedColor)
  = Right $ Named (toEnum n)
  | otherwise = Left "Invalid named color tag"
unpackColor (1 : [r, g, b])
  | all inRange [r, g, b] = Right $ RGB r g b
  | otherwise = Left "RGB values out of range"
unpackColor (2 : [c, m, y, k])
  | all inRange [c, m, y, k] = Right $ CMYK c m y k
  | otherwise = Left "CMYK values out of range"
unpackColor _ = Left "Invalid value tag"


inRange :: Int -> Bool
inRange x = x >= 0 && x <= 255

instance Arbitrary NamedColor where
  arbitrary = elements[minBound .. maxBound]

instance Arbitrary Color where 
  arbitrary = oneof
    [
      RGB <$> range <*> range <*> range
    , CMYK  <$> range <*> range <*> range <*> range
    , Named <$> arbitrary
    ] 
    where range = choose(0, 255) 


prop_inverse_color :: Color -> Bool
prop_inverse_color c = 
    packColor . (unpackColor c) == Right c

prop_color_cycle :: [Int] -> Bool
prop_color_cycle list =
  case unpackColor list of
    Right c -> unpackColor (packColor c) == Right c
    Left _ -> True

-- test suite (this all counts as one test though based on the number of OKs, (you can see all the properties being tested in the terminal)
main :: IO ()
main = do
     quickCheck (prop_sort :: [Int] -> Bool)
     quickCheck (prop_sort :: [String] -> Bool)
     quickCheck (prop_append :: [Int] -> Int -> Bool)
     quickCheck (prop_append :: [String] -> String -> Bool)
     quickCheck (prop_append :: [Bool] -> Bool -> Bool)
     quickCheck (prop_json :: StudentDB -> Bool)
     quickCheck (prop_color_cycle :: [Int] -> Bool)
     quickCheck (prop_inverse_color :: Color -> Bool)