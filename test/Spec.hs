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

-- enumeration for finite number of colors 
data NamedColor
    = Red | Blue | Green | Yellow | Purple | Black
    deriving (Show, Enum, Eq, Bounded)

-- Data type representing three possible color formats:
--   - Named (from the NamedColor enumeration)
--   - RGB (Red, Green, Blue values: 0-255)
--   - CMYK (Cyan, Magenta, Yellow, Black values: 0–255)
data Color
  = Named NamedColor
  | RGB Int Int Int
  | CMYK Int Int Int Int
  deriving (Eq, Show)

-- converts a Color value to a packed list of integers:
--   - 0 indicates a NamedColor
--   - 1 indicates an RGB color
--   - 2 indicates a CMYK color
packColor :: Color -> [Int]
packColor (Named c) = 0 : [fromEnum c]
packColor (RGB r g b) = 1 : [r, g, b]
packColor (CMYK c m y k) = 2 : [c, m, y, k]

-- unpacks a list of integers into a Color value if valid
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

-- helper function to check if an individual color component is in proper range 
inRange :: Int -> Bool
inRange x = x >= 0 && x <= 255

-- arbitrary instance for NamedColor: generates any defined NamedColor value (contract-generate)
instance Arbitrary NamedColor where
  arbitrary = elements [minBound .. maxBound]

-- arbitrary instance for Color: generates random values from RGB, CMYK, or NamedColor (contract-generate)
instance Arbitrary Color where 
  arbitrary = oneof
    [ RGB <$> range <*> range <*> range
    , CMYK <$> range <*> range <*> range <*> range
    , Named <$> arbitrary
    ] 
    where range = choose (0, 255)

-- packing and then unpacking a Color should return the original color
prop_inverse_color :: Color -> Bool
prop_inverse_color c = unpackColor (packColor c) == Right c

-- if a list of integers decodes to a valid Color, then re-packing and unpacking it should preserve it
prop_color_cycle :: [Int] -> Bool
prop_color_cycle list =
  case unpackColor list of
    Right c -> unpackColor (packColor c) == Right c
    Left _  -> True

--  Main test-suite
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
