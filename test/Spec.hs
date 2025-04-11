import Test.QuickCheck
import Data.Bool ()
import Data.List (sort)

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

-- test suite (this all counts as one test though based on the number of OKs, you can see all the properties being tested)
main :: IO ()
main = do
     quickCheck (prop_sort :: [Int] -> Bool)
     quickCheck (prop_sort :: [String] -> Bool)
     quickCheck (prop_append :: [Int] -> Int -> Bool)
     quickCheck (prop_append :: [String] -> String -> Bool)
     quickCheck (prop_append :: [Bool] -> Bool -> Bool)