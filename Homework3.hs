module Homework3 where

import Control.Monad.State      -- State, runState, get, put, guard
import Control.Concurrent       -- threadDelay
import Data.Char                -- toUpper

type Name = String
type FamilyTree = [(Name,Name)]

tree = [
  ("Animal", "Object"),
  ("Cat","Animal"),
  ("Dog","Animal"),
  ("Siamese","Cat"),
  ("Calico","Cat"),
  ("Labrador","Dog"),
  ("Pug","Dog"),
  ("Book","Object"),
  ("Garbage","Can")
  ]

parent :: Name -> FamilyTree -> Maybe Name
parent n [] = Nothing
parent n ((c,p):ts)
	| (n == c) = Just p
	| otherwise = parent n ts

fromJust :: Maybe a -> a
fromJust Nothing = error "In fromJust"
fromJust (Just a) = a

ancestors :: Name -> FamilyTree -> Maybe [Name]
ancestors "" t = Nothing
ancestors a t = do
	if (parent a t == Nothing && a /= "Object")
		then Just ((fromJust (parent a t)) : lst)
	else
		if (a == "Object")
			then Just (a : lst)
		else
			Just ((fromJust (parent a t)) : (fromJust(ancestors (fromJust (parent a t)) t)))
	where lst = []

isSubsequence::(Eq a) => [a] -> [a] -> Bool
isSubsequence [] b = True
isSubsequence a [] = False
isSubsequence (a:as) (b:bs)
	| (a==b) = isSubsequence as bs
	| otherwise = isSubsequence (a:as) bs

countLength ::  (Eq a) => [a] -> [a] -> Int
countLength a (b:bs)
	| (isSubsequence a (b:bs) == True) = countLength a bs
	| otherwise =   length (b:bs)

indexOf :: (Eq a) => [a] -> [a] -> Maybe Int
indexOf [] a = Just 0
indexOf a [] = Nothing
indexOf a b
	| (isSubsequence a b == True) = Just (countLength a b)
	| otherwise = Nothing

headMaybe :: [a]-> Maybe a
headMaybe [] = Nothing
headMaybe (a:as) = Just a

geometric :: Int -> Int -> [Int]
geometric s f =  do s : geometric (s*f) f

divisors :: Int -> [Int]
divisors n = do 
	v <- [1..n]
	guard $ n `mod` v == 0
	return v

mersennes :: [Int]
mersennes = do   --[2^v -1 | v <- [1..], v <= 2^v-1]
	v <- [1..]
	guard $ v <= 2^v-1
	return (2^v-1)

share4way :: Int -> [(Int,Int,Int,Int)]
share4way n = [(p1,p2,p3,p4) | 
								p1 <- [0..n],
								p2 <- [0..n],
								p3 <- [0..n],
								p4 <- [0..n],
								p1 + p2 + p3 + p4 == n
								]

readNum ::IO Int
readNum = do
	putStrLn "Enter A Number: "
	n <- getLine
	return (read n :: Int)

slowEcho :: [String] -> IO ()
slowEcho [] = putStr ""  
slowEcho (l:lst) = do
	putStr l
	threadDelay 1000000
	slowEcho lst

allCaps :: String -> String
allCaps [] = []
allCaps (c:cs) =  toUpper c : allCaps cs

crudeDiff :: FilePath -> FilePath -> IO Bool
crudeDiff a b = do
	f1 <- readFile a
	f2 <- readFile b
	return $ allCaps f1 == allCaps f2