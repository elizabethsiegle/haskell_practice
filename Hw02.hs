{- CS380 Assignment 2
   Name: Lizzie Siegle
   College email: esiegle@brynmawr.edu
   Resources / collaborators:

   **DUE ON GRADESCOPE BEFORE CLASS ON WEDNESDAY, FEBRUARY 8, 2017.**
-}

{-# LANGUAGE GADTSyntax #-}

module Hw02 where

import Test.HUnit
import qualified Data.Map as Map
import Data.List

--------------------------------------------------------------------------------
-- Binary Search Trees

{- In this assignment, you will be writing a binary search tree implementation
   of a finite map. A finite map is a data structure that associates *keys*
   (of one type) with *values* (of, potentially, another type). This is useful for,
   say, storing an address book or counting word frequencies in a file. Java's
   finite map interface is Map (https://docs.oracle.com/javase/8/docs/api/java/util/Map.html);
   Python calls this type a dictionary.

   Note that this is a *different*, unrelated use of the word "map" than the
   higher-order function that we've seen.

   To keep things simple, our map will use Strings as its *key* type.

   The map will be implemented using a binary search tree.
   https://en.wikipedia.org/wiki/Binary_search_tree

   The tree type will store a String and a value (of type `a`) at each interior
   node. It will obey the usual binary search tree properties. Given a interior
   node T with left child L and right child R:

   1. The key stored in every node in the tree rooted at L is less than T's key.
   2. The key stored in every node in the tree rooted at R is greater than T's key.

   By "less" and "greater" here, I mean by using the operators (<) and (>), which
   work on Strings.

   **** UNIT TESTS ****
   Each function must be tested against at least 4 test cases. Add to mine!
-}

-- The tree datatype:
data Tree a where
  Leaf :: Tree a
  Node :: String  -- key
       -> a       -- value
       -> Tree a  -- left child
       -> Tree a  -- right child
       -> Tree a
    deriving (Eq, Show)   -- this allows (==) on trees and (show :: Tree a -> String)

sampleTree1 :: Tree Int
sampleTree1 = 
  Node "pickle" 5 
    (Node "avocado" 2
      Leaf(Node "clementine" 10
        Leaf Leaf)
      )           
    (Node "tomato" 7
      (Node "radish" 9
        Leaf Leaf)
      (Node "yam" 1
        Leaf   Leaf)
      )

sampleTree2 :: Tree Char
sampleTree2 = 
  Node "Haskell" 'x'
  (Node "C" 'q'
    (Node "Ada" 'p'
      Leaf   Leaf)    (Node "C++" 'r'
      Leaf (Node "F#" 'e'
        Leaf   Leaf)))    
  (Node "OCaml" 'd'
    Leaf     Leaf)

sampleTree3 :: Tree Float
sampleTree3 =
  Node "Steph" 1.0 Leaf Leaf

sampleTree4 :: Tree Char
sampleTree4 = 
  Node "Steve" 'h'
  (Node "Alexis" 'o'
    Leaf(Node "Stephen" 'b'
      Leaf Leaf)
   ) Leaf 
 

--------------------------------------------------------------------------------
-- Problem 1

{- Write a function that gets the size (number of interior nodes) of a tree. -}
sizeTree :: Tree a -> Int
sizeTree Leaf = 0
sizeTree (Node str num l r) = (1 + (sizeTree l) + (sizeTree r))

sizeTree_tests = "sizeTree" ~:
                 TestList [ "sampleTree1" ~: sizeTree sampleTree1 ~?= 6
                          , "sampleTree2" ~: sizeTree sampleTree2 ~?= 6
                          , "sampleTree3" ~: sizeTree sampleTree3 ~?= 1
                          , "sampleTree4" ~: sizeTree sampleTree4 ~?= 3
                          --, "sampleTree5" ~: sizeTree sampleTree5 ~?- 0
                          ]

--------------------------------------------------------------------------------
-- Problem 2

{- Write a function that finds a key in a binary search tree and
   returns the associated value, if there is one. -}

findTree :: Tree a -> String -> Maybe a
findTree Leaf _ = Nothing
findTree (Node k v l r) p 
    | k == p         = Just v
    | k < p          = findTree r p
    | otherwise      = findTree l p
  
findTree_tests = "findTree" ~:
                 TestList [ "pickle" ~: findTree sampleTree1 "pickle" ~?= Just 5
                          , "Java"   ~: findTree sampleTree2 "Java"   ~?= Nothing 
                          , "Curry" ~: findTree sampleTree3 "Steph" ~?= Just 1.0
                          , "PubNubxReddit" ~: findTree sampleTree4 "Alexis" ~?= Just 'o' 
                          , "redditx2" ~: findTree sampleTree4 "Stephen" ~?= Just 'b'
                          ]

--------------------------------------------------------------------------------
-- Problem 3

{- Write a function that inserts a key into a binary search tree. If the key
   is already in the tree, then update the value associated with the key. -}

insertTree :: Tree a -> String -> a -> Tree a
insertTree Leaf k v = Node k v Leaf Leaf
insertTree (Node k v l r) newKey newVal
    | k < newKey    = Node k v l (insertTree r newKey newVal)
    | k > newKey    = Node k v (insertTree l newKey newVal) r
    | otherwise = Node newKey newVal l r

insertTree_tests
  = "insertTree" ~:
    TestList [ "insert/find" ~: findTree (insertTree Leaf "hi" "there") "hi" ~?= Just "there"
             , "update"      ~: findTree (insertTree sampleTree1 "clementine" (-5)) "clementine" ~?= Just (-5)
             , "insert2"     ~: findTree (insertTree sampleTree2 "twilio" 't') "twilio" ~?= Just 't'
             , "update2"     ~: findTree (insertTree sampleTree3 "steph" 1.0) "steph" ~?= Just 1.0
             , "update3"     ~: findTree (insertTree sampleTree3 "steph" 3.0) "steph" ~?= Just 3.0
             --, "update4"     ~: findTree (insertTree sampleTree3 "steph" 1.0) "steph" ~?= Just 1.0
             ] 

--------------------------------------------------------------------------------
-- Problem 4

{- Write a function that maps a function over all the *values* in your tree. -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node k v l r) = (Node k (f v) (mapTree f l) (mapTree f r))

mapTree_tests
  = "mapTree" ~:
    TestList [ "isVowel" ~: findTree (mapTree (`elem` "aeiou") sampleTree2) "F#" ~?= Just True
             , "samesize" ~: sizeTree (mapTree (+1) sampleTree1) ~?= 6 
             , "diffsize" ~: sizeTree (mapTree (`elem` "AEIOUY") sampleTree4) ~?= 3
              , "nums" ~: sizeTree(mapTree(`elem` [1,2,3,4,5,6,7,8,9])sampleTree1) ~?= 6
             ]

--------------------------------------------------------------------------------
-- Problem 5

{- Write a function that returns all the key/value pairs of a tree in preorder.
   That is, return the key/value pair of a node before recurring into its children. -}

preorder :: Tree a -> [(String, a)]
preorder Leaf = []
preorder (Node k v l r) = (k, v): (preorder l) ++ (preorder r)

preorder_tests
  = "preorder" ~:
    TestList [ "sampleTree1" ~: preorder sampleTree1 ~?= [ ("pickle", 5), ("avocado",2)
                                                         , ("clementine", 10), ("tomato", 7)
                                                         , ("radish", 9), ("yam", 1) ]
             , "empty" ~: preorder (Leaf :: Tree Integer) ~?= [] 
             , "sampleTree2" ~: preorder sampleTree2 ~?= [("Haskell",'x'),("C",'q'),("Ada",'p'),("C++",'r'),("F#",'e'),("OCaml",'d')]
             , "sampleTree3" ~: preorder sampleTree3 ~?= [("Steph", 1.0)]
             , "sampleTree4" ~: preorder sampleTree4 ~?=  [("Steve",'h'),("Alexis",'o'),("Stephen",'b')]
             ]
                                    -- HUnit struggles if it doesn't know the type
                                    -- of data stored in a polymophic structure
  -- Add more tests

--------------------------------------------------------------------------------
-- Problem 6

{- Write a function that uses your tree structure to efficiently compute the
   frequencies of words in a list of words. The input to your function is a list
   of words (Strings). The output is an association list associating each word
   with the number of times it occurs in the list. -}

--frequencies :: [String] -> [(String, Int)] --make tree recurse lookup, inserttree

--if extra libraries unallowed, use this one (doesn't pass one of your test cases and !sure why)

frequencies :: [String] -> [(String, Int)]
frequencies xs = [(x, length $ filter (== x) $ xs) | x <- xs]


--frequencies w = Map.toList $ foldl' updateMap (Map.empty :: Map.Map String Int) w
--    where updateMap nummap word = case (Map.lookup word nummap) of
--                                     Nothing -> (Map.insert word 1 nummap)
--                                     Just x  -> (Map.insert word $! x + 1) nummap


frequencies_test
  = "frequencies" ~:
    TestList [ "palindrome a   " ~: lookup "a"    (frequencies words)  ~?= Just 3
             , "palindrome plan" ~: lookup "plan" (frequencies words)  ~?= Just 1
             , "let it go      " ~: lookup "let"  (frequencies words2) ~?= Just 2      
             , "Let it go      " ~: lookup "Let"  (frequencies words3) ~?= Just 1 
             , "LET IT GO      " ~: lookup "G0"   (frequencies words3) ~?= Nothing    
              ]
  where
    words = ["a", "man", "a", "plan", "a", "canal", "Panama"]
    words2 = ["let", "it", "go", "let", "it", "go"]
    words3 = ["Let", "it", "go", "let", "it", "go"]

all_tests = TestList [
    frequencies_test,
    preorder_tests,
    mapTree_tests,
    insertTree_tests,
    findTree_tests,
    sizeTree_tests
    ]
