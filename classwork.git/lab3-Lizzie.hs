-- Lab03.hs
-- Names: Lizzie Siegle
--
--
-- Read the problems below.
-- When you are done, upload your work in the lab03 directory at our
-- classwork repo, at https://github.com/bmc-cs380/classwork
-- Please rename the file before upload so you don't clobber your
-- classmates' work!

{-# LANGUAGE GADTSyntax #-}

module Lab03 where
import Test.HUnit

{- 1. Peer Review

   Taking turns, each member of your group should seek peer review about their
   solutions to hw01. Although scrutiny of any of hw01 is a good idea, make
   sure to discuss at least these functions:
     - maximumTableArea
     - containsStr
     - merge
     - mergeSorted

   Compare different styles and approaches to the problems. How did the others
   figure out the answers? Do any of you have lingering questions?

   During this process, remember that everyone in this class has a different
   level of experience and be supportive of one anothers' challenges. This is
   not a time to show off!

   I expect this will take at least 20 minutes to get through everyone's work.
-}

{- 2. List exercises

   Complete the List comprehension exercises from our syllabus page on the
   website (in the row for class 4).

   If you're having fun here, look up Project Euler problem #4, which is
   also solvable using a list comprehension.

-}

{- 3. Library functions

   Look up the following library functions on Hoogle to find their types and
   definitions. Then, reimplement them yourself.

     - mapMaybe
     - find
     - lookup
     - maybeToList
     - concatMap

-}
--MAPMAYBE
mapMaybe::(a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = [] 
mapMaybe f (x:xs) =
  let mapmayb = mapMaybe f xs in
  case (f x) of
    Nothing -> mapmayb
    Just y -> y:mapmayb
mapmaybe_tests = "mapMaybe" ~: TestList [
    "(1,11),(2,22).." ~: mapMaybe (`lookup` [(1,11),(2,22),(3,33)]) [1,2,11] ~?= [11,22]
    , mapMaybe (sq 10) [0, 1, 2] ~?= [1, 10, 100]
    , mapMaybe (sq (-5)) []        ~?= []
    , mapMaybe (plus1)[5, 4, 3, 2] ~?= [6, 5, 4, 3]
    --, "+1" ~: mapMaybe (1+[5, 10, 15, 20, 25]) ~?= [6, 11, 16, 21, 26]
    --, "sq" ~: mapMaybe ((\y -> y*y) [0, 1, 2, 3]) ~?= [0, 1, 4, 9]
  ]
   where
                     sq 0 _ = Nothing
                     sq a b = Just $ a ^ b
                     plus1 b = Just $ 1 + b

--FIND
find::(a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs)
    | p x       = Just x
    | otherwise = find p xs

find_tests = "find" ~: TestList [ 
    ">3" ~: find (>3)[0, 2, 4, 6, 8] ~?= Just 4
    , "== 3" ~: find (==3)[10, 9, 8, 7, 6, 5] ~?= Nothing
    , "even" ~: find even [2, 4, 6, 8] ~?= Just 2
    , "5**2" ~: find (\x -> 5**x > 10000) [2,4,6,8] ~?= Just 6.0 
    ]

--lookup
lookup1::Eq a => a -> [(a,b)] -> Maybe b
lookup1 _ [] = Nothing
lookup1 p ((x, xs) : pairs)
    | p == x    = Just xs
    | otherwise = lookup1 p pairs

lookup1_tests = "lookup1" ~: TestList [ 
    "c" ~: lookup1 'c' [('a',0),('b',1),('c',2)] ~?= Just 2
    , "big c" ~: lookup1 'c' [('a',0),('b',1),('c',2),('a',3),('b',4),('c',5)] ~?= Just 2
    , "Nothing" ~:  lookup1 'f' [('a',0),('b',1),('c',2)]~?= Nothing
    ]


--myMaybeToList_test = "myMaybeToList" ~:
                     --TestList [ myMaybeToList (Just 1) ~?= [1]
                     --         , myMaybeToList (Nothing :: Maybe Int) ~?= ([] :: [Int])
                     --         ]
maybeToList::Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x)  = [x]
maybeToList_tests = "maybetolists" ~: TestList [
    "[]" ~: maybeToList (lookup1 32 [(1,11), (2,22), (3,33) ]) ~?= []
    ,"just1" ~: maybeToList (Just 1) ~?= [1]
    , "c" ~: maybeToList (lookup1 3 [(1,'A'),(2,'B'),(3,'C')]) ~?= "C"
    --, "[44]" ~: maybeToList (lookup1 4 [(4, 44), (5,55), (6,66)]) ~?= "[44]"
    , "" ~: maybeToList (lookup1 32 [(1,'A'),(2,'B'),(3,'C')]) ~?= ""
    ]

-- concatMap
concatMap1::(a -> [b]) -> [a] -> [b]
concatMap1 _ [] = []
concatMap1 f xs = concat $ map f xs

concatMap1_tests ="concatMap_tests" ~: TestList [
    "tuple" ~: concatMap1 (\x -> [(x,x+2,x/2)]) [1,3,5] ~?= [(1.0,3.0,0.5),(3.0,5.0,1.5),(5.0,7.0,2.5)]
    , "enum" ~: concatMap1 (enumFromTo 1) [1,3,5] ~?= [1, 1, 2, 3, 1, 2, 3, 4, 5]
    ]

{- 4. Binary trees -}

-- Here is a binary tree type:
data Tree a where
  Leaf :: Tree a
  Node :: a      -- data
       -> Tree a -- left child
       -> Tree a -- right child
       -> Tree a
  deriving (Eq, Show)

-- Write a function that finds if an element is within a given tree:
elemTree :: Eq a => a -> Tree a -> Bool
elemTree _ Leaf = False
elemTree e (Node curr r l)
    | e == curr = True
    | otherwise = elemTree e l || elemTree e r
elemTree_tests ="elemTree" ~: TestList [
    elemTree 3 Leaf ~?= False
    , elemTree 3 (Node 3 Leaf Leaf) ~?= True
    , elemTree 2 (Node 1 Leaf Leaf) ~?= False
    , elemTree 5 (Node 1 (Node 5 Leaf Leaf) Leaf) ~?= True
    , elemTree 4 (Node 1 (Node 3 Leaf Leaf) Leaf) ~?= False
    , elemTree 8 (Node 8 (Node 2 (Node 7 Leaf Leaf) Leaf) Leaf) ~?= True
    ]

-- Write a function that swaps the elements in a tuple in a tree.
-- Note that this does not swap left children with right children.
swapTree :: Tree (a,b) -> Tree (b,a)
swapTree Leaf = Leaf
swapTree (Node (t1, t2) l r) = Node (t2, t1) (swapTree l) (swapTree r)

swapTree_tests = "swapTree" ~: TestList [
    "swapTree" ~: swapTree (Node (5, 'l') Leaf Leaf) ~?= (Node ('l', 5) Leaf Leaf)
    ,"v2" ~: swapTree (Node ("lol", "hmm") Leaf Leaf) ~?= (Node ("hmm", "lol") Leaf Leaf)
    , "types" ~: swapTree (Leaf :: Tree (Char, Double)) ~?= (Leaf :: Tree (Double, Char))
    ]

-- Write a function that computes the depth of the tree: that is,
-- the largest number of Nodes to traverse on the way to a Leaf.
depth :: Tree a -> Int
depth Leaf = 0
depth (Node curr l r) = 1 + max (depth l) (depth r)
depth_tests = "depth" ~: TestList [
   "depth" ~: depth (Node (5, 'l') Leaf Leaf) ~?= 1
   ,"v2" ~: depth (Leaf :: Tree (Char, Double)) ~?= 0
   , "v3" ~: depth (Node 8 (Node 2 (Node 7 Leaf Leaf) Leaf) Leaf) ~?= 3
   ]