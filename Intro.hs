{-CS380 Assignment 1
   Name: Lizzie Siegle
   College email: esiegle@brynmawr.edu
   Resources / collaborators:

   **DUE BEFORE CLASS ON MONDAY, JANUARY 30, 2017.**
   See forthcoming email for submission instructions.

   (This assignment is directly inspired by an assignment in CIS120 at UPenn.)
-}
module Intro where

import Test.HUnit
import Data.Ord

{- NOTE: you should _not_ use functions in the Haskell standard libraries,
   especially the ones in the Data.List module, except where they are
   explicitly allowed in the comments. The purpose of this assignment is to
   familiarize you with the basics of Haskell programming, so we want you to
   explicitly write out each of these problems even though there is often a
   built-in function that would achieve the same result. You will not receive
   credit for solutions that are contrary to the spirit of the assignment.

   You MAY use helper functions / other top-level definitions as you wish. -}

--------------------------------------------------------------------------------
-- Problem 1 (counting coins)

{- Your job in this problem is to calculate the smallest number of
   pennies, nickels, and dimes that can be used to add up to the given
   amount. For example, to make 7 cents, a total of 3 coins are
   needed (two pennies and a nickel); to make 99 cents, 14 coins are
   needed (9 dimes, 1 nickel, and 4 pennies). Fill in the body of the
   function 'coins' below so that it returns the right answer. Prefer
   guards over `if`/`then`/`else`. -}
sumHelp::Int -> Int -> Int -> Int
sumHelp x y z = x + y + z
coins :: Int -> Int
coins x = 
  let (d, leftover)    = x `divMod` 10 --divide by 10, pass remainder to be divided by nickels (2nd largest)
      (n, leftover1)   = leftover `divMod` 5
      (p, overflowish) = leftover1 `divMod` 1
  in sumHelp d n p 

-- | cents >= 10 = 1 + coins(cents-10)
-- | cents >= 5  = 1 + coins(cents-5)
-- | cents >= 0 = cents
-- coins cents = (div cents 10) + (div (mod cents 10) 5) + mod cents 5

-- I provide two test cases. You must provide two more.
-- See https://hackage.haskell.org/package/HUnit-1.5.0.0/docs/Test-HUnit-Base.html
-- for the functions to create tests. (For now, treat Assertion and Test as
-- interchangeable.)

coins_tests = "coins" ~:
              TestList [ "coins 7"  ~: coins 7  ~?= 3
                       , "coins 99" ~: coins 99 ~?= 14
                       , "coins 55" ~: coins 55 ~?= 6
                       , "coins 77" ~: coins 77 ~?= 10 ]

--------------------------------------------------------------------------------
-- Problem 2 (geometry)

{- Sometimes street magicians need to use crates as tables in their
   acts.  Given the dimensions of a crate, find the largest surface
   area it can provide when used as a table.

   Hint: Haskell provides built-in max and min functions that take in two
   arguments and behave exactly as you might expect: `max 5 2` returns 5,
   for example.

   Note: The behavior of this function when at least one of the input side
   lengths is <= 0 is undefined. Your function may return any value in this
   case; we will not test this case on submission. -}

minHelp :: (Ord a) => [a] -> a  --find minimum of list
minHelp [x] = x
minHelp (x:xs)
    | x < minHelp xs = x
    | otherwise = minHelp xs
maximumTableArea :: Int -> Int -> Int -> Int
maximumTableArea side1 side2 side3  --brute force? ish.                  
    | minHelp [side1, side2, side3] == side1  = side2*side3
    | minHelp [side1, side2, side3] == side2  = side1*side3
    | minHelp [side1, side2, side3] == side3  = side1*side2
    | otherwise                               = 0
   

maximumTableArea_tests = "maximumTableArea" ~:
                         TestList [ "mta 1 2 3"  ~: maximumTableArea 1 2 3  ~?= 6
                                  , "mta 4 3 3"  ~: maximumTableArea 4 3 3  ~?= 12
                                  , "mta 9 8 7"  ~: maximumTableArea 9 8 7  ~?= 72
                                  , "mta 0 10 9" ~: maximumTableArea 0 10 9 ~?= 90
                                  ]

--------------------------------------------------------------------------------
-- Problem 3 (simulating robot movement)

{- Help a robot move along its track (with spaces numbered 0 through
   99) by calculating its new position when given `dir` (equal to
   "forward" or "backward") and `num_moves` indicating a non-negative
   number of spaces.  Keep in mind that the robot can't move past the
   0 or 99 spot so when it reaches either end it stays there. -}

moveRobot :: Int -> String -> Int -> Int --track, dir, num_moves
moveRobot cur_pos dir num_moves
  
  -- dir forward and check if moving forward num_moves is <= 99 spot
  | dir == "forward" && (cur_pos + num_moves <= 99)      = cur_pos + num_moves
  
  -- dir backward and check if moving backward num_moves is >= 0 spot
  | dir == "backward" && (cur_pos - num_moves >= 0)      = cur_pos - num_moves 

  -- if adding num_moves to cur_pos is out of range, set min or max pos
  | cur_pos - num_moves < 0  || cur_pos + num_moves < 0  = 0
  | cur_pos + num_moves < 99 || cur_pos + num_moves > 99 = 99

moveRobot_tests = "moveRobot" ~:
                  TestList [ "10 forward 3" ~: moveRobot 10 "forward" 3 ~?= 13
                           , "1 backward 4" ~: moveRobot 1 "backward" 4 ~?= 0
                           , "98 forward 3" ~: moveRobot 98 "forward" 3 ~?= 99
                           , "1 backward (-2)" ~: moveRobot 1 "backward" (-2) ~?= 3]

--------------------------------------------------------------------------------
-- Problem 4 (Philadelphia geography)

{- Philadelphia has a fairly logical layout: the numbered streets
   are typically one-way, and their direction is determined by their
   number and where you are in the city.

   Even streets go one way and odd streets go another:

     East of Broad (<14th): even go south, odd go north
     West of Broad (>14th): even go north, odd go south
     West Philly  (>=32nd): even go south, odd go north
     West Philly  (>=46th): two-way

   There are, however, a few exceptions.
     - 1st and 14th do not actually exist as street names -- they're
       called Front and Broad. We'll ignore this and pretend they do.
     - Broad (14th), 25th, 38th, 41st, and 42nd are all two-way.
     - 24th and 59th go south.
     - 58th goes north.

   Write a program that returns one of four string values for each street
   number:
     - "N/A" when the street doesn't exist. We only consider Front
       (=1st) through 69th Streets.
     - "N" when the street goes north.
     - "S" when the street goes south.
     - "NS" when the street is two-way.
     - you might find the infix 'mod' (modulo) function useful:
           (x mod 2)
       evaluates to 0 if x is even and 1 otherwise.
     - sometimes there's no 'simple' way of writing down complex case
       analysis... -}

streetDirection :: Int -> String
twowayLst       = [14, 25, 38, 41, 42] --list of 2-way streets
specialSouthLst = [24, 59] --list of streets that go south, breaking pattern
southLst = [2, 4..12] ++ [15, 17..31] ++ [32, 34..44] --south streets
northLst = [1, 3..13] ++ [16, 18..30] ++ [33, 35..45] --north streets
--check which list street belongs to
streetDirection num
    | num `elem` specialSouthLst || num `elem` southLst = "S"
    | num `elem` twowayLst                              = "NS"
    | num == 58 || num `elem` northLst                  = "N"
    | otherwise                                         = "N/A"

streetDirection_tests = "streetDirection" ~:
                        TestList [ "14" ~: streetDirection 14 ~?= "NS"
                                 , "9"  ~: streetDirection 9  ~?= "N"
                                 , "18" ~: streetDirection 18 ~?= "N"
                                 , "100" ~: streetDirection 100 ~?= "N/A"
                                 , "neg" ~: streetDirection (-2) ~?= "N/A"
                                 , "24" ~: streetDirection 24 ~?= "S" ]

--------------------------------------------------------------------------------
-- Problem 5 (exists)

{- Write a function that determines whether at least one boolean value
   in its input list is true. -}

exists :: [Bool] -> Bool
--check if True is element of list
exists b 
    | (True `elem` b) == True   = True --don't need == True
    | otherwise                 = False
-- afoldl (||) False

exists_tests = "exists" ~:
               TestList [ "FF"  ~: exists [False, False]       ~?= False
                        , "TFT" ~: exists [False, True, False] ~?= True
                        , "FFFTF" ~: exists [False, False, False, True, False] ~?= True
                        , "empty" ~: exists [] ~?= False ]

--------------------------------------------------------------------------------
-- Problem 6 (join)

{- Write a function that takes a list of strings and "flattens" it
   into a single string. This function also takes an additional
   argument, a separator string, which is interspersed between all of
   the strings in the list. -}
join :: String -> [String] -> String --can ++ strings
join xs ys = 
  drop (length xs) --w/o this, String returned w/ separator string at front
  $ concat 
  $ map (\y -> xs ++ y) ys --anonymous func adding ys to xs for each element in ys

join_tests = "join" ~:
             TestList [ ", abc" ~: join "," ["a", "b", "c"] ~?= "a,b,c"
                      , "abc"   ~: join ""  ["a", "b", "c"] ~?= "abc"
                      , "empty" ~: join "," []              ~?= ""
                      , "~ lol" ~: join "~" ["l", "o", "l"] ~?= "l~o~l"
                      , "---- LIZZIe" ~: join "----"["L", "I", "Z", "Z", "I", "e"] ~?= "L----I----Z----Z----I----e" 
                      ]

--------------------------------------------------------------------------------
-- Problem 7 (finding dolls in a toy store)

{- Write a function that checks whether a list of toys contains some
   particular toy. -}

containsStr :: [String] -> String -> Bool
containsStr lstToys toy 
    | (toy `elem` lstToys) == True = True
    | otherwise                    = False

containsStr_tests
  = "containsStr" ~:
    TestList [ "barbie" ~:
               containsStr ["truck", "barbie", "top"] "barbie" ~?= True
             , "woody" ~:
               containsStr ["truck", "barbie", "top"] "woody"  ~?= False
             , "Mulan" ~:
               containsStr ["mulan", "Jessie", "rex"] "Mulan" ~?= False
             , "disney" ~: 
               containsStr ["polly pocket", "steph curry", "dog", "disney"] "disney" ~?= True ]

{- Next, write a function that, given a list of toys and a list of
   dolls, filters the toys list so that only dolls remain. Your
   function should return a list containing all the elements of a
   given list of toy names that appear in a given list of doll
   names. -}

dollsOf :: [String]  -- all toys
        -> [String]  -- dolls
        -> [String]  -- the toys that are dolls
--dollsOf t d = filter (a (`elem` s)) s where

-- return list of elements where element is a toy and is a doll
dollsOf t d = [x | x <- t, containsStr d x]


dollsOf_tests
  = "dollsOf" ~:
    TestList [ "barbie" ~:
               dollsOf ["truck", "barbie", "top"] ["barbie", "woody"]
                 ~?= ["barbie"]
             , "none" ~:
               dollsOf [] ["barbie", "woody"] ~?= []
             , "mulan, tiana, merida, belle" ~: 
             dollsOf["mulan", "merida", "optimus prime", "tiana", "bb8","yoda", "belle"] ["mulan", "merida", "tiana", "belle"] ~?=["mulan", "merida", "tiana", "belle"]
             , "none2" ~: dollsOf["telescope", "trampoline"] [] ~?= [] 
             ]

--------------------------------------------------------------------------------
-- Problem 8 (merging lists)

{- Write a function that merges two input lists into a single list
   that contains all the elements from both input lists in alternating order:
   the first, third, etc. elements come from the first input list and
   the second, fourth, etc. elements come from the second input list.

   The lengths of the two lists may not be the same -- any
   extra elements should appear at the very end of the result. -}

merge :: [a] -> [a] -> [a]
merge x[] = x
merge [] x = x
merge (x:xs) ys = x: (merge ys xs)

merge_tests = "merge" ~:
              TestList [ "1 through 8" ~: merge [1,3,5,7] [2,4,6,8] ~?= [1..8]
                       , "empty list"  ~: merge [1,2,3]   []        ~?= [1,2,3]
                       , "even -" ~: merge [(-2),(-6),(-10)] [(-4),(-8)] ~?= [(-2),(-4),(-6),(-8),(-10)]
                       , "odd - +1"  ~: merge [(-1),(-5)] [(-3)] ~?= [(-1),(-3),(-5)]]

--------------------------------------------------------------------------------
-- Problem 9 (is_sorted)

{- Write a function that determines whether a given list of integers
   is SORTED -- that is, whether the elements appear in ascending
   order. It is okay if the list has repeated elements, so long as they
   are next to each other.
--| length x == 1   = True
    --| otherwise x: isSorted xs 
   For the purposes of this function, we consider lists containing zero
   or one elements to be sorted. -}

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:y:xs) = x <= y && isSorted(y:xs) --x must be less than or equal to following elements
    

isSorted_tests = "isSorted" ~:
                 TestList [ "123" ~: isSorted [1,2,3] ~?= True
                          , "321" ~: isSorted [3,2,1] ~?= False
                          , "repeattrue" ~: isSorted [1, 1, 2, 2, 4, 4, 50, 100, 100] ~?= True
                          , "repeatfalse" ~: isSorted [6, 6, 8, 7, (-2), (-2)] ~?= False
                          , "repeatminus1true" ~: isSorted [6, 6, 7, 6] ~?= False
                           ]

--------------------------------------------------------------------------------
-- Problem 10 (merge_sorted)

{- Write a function that takes two sorted lists (in ascending order)
   and yields a merged list that is also sorted and contains all the
   elements from the two input lists. -}

mergeSorted :: [Int] -> [Int] -> [Int]
--1 empty list = other list
mergeSorted xs [] = xs
mergeSorted [] ys = ys
mergeSorted (x:xs) (y:ys) -- both lists = 1 element followed by list
     -- if x is less than y, x = first, then recursively call MS on rest of xs, (y:ys)
    | x <= y    = x : mergeSorted (y:ys) xs
    | otherwise = y : mergeSorted (x:xs) ys


mergeSorted_tests
  = "mergeSorted" ~:
    TestList [ "primes"     ~: mergeSorted [2,7] [3,5,11] ~?= [2,3,5,7,11]
             , "sequential" ~: mergeSorted [1,2,3] [4,5,6] ~?= [1,2,3,4,5,6]
             , "negs" ~: mergeSorted [(-10), (-8), (-6), (-2)] [(-9), (-7), (-5), (-3)] ~?= [(-10), (-9), (-8), (-7), (-6), (-5),(-3), (-2)]
             , "big" ~: mergeSorted [(-100), (-75)] [199, 500] ~?= [(-100), (-75), 199, 500] 
             , "both empty" ~: mergeSorted [] [] ~?= [] 
             , "1 empty" ~: mergeSorted [] [(-2), 0, 2] ~?= [(-2), 0, 2]
             ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- NOTE:
--   From here on out (other than the challenge problem), **NO RECURSION** is
--   allowed. Instead, use the library functions `map`, `filter`, and `zipWith`.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Problem 11 (evens)

{- Write a function that takes a list of integers and returns a list containing
   only the even numbers from the input list. -}
--1 way: pattern matching
evensOnly :: [Int] -> [Int]
evensOnly x = [y | y <- x, y `mod` 2 == 0]
--filter even x
-- == filter even 
--2nd way: helper function
--evenHelper::Int -> Bool
--evenHelper n = n `mod` 2 == 0
--evensOnly x = filter (evenHelper) x 



evensOnly_tests = "evensOnly" ~:
                  TestList [ "12345" ~: evensOnly [1,2,3,4,5] ~?= [2,4]
                           , "2468"  ~: evensOnly [2,4,6,8]   ~?= [2,4,6,8]
                           , "(-5)(-2)4(-6)" ~: evensOnly [(-5),(-2),4,(-6)]   ~?= [(-2), 4, (-6)]
                           , "(-1)(-2)0235" ~: evensOnly [(-1), (-2), 0, 235] ~?= [(-2), 0] 
                           ]

--------------------------------------------------------------------------------
-- Problem 12 (squares)

{- Write a function that takes a list of integers and returns a list containing
   the squares of the numbers in the input list. -}
-- 1 way: helper function, map square function onto list
squareHelper:: Int -> Int
squareHelper y = y*y
squares :: [Int] -> [Int]
squares x = map squareHelper x
--squares = map (^2) --curry infix operator
--2nd way: pattern matching
--squares x = [a*a | a <-x]

squares_tests = "squares" ~:
                TestList [ "123"  ~: squares [1,2,3]    ~?= [1,4,9]
                         , "negs" ~: squares [-1,-2,-3] ~?= [1,4,9]
                         , "odd big" ~: squares [11, 13, 17, 19 ] ~?= [121, 169, 289, 361]
                         , "yuge even some neg" ~: squares [(-20), (-100), (96)] ~?= [400, 10000, 9216]]

--------------------------------------------------------------------------------
-- Problem 13 (wurble)

{- Write a function that takes a list of integers and returns a list containing
   the squares of the negative integers in the input list, as long as that square's
   last digit is a 6. -}

--pattern match: return squared value if value is in list, and it's negative, and it ends in 6
wurble :: [Int] -> [Int]
wurble [] = []
wurble x = [a*a | a <- x, (mod (a*a) 10 == 6), (a < 0)]

wurble_tests = "wurble" ~:
               TestList [ "negs" ~: wurble [-1,-2,-3,-4,-5] ~?= [16]
                        , "neg6" ~: wurble [1,2,3,4,5,-6]   ~?= [36]
                        , "negeven" ~: wurble [-2, -4, -6, -8, 6, 14] ~?= [16, 36]
                        , "emptylist" ~: wurble [] ~?= [] ]

--------------------------------------------------------------------------------
-- Problem 14 (sums)

{- Write a function that takes two lists of integers and returns a list
   containing the sums of corresponding integers. If one list is longer than
   the other, simply ignore the extra elements. -}

sums :: [Int] -> [Int] -> [Int]
sums a b = zipWith (+) a b

sums_tests = "sums" ~:
             TestList [ "123,456" ~: sums [1,2,3] [4,5,6] ~?= [5,7,9]
                      , "1234,00" ~: sums [1,2,3,4] [0,0] ~?= [1,2]
                      , "[], 987" ~: sums [] [98, 7] ~?= []
                      , "1000, 5544332211" ~: sums [10, 0, 0] [55, 44, 33, 22, 11] ~?= [65, 44, 33]]

--------------------------------------------------------------------------------
-- Problem 15 (permutations)

-- This one is a challenge problem, so it's worth 0 points -- kudos only.
-- You *MAY* use recursion here.

{- A PERMUTATION of a list l is a list that has the same elements as l
   but is not necessarily in the same order.

   Write a function that, given a list l, calculates ALL of the
   permutations of l (and returns them as a list). For example,

       permutations [1,2,3]

   might yield

       [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]].

   (We say "might yield" here because we haven't specified the
   order of the permutations in the list returned by your function.
   For example, the result

       [[1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1], [1,2,3]]

   would also be correct.)

   Hint: Begin by writing a unit test or two, to make sure you understand the
   problem (even though you may need to rewrite them if your answer comes out
   in a different order, the exercise is useful). Also, you'll probably want
   to break the problem down into one or more sub-problems, each of which can
   be solved by recursion.

   Note: Do not remove or comment out this function stub, even if you
   choose not to attempt the challenge problem. Your file will not
   compile when you upload it for grading if 'permutations' is
   missing. -}

permutations :: [a] -> [[a]]
permutations = error "ugh perm"

{- Note that you will also have to think about how to TEST
   permutations, as there may be several correct solutions for each
   input. -}


permutations_tests
  = "permutations" ~:
    TestList [ "perm1" ~: permutations [5, 4, 3] ~?= [[5, 4, 3], [5, 3, 4], [4, 3, 5], [4, 5, 3], [3, 4, 5], [3, 5, 4]]
             , "perm2" ~: permutations [1, 2, 3, 4] ~?= [[1, 2, 3, 4], [1, 3, 2, 4], [1, 2, 4, 3], [1, 4, 3, 2], [1, 3, 4, 2], [1, 4, 2, 3], [2, 1, 3, 4], [2, 3, 1, 4], [2, 1, 4, 3], [2, 3, 4, 1], [2, 1, 4, 3], [2, 1, 3, 4], [3, 2, 1, 4], [3, 2, 4, 1], [3, 4, 1, 2], [3, 4, 1, 2]]
             ]

--------------------------------------------------------------------------------
-- All the tests, for a quick overview.
-- You may remove the permutations tests if you don't want them here.

all_tests = TestList [ coins_tests
                     ,maximumTableArea_tests
                     , moveRobot_tests
                     , streetDirection_tests
                     , exists_tests
                     , join_tests
                     , containsStr_tests
                     , dollsOf_tests
                     , merge_tests
                     , isSorted_tests
                     , mergeSorted_tests
                     , evensOnly_tests
                     , squares_tests
                     , wurble_tests
                     , sums_tests
                     , permutations_tests ]
--------------------------------------------------------------------------------
{- Now that you've finished the assignment, please answer the following
   questions:

1. It went alright. I got frustrated easily because it took a while to get used to the errors and how to express what I was thinking, especially with loops.
It really helped to write it out. I liked how there were multiple ways to do something (ie pattern matching and helper functions)

2. What questions do you have?
None at the moment, it just takes a while to get used to I guess this paradigm. 

3. How long did this assignment take?
Too long, but I tried different ways of expressing something after I had already finished that problem, 
tried different error messages, and did problems multiple times testing different ways to see 
which would work, which did not work, why, and also tried to optimize some. (like for the maxTableArea,
I initially made a maxHelper function and checked the maximum of a lot, but realized minimum was more efficient
because if I knew the minimum, I knew the other two were the sides to use, whereas with the maximum, I had to call
maximum twice!!)  Overall, I'd say I spent maybe 12 hours, but it wasn't all productive time (lots of checking Twitter 
for political happenings, etc). Also spent a lot of time on test cases, getting used to GradeScrope, trying to get feedback...

-} 