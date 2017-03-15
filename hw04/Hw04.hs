{-
Name: Lizzie Siegle,
email: esiegle@brynmawr.edu,
collaborators: Eileen, Kennedy, Rachel, Jordan (talked to--not quite collab),
comments: technically late (Wed March 1 11:42 PM PST, post ForwardJS Conference, asked for extension)
:!pwd.
import qualified ... as B, which means that every use of a ByteString function (including operators) or type
must be preceded by B. . Thus, to get the length of a ByteString, you use B.length. Even to mention the
type ByteString, you must use B.ByteString.
* Data.ByteString.Lazy.readFile
-}
{-#LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, GADTSyntax #-}
module Hw04 where
import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Test.HUnit

import Data.List (sort)

--proper booleans instead of Y and N -> value from Aeson
--decode :: FromJSON a ⇒ ByteString → Maybe a
--One aeson function that parses JSON is called decode:
--decode :: FromJSON a ⇒ ByteString → Maybe a
--The FromJSON type class is also exported by the aeson package6
--Its method parseJSON::Value → Parser a
--(which you will not have to write in this assignment) says how to parse from JSON to the class type a. Thus,
--I am giving you these details in case you want to look at other datasets.
--5Value is a type from the aeson library. Hoogle does not search the aeson package by default, so you will have to access the
--package documentation on Hackage. Try this URL: http://hackage.haskell.org/package/aeson.
--anything in the FromJSON type class can be parsed from a JSON file. Of course, parsing can fail, so decode
--returns a Maybe type.
--A useful member of the FromJSON type class is Value. Value represents JSON syntax in a Haskell type.
--Check out its documentation.7 A JSON Value can be one of six things: an object (something in braces; a
--mapping from key names to other values), an array (something in brackets; a listing of JSON values), some
--text, a number, a Boolean value, or the special constant null. Look a little further down in the documentation
--to see the definitions for the types Object and Array.
--An Object is a HashMap Text Value — that is, a way to get Values indexed by some Text. However, the
--details of HashMap aren’t important at all for you. What is critically important is that there is an instance
--Functor (HashMap k). That means that a valid type for fmap is (a → b) → HashMap k a → HashMap k b.
--An Array is a Vector Value. Vector is a type quite like normal lists but uses a different internal representation.8
--Some operations on Vectors are faster than for lists; some are slower. However, the details of Vector
--aren’t important at all for you. What is critically important is that there

{-1: Write a func that changes all occurrences of String "Y" to Bool:True, + all occurrences of String "N" to be Bool:
False. No other part of the input Value should change. -}
--instance ToJSON where 
--	toJSON (Value v) = Bool
--filedata ← B.readFile "markets.json" in GHCi and
--then calling parseData on filedata.
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object obj) = Object $ fmap ynToBool obj
ynToBool (Array arr)  = Array $ fmap ynToBool arr
ynToBool val          = val

{- 2 takes in a ByteString containing JSON data and outputs either Nothing or a Value that has been
processed by ynToBool.
Hint: This can be very short, if you use Maybe’s Functor instance!
filedata ← B.readFile "markets.json" 
parseData filedata.
-}
parseData :: B.ByteString -> Maybe Value 
parseData b = fmap ynToBool $ decode b

{- 3
Write a Market type, including the fields that interest you. At a minimum, include marketname, x
(the longitude of the market), y (the latitude of the market), state, and cheese (which is a Bool). Use
T.Text to represent text. (String also works, but is less efficient.)
-}
data Market = Market { marketname :: T.Text, 
                       x          :: Double,
                       y          :: Double,
                       state      :: T.Text,
                       cheese     :: Bool
                   }
                   deriving (Show, Generic)

instance FromJSON Market

{- 4
that uses parseData and fromJSON (from the aeson package) to parse in the list of markets in the file
-}
parseMarkets :: B.ByteString -> Maybe [Market]
parseMarkets f
       | ((pure test) <*> fmap (fromJSON :: Value -> Result [Market] ) (parseData f)) == Just True = (pure getResult) <*> fmap (fromJSON :: Value -> Result [Market]) (parseData f)
       | otherwise = error "invalid file"
       
       where
          test :: Result a -> Bool
          test (Success a) = True
          test _ = False
          
          getResult :: Result a -> a
          getResult (Success a) = a

{- 5
that loads the market data. Use B.readFile :: String → IO B.ByteString. In the event of a parsing
failure, report the error using fail :: String → IO a. (fail aborts an action, reporting an error to the
user. It never returns, so it can be used no matter what IO type is expected. That’s why it returns
type IO a, for any a.)
Once this is defined, you can get your market data by saying mkts ← loadData in GHCi
-}
loadData :: IO [Market]
loadData = B.readFile "markets.json" >>= return . getData . parseMarkets
          where
            getData :: Maybe [Market] -> [Market]
            getData Nothing = []
            getData $ Just a = a
{- 6 
Write the OrdList datatype and its Monoid instance. Make sure your implementation of (<>) is
O(m + n), where m and n are the lengths of the input lists.
-}

data OrdList a = OrdList { getOrdList :: [a] }
  deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty  = OrdList { getOrdList = [] }
    mappend xs ys = OrdList { getOrdList = sort $ (getOrdList xs) ++ (getOrdList ys) }
    mconcat [] = mempty
    mconcat xs = OrdList { getOrdList = sort $ concat $ map getOrdList xs }


{-7
--that searches through the provided list of Markets for market names containing the given T.Text
--(Data.Text.isInfixOf will be useful here). With each found market record, use the function provided to
--convert the market record into the monoid of the caller’s choice, and then combine all the individual
--results using mappend and mconcat.
--Note that we can always expand type synonyms in Haskell. So, the type of search is fully equivalent
--to Monoid m ⇒ (Market → m) → T.Text → [Market] → m. This means that the definition for search
--may include up to three arguments, even though the type looks like it should take only one.
--Hint: This function should not be very long. If it’s getting long, you’re probably doing something the
--wrong way. You may also want to check out the intInts example from the lecture notes.
--Hint: Using an as-pattern might be helpful. Here is an example:
--marketWithName :: Market → (T.Text, Market)
--marketWithName mkt@(Market {marketname = name }) = (name, mkt)
--Note that mkt is matched against the whole market record, while the pattern-mat
-}
type Searcher m = T.Text -> [Market] -> m
search :: Monoid m => (Market -> m) -> Searcher m
search f text [] = mconcat mempty
search f text (x:xs) 
    = case (T.isInfixOf text (marketname x)) of
    	True -> (f x) <> search f text xs
    	False -> search f text xs

-- #8: returns the first market found by a search, if any are found at all.
--Like in the case for search, above, your firstFound function can be given arguments, even though the
--type looks like there should be no arguments. Unlike search, though, this one is definable without
--taking any arguments, with the right call to search.
--This function (and all future function

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 getFirst (search toFirst)
  where toFirst m = First (Just m)

-- #9
lastFound :: Searcher (Maybe Market)
lastFound = compose2 getLast (search last1)
  where last1 m = Last (Just m)
   {-10
returns all the markets found by a search.
  -}
allFound :: Searcher [Market]
allFound = search (:[])

{-11
 returns the number of markets found by a search.
-}
numberFound :: Searcher Int
numberFound = compose2 getSum (search sum1)
  where sum1 _ = Sum 1

 {- 12 
 -}
instance Eq Market where
    (==) (Market {x = long1}) (Market {x = long2}) = long1 == long2
instance Ord Market where
    compare (Market {x = long1}) (Market {x = long2}) = compare long1 long2

orderedNtoS :: Searcher [Market]
orderedNtoS = compose2 reverse $ compose2 sort allFound