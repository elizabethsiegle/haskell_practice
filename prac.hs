module Main where

import Network.HTTP
import Network.Browser
import System.IO

main = do
 putStrLn "Hello, World!"
 putStr "Enter your name: "
 hFlush stdout
 name <- getLine --not string -> recipe to get string, follow to get string out of it = produce later
 --last statement must be expression: need putStrLen, not name <- getLine
 putStrLn ("Hello, " ++ name)
 e_rsp <- simpleHTTP (getRequest "http://elizabethsiegle.github.io") --returns result
 --case e_rsp of
 --	Left err -> print err --no error bc page exists
 --	Right rsp -> do
 --		print rsp --print out header
 --		let body = rspBody rsp
 --		let tags = parseTags body
 --print [(rspBody rsp)] --print uut body -> haskell
 -- fetch document and return it (as a 'String'.)
 --fmap (take 100) (getResponseBody e_rsp)
 do
      (_, rsp)
         <- Network.Browser.browse $ do
               setAllowRedirects True -- handle HTTP redirects
               request $ getRequest "http://www.elizabethsiegle.github.io/"
      return (take 100 (rspBody rsp))

--identify a tags