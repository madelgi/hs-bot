--------------------------------------------------------------------------------
-- Module       : Bot.Wikipedia
-- Maintainer   : maxdelgiudice@gmail.com
-- Stability    : Experimental
-- Summary      : Will pull summary information off of information and display
--                it in your channel. Triggered with a `!wiki <thing>` command
--
-- TODOs        : Some things on the agenda:
--
--                  * Handle alternative entries better. `!wiki the great gatsby`
--                    should not be met with a redirect error b/c it's not in
--                    title case.
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Bot.Wikipedia
    ( wiki
    ) where

import           Network.HTTP.Conduit (simpleHttp)

import           Control.Applicative
import           Control.Monad       ( mzero, mapM_ )

import qualified Data.ByteString.Lazy.Char8 as BS hiding ( map )
import           Data.Map            ( Map, elems )
import           Data.Aeson
import           Data.Char           ( toLower )
import           Bot.BaseFunctions   ( idMsg )
import           Bot.Common

-- | Base Wikipedia URL
base = "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro=&explaintext=&titles="
homePage = "https://en.wikipedia.org/wiki/"

--------------------------------------------------------------------------------
-- JSON Parsing
data Request = Request
    { query :: Query
    } deriving (Show)

instance FromJSON Request where
    parseJSON (Object o) =
        Request <$> (o .: "query")

data Query = Query
    { pages      :: Pages
    } deriving (Show)

instance FromJSON Query where
    parseJSON (Object o) =
        Query <$> (o .: "pages")

newtype Pages = Pages { unPage :: Map String Page }
    deriving Show

instance FromJSON Pages where
    parseJSON val = Pages <$> parseJSON val

data Page = Page
    { pageId  :: Int
    , ns      :: Int
    , title   :: String
    , extract :: String
    } deriving (Show)

instance FromJSON Page where
    parseJSON (Object o) =
        Page <$> (o .: "pageid")
             <*> (o .: "ns" )
             <*> (o .: "title")
             <*> (o .: "extract")
    parseJSON _ = mzero

--------------------------------------------------------------------------------

wiki :: String -> Net ()
wiki s = do
    req <- simpleHttp $ prepReq s
    let reqStruct = decode req
    case reqStruct of
        Nothing -> idMsg "I got nothin'"
        Just b  -> do
            mapM_ idMsg . splitString . extractSummary $ b
            idMsg $ "Learn More: " ++ homePage ++ (clean s)

-- | Take the summary out of the nested Request data structure
extractSummary :: Request -> String
extractSummary req = cleanSummary . extract . head . elems
                                  . unPage . pages . query $ req

cleanSummary :: String -> String
cleanSummary [] = []
cleanSummary (x:xs)
    | x == '\n' = ' ' : (cleanSummary xs)
    | otherwise = x   : (cleanSummary xs)

-- | Takes a string and appends it ot the end of our base URL
prepReq :: String -> String
prepReq x | x == "" = homePage
prepReq x = case (clean x) of
    ""  -> homePage
    _   -> base ++ (clean x)

clean :: String -> String
clean = map $ \c -> if c == ' ' then '_' else toLower c
