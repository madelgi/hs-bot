{-# LANGUAGE OverloadedStrings #-}
module Wikipedia where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Text.XML.Cursor

import qualified Data.ByteString.Lazy.Char8 as BS hiding ( map )

import Data.Aeson

testUrl = "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro=&explaintext=&titles=dog"
mainPage = "https://en.wikipedia.org/wiki/Main_Page"
base = "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro=&explaintext=&title="

wikiLookup :: String -> Maybe ()
wikiLookup s = do
    wp <- decode $ simpleHttp (prepReq s)
    let wpData = (decode wp) :: Maybe Value
    putStrLn "Oh"

--extractSummary :: Maybe Value -> Maybe String
--extractSummary s = undefined

prepReq :: String -> String
prepReq x | x == "" = mainPage
prepReq x = case (clean x) of
    ""  -> mainPage
    _   -> base ++ (clean x)
    where clean s = map (\c -> if c == ' ' then '_' else c) s

getWikiSummary :: String -> String
getWikiSummary = undefined
