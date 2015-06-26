--------------------------------------------------------------------------------
-- Module       : Main
-- Maintainer   : maxdelgiudice@gmail.com
-- Stability    : Experimental
-- Summary      : Some common functions/datas used by a bunch of other modules.
--------------------------------------------------------------------------------
module Bot.Common where


import           System.IO              ( Handle )
import           Text.Printf            ( hPrintf, printf )
import           System.Time            ( ClockTime )
import           Control.Monad.Reader

import           Utils.Settings
-- | Define a data type that holds the bot state, which currently consists
--   of:
--
--      * A handle (socket)
--      * When the bot joined the channel (starttime)
--      * Configuration read from an external yaml file (settings)
data Bot = Bot { socket    :: Handle
               , starttime :: ClockTime
               , settings  :: Settings
               }


type Net = ReaderT Bot IO

-- | Write stuff to the IRC server. Also writes a message to the console in which
--   the bot is running.
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t

splitString :: [Char] -> [[Char]]
splitString [] = []
splitString s  = line : (splitString $ drop n s)
    where n    = length line + 1
          line = splitHelp s

splitHelp :: [Char] -> [Char]
splitHelp s | (drop 400 s) == [] = (take 400 s)
splitHelp s =
    if (head rem /= ' ')
    then front ++ (takeWhile (/= ' ') rem)
    else front
        where front = take 400 s
              rem   = drop 400 s
