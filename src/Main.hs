--------------------------------------------------------------------------------
-- Module       : Main
-- Maintainer   : maxdelgiudice@gmail.com
-- Stability    : Experimental
-- Summary      : gets yr bot up and running
--
--------------------------------------------------------------------------------
-- {{{ Module declaration and imports.
module Main
    ( main
    ) where

import           Data.List                ( dropWhile, isPrefixOf )
import           Network                  ( connectTo, PortID(..) )
import           System.IO                ( hSetBuffering, hClose, hFlush
                                          , stdout, hGetLine, Handle
                                          , BufferMode(..) )
import           Text.Printf              ( hPrintf, printf )
import           Control.Exception        ( bracket, bracket_ )
import           Control.Monad.Reader
import           System.Random
import           System.Time

import           Utils.Settings
import           Bot.BaseFunctions
import           Bot.Wikipedia            ( wiki )
import           Bot.Common
-- }}}

-- | TODO: add words
main :: IO ()
main = bracket connect disconnect loop
    where disconnect = hClose . socket
          loop st    = runReaderT run st

-- | TODO: add words
connect :: IO Bot
connect = notify $ do
    s <- getSettings
    t <- getClockTime
    h <- connectTo (server $ irc s) (PortNumber (fromIntegral (port $ irc s) ))
    hSetBuffering h NoBuffering
    return (Bot h t s)
    where
        notify a = bracket_
            (printf "Connecting to server ..." >> hFlush stdout)
            (putStrLn "done.")
            a

-- | TODO: add words
run :: Net ()
run  = do
    s <- liftIO getSettings
    write "NICK" (nick $ bot s)
    write "USER" ( (nick $ bot s) ++ " 0 * :tutorial bot")
    write "JOIN" (chan $ irc s)
    asks socket >>= listen

-- | TODO: add words
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` liftIO (hGetLine h)
    liftIO (putStrLn s)
    eval (clean s)
    where
        forever a = a >> forever a
        clean     = drop 1 . dropWhile (/= ':') . drop 1

-- | Eval takes a string as input and returns some sort of bot action. E.g.,
--   in an IRC session:
--
--      | 00:22:19 <@mfdel_> !id uh
--      | 00:22:20 < jeeves> uh
--
--    In the above example, eval catches the `!id` command, and returns the
--    appropriate bot response.
eval :: String -> Net ()
eval "!quit"           = quitBot
eval "!help"           = helpMsg
eval "!uptime"         = uptime >>= idMsg
eval x | pre "!id" x   = idMsg (drop 4 x)
eval x | pre "!roll" x = roll (drop 6 x)
eval x | pre "!wiki" x = wiki (drop 6 x)
eval _                 = return ()

-- | Helper function, shortens `isPrefixOf`.
pre :: String -> String -> Bool
pre = isPrefixOf
