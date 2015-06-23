--------------------------------------------------------------------------------
-- Module       : Main
-- Maintainer   : maxdelgiudice@gmail.com
-- Stability    : Experimental
-- Summary      : Some common functions/datas used by a bunch of other modules.
--------------------------------------------------------------------------------
module Bot.Common where

import           Data.List
import           Network
import           System.IO
import           Text.Printf
import           Control.Exception        ( bracket, bracket_ )
import           System.Exit
import           Control.Arrow
import           Control.Monad.Reader
import           Text.Read                ( readMaybe )
import           System.Random
import           System.Time

import qualified Utils.Settings as S

-- | Basic Parameters TODO: incorporate a settings module
--   that reads from settings.yml file
server  = "irc.freenode.net"
port    = 6667
chan    = "##mfdel-chat"
nick    = "jeeves"

-- | Define a data type that holds our handle, so we don't have to
--   pass it  to every function.
data Bot = Bot { socket    :: Handle
               , starttime :: ClockTime
               , settings  :: S.Settings
               }
type Net = ReaderT Bot IO

-- | Write stuff to the IRC server. Also writes a message to the console in which
--   the bot is running.
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t
