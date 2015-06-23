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

-- | Define a data type that holds our handle, so we don't have to
--   pass it  to every function.
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
