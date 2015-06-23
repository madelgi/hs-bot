--------------------------------------------------------------------------------
-- Module       : Bot.Base
-- Maintainer   : maxdelgiudice@gmail.com
-- Stability    : Experimental
-- Summary      : Contains basic commands for hs-bot. These are simple enough
--                that they don't merit individual modules.
--------------------------------------------------------------------------------
module Bot.BaseFunctions
    ( idMsg
    , quitBot
    , helpMsg
    , roll
    , uptime
    ) where

import           Control.Monad.Reader
import           Control.Exception        ( bracket, bracket_ )
import           System.Exit              ( exitWith, ExitCode(..) )
import           Text.Read                ( readMaybe )
import           System.Random            ( newStdGen, randomRs )
import           System.Time              ( getClockTime, diffClockTimes
                                          , TimeDiff
                                          )

import           Bot.Common
import           Utils.Settings

-- | Takes a string, reads it into an int, and then generates a random
-- | Print a message to the terminal. Can be used via IRC with
--   `!id`:
--
--      | 00:22:19 <@mfdel_> !id uh
--      | 00:22:20 < jeeves> uh
--
--   Mostly used as a helper function to other commands.
idMsg :: String -> Net ()
idMsg s = do
    config <- asks settings
    let chn = chan $ irc config
    write "PRIVMSG" (chn ++ " :" ++ s)

-- | Shut down the bot
quitBot :: Net ()
quitBot = write "QUIT" ":EXITING" >> liftIO (exitWith ExitSuccess)

-- | Return instructions on how to use the a bot
helpMsg :: Net ()
helpMsg = do
    idMsg "BASIC COMMANDS:"
    idMsg "    !help              : Display help text."
    idMsg "    !quit              : Remove hs-bot from the channel."
    idMsg "    !id    <String>    : Return the <string> back, verbatim."
    idMsg "    !roll  <Int>       : Return a random integer in range 0 Int. If no Int is"
    idMsg "                         supplied, defaults to 6 (like a dice)"

-- | number between one and the int. If the string is empty, the int defaults
--   to 6.
roll :: String -> Net ()
roll s | s == "" = roll "6"
roll s = case (readMaybe s :: Maybe Int) of
    Nothing -> idMsg "'!roll' takes an int as argument."
    Just n  -> getRand n >>= idMsg

-- Generate a random #, put it in the Net Monad.
getRand :: Int -> Net String
getRand n = liftIO $ do
    g <-  newStdGen
    return . show . head $ (randomRs (1, n) g)

-- | TODO
uptime :: Net String
uptime = do
    now  <- liftIO getClockTime
    zero <- asks starttime
    return . show $ diffClockTimes now zero
