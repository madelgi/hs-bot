--------------------------------------------------------------------------------
-- Module       : Utils.Settings
-- Maintainer   : maxdelgiudice@gmail.com
-- Stability    : Experimental
-- Summary      : Reads your settings file into a Settings struct.
--------------------------------------------------------------------------------
-- {{{ Module declaration and imports.
{-# LANGUAGE OverloadedStrings #-}
module Utils.Settings where

import Control.Applicative
import Data.Yaml
import Data.Maybe       ( fromJust )

import qualified Data.ByteString.Char8 as BS
-- }}}

settingsFile = "data/settings.yml"

getSettings :: IO Settings
getSettings = do
    ymldata <- BS.readFile settingsFile
    let settings = Data.Yaml.decode ymldata :: Maybe Settings
    return $ fromJust settings

--cleanSettings :: Settings -> Settings
--------------------------------------------------------------------------------
-- | Data structures and FromJSON instances for parsing the yml file.

data Settings = Settings
    { irc :: IRCSettings
    , bot :: BotSettings
    } deriving ( Show )

data IRCSettings = IRCSettings
    { server :: String
    , port   :: Int
    , chan   :: String
    } deriving ( Show )

data BotSettings = BotSettings
    { nick :: String
    } deriving ( Show )

instance FromJSON Settings where
    parseJSON (Object v) =
        Settings <$> (v .: "irc")
                 <*> (v .: "bot")
    parseJSON _ = error "Can't parse from YAML"

instance FromJSON IRCSettings where
    parseJSON (Object v) =
        IRCSettings <$> (v .: "server")
                    <*> (v .: "port")
                    <*> (v .: "chan")
    parseJSON _ = error "Can't parse from YAML"

instance FromJSON BotSettings where
    parseJSON (Object v) =
        BotSettings <$> (v .: "nick")
    parseJSON _ = error "Can't parse from YAML"
