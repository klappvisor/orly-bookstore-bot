{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text
import Network.HTTP.Client (Manager)
import Web.Telegram.API.Bot
import qualified Paths_orly_bookstore_bot as P
import Data.Version (showVersion)

-- We needed those language extensions to make it as simple as that
data Version = Version
  { version :: Text
  } deriving (Show, Generic, ToJSON)

-- At the moment Bot API consists of only version resource
-- that returns Version data record as JSON.
-- Thanks to Generic and ToJSON deriving Servant knows how
type BotAPI = "version" :> Get '[JSON] Version

botApi :: Proxy BotAPI
botApi = Proxy

startApp :: IO ()
startApp = do
  putStrLn "ORLY book store bot is starting..."
  run 8080 app

app :: Application
app = serve botApi botServer

-- actual server implementation
botServer :: Server BotAPI
botServer =
  returnVersion
    where version' = Version $ pack $ showVersion P.version
          returnVersion = return version'

{-
newtype Bot a = Bot
    { runBot :: ReaderT BotConfig Handler a
    } deriving ( Functor, Applicative, Monad, MonadIO, -- classes from base and transformers
                 MonadReader BotConfig, MonadError ServantErr) -- classes from mtl for

data BotConfig = BotConfig
  { telegramToken :: Token
  , paymentToken :: Text
  , manager :: Manager
  }

-}
