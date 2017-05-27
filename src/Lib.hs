{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Data.Maybe
import Web.Telegram.API.Bot
import System.Environment
import qualified Paths_orly_bookstore_bot as P
import Data.Version (showVersion)

-- We needed those language extensions to make it as simple as that
data Version = Version
  { version :: Text
  } deriving (Show, Generic)

instance ToJSON Version

-- At the moment Bot API consists of only version resource
-- that returns Version data record as JSON.
-- Thanks to Generic and ToJSON deriving Servant knows how
type BotAPI = "version" :> Get '[JSON] Version

botApi :: Proxy BotAPI
botApi = Proxy

startApp :: IO ()
startApp = do
  putStrLn "ORLY book store bot is starting..."
  env <- getEnvironment
  manager' <- newManager tlsManagerSettings
  let telegramToken' = fromJust $ lookup "TELEGRAM_TOKEN" env
      paymentsToken' = fromJust $ lookup "PAYMENTS_TOKEN" env
      config = BotConfig
        { telegramToken = Token $ pack telegramToken'
        , paymentsToken = pack paymentsToken'
        , manager = manager'
        }
  run 8080 $ app config

app :: BotConfig -> Application
app config = serve botApi $ initBotServer config

-- defined natural transformation from ServerT BotAPI Bot to Server BotAPI
initBotServer :: BotConfig -> Server BotAPI
initBotServer config = enter (transform config) botServer
    where transform :: BotConfig -> Bot :~> ExceptT ServantErr IO
          transform config = Nat (flip runReaderT config . runBot)

-- actual server implementation
botServer :: ServerT BotAPI Bot
botServer =
  returnVersion
    where version' = Version $ pack $ showVersion P.version
          returnVersion :: Bot Version
          returnVersion = return version'

newtype Bot a = Bot
    { runBot :: ReaderT BotConfig Handler a
    } deriving ( Functor, Applicative, Monad, MonadIO, -- classes from base and transformers
                 MonadReader BotConfig, MonadError ServantErr) -- classes from mtl for

data BotConfig = BotConfig
  { telegramToken :: Token
  , paymentsToken :: Text
  , manager :: Manager
  }

