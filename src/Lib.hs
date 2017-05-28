{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics hiding (from)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Data.Maybe
import Data.Monoid
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
         :<|> "webhook"
              :> Capture "secret" Text
              :> ReqBody '[JSON] Update
              :> Post '[JSON] ()

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
        { telegramToken = Token $ T.pack $ "bot" <> telegramToken'
        , paymentsToken = T.pack paymentsToken'
        , manager = manager'
        }
  run 8080 $ app config

newtype Bot a = Bot
    { runBot :: ReaderT BotConfig Handler a
    } deriving ( Functor, Applicative, Monad, MonadIO, -- classes from base and transformers
                 MonadReader BotConfig, MonadError ServantErr) -- classes from mtl for

data BotConfig = BotConfig
  { telegramToken :: Token
  , paymentsToken :: Text
  , manager :: Manager
  }

app :: BotConfig -> Application
app config = serve botApi $ initBotServer config

initBotServer :: BotConfig -> Server BotAPI
initBotServer config = enter (transform config) botServer
    where transform :: BotConfig -> Bot :~> ExceptT ServantErr IO
          transform config = Nat (flip runReaderT config . runBot)

-- actual server implementation
botServer :: ServerT BotAPI Bot
botServer = returnVersion :<|> handleWebhook
    where version' = Version $ T.pack $ showVersion P.version
          returnVersion :: Bot Version
          returnVersion = return version'
          handleWebhook :: Text -> Update -> Bot ()
          handleWebhook secret update = do
              Token token <- asks telegramToken
              if EQ == compare secret token
                 then handleUpdate update
                 else throwError err403

handleUpdate :: Update -> Bot ()
handleUpdate update = do
    case update of
        Update { message = Just Message
          { successful_payment = Just payment } } -> handleSuccessfulPayment payment
        Update { message = Just msg } -> handleMessage msg
--      Update { ... } more cases
        Update { pre_checkout_query = Just query } -> handlePreCheckout query
        _ -> liftIO $ putStrLn $ "Handle update failed. " ++ show update

helpMessage userId = sendMessageRequest userId $ T.unlines
    [ "/help - show this message"
    , "/books - show list of all books"
    , "/find title - find book by title"
    ]

handleMessage :: Message -> Bot ()
handleMessage msg = do
    BotConfig{..} <- ask
    let chatId = ChatId $ fromIntegral $ user_id $ fromJust $ from msg
        messageText = text msg
        sendHelpMessage = sendMessageM (helpMessage chatId) >> return ()
        sendInvoices books = mapM_ sendInvoiceM $ map (buildBuyBookInvoice chatId paymentsToken) books
        byTitle title book = T.isInfixOf title $ fst book -- book title contains title
        onCommand (Just (T.stripPrefix "/help" -> Just _)) = sendHelpMessage
        onCommand (Just (T.stripPrefix "/books" -> Just _)) = sendInvoices allBooks
        onCommand (Just (T.stripPrefix "/find " -> Just title)) = sendInvoices $ filter (byTitle title) allBooks
        onCommand _ = sendHelpMessage
    liftIO $ runClient (onCommand messageText) telegramToken manager
    return ()

allBooks :: [(Text, (Text, Text, Int))]
allBooks =
  [ ("Copying and Pasting from Stack Overflow",
        ("http://i.imgur.com/fawRchq.jpg", "Cutting corners to meet arbitrary management deadlines", 7000))
  , ("Googling the Error Message",
        ("http://i.imgur.com/fhgzVEt.jpg", "The internet will make those bad words go away", 4500))
  , ("Whiteboard Interviews",
        ("http://i.imgur.com/oM9yCym.png", "Putting the candidate through the same bullshit you went through", 3200))
  , ("\"Temporary\" Workaround",
        ("http://i.imgur.com/IQBhKkT.jpg", "Who are you kidding?", 4200))
  ]

buildBuyBookInvoice (ChatId chatId) token (title, (image, description, price)) =
    (sendInvoiceRequest chatId title description payload token link code prices)
        { snd_inv_photo_url = Just image }
        where code = CurrencyCode "USD"
              payload = "book_payment_payload"
              link = "deep_link"
              prices = [ LabeledPrice title price
                       , LabeledPrice "Donation to a kitten hospital" 300
                       , LabeledPrice "Discount for donation" (-300) ]

handlePreCheckout :: PreCheckoutQuery -> Bot ()
handlePreCheckout query = do
    BotConfig{..} <- ask
    let chatId = ChatId $ fromIntegral $ user_id $ pre_che_from query
        queryId = pre_che_id query
        okRequest = AnswerPreCheckoutQueryRequest queryId True Nothing
    liftIO $ runClient (answerPreCheckoutQueryM okRequest) telegramToken manager
    return ()

handleSuccessfulPayment :: SuccessfulPayment -> Bot ()
handleSuccessfulPayment payment = do
    let totalAmount = T.pack $ show $ (suc_pmnt_total_amount payment) `div` 100
        CurrencyCode code = suc_pmnt_currency payment
    liftIO $ print $ "We have earned " <> code <> totalAmount <> ". Shipping book to the client!"
    return ()

