{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module SatO.Karma (
    defaultMain,
    -- * Other stuff
    Action,
    ) where

import Control.Exception      (Exception)
import Control.Monad          (void, forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.FileEmbed         (embedStringFile)
import Data.Maybe             (fromMaybe)
import Data.List              (nub, sort)
import Data.Pool              (Pool, createPool, withResource)
import Data.Text              (Text)
import Data.Time              (UTCTime)
import Data.Typeable          (Typeable)
import Lucid
import Network.Wai
import Servant
import Servant.HTML.Lucid
import System.Environment     (lookupEnv)
import System.IO              (hPutStrLn, stderr)
import Text.Read              (readMaybe)

import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)

import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple           as Postgres
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.FromRow   as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Database.PostgreSQL.Simple.ToRow     as Postgres
import qualified Network.Wai.Handler.Warp             as Warp

data ActionEnum
    = DishwasherIn
    | DishwasherOut
    | Dishes
    | TablesPori
    | TablesRauma
    deriving (Eq, Ord, Enum, Bounded, Show)

actionEnumFromText :: Text -> Maybe ActionEnum
actionEnumFromText = flip lookup l
  where
    l = fmap (\a -> (actionEnumToText a, a)) [minBound .. maxBound]

actionEnumToText :: ActionEnum -> Text
actionEnumToText DishwasherIn  = "dishwasher-in"
actionEnumToText DishwasherOut = "dishwasher-out"
actionEnumToText Dishes        = "dishes"
actionEnumToText TablesPori    = "tables-pori"
actionEnumToText TablesRauma   = "tables-rauma"

actionEnumToHuman :: ActionEnum -> Text
actionEnumToHuman DishwasherIn  = "Täytti tiskikoneen"
actionEnumToHuman DishwasherOut = "Tyhjensi tiskikoneen"
actionEnumToHuman Dishes        = "Tiskasi"
actionEnumToHuman TablesPori    = "Siivosi porin pöydät"
actionEnumToHuman TablesRauma   = "Siivosi rauman pöydät"

instance Postgres.FromField ActionEnum where
    fromField f mdata = Postgres.fromField f mdata >>= g
      where
        g t = maybe (Postgres.conversionError $ ActionEnumConversionError t)  pure. actionEnumFromText $ t

instance Postgres.ToField ActionEnum where
    toField = Postgres.toField . actionEnumToText

newtype ActionEnumConversionError = ActionEnumConversionError Text
    deriving (Show, Typeable)

instance Exception ActionEnumConversionError

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

data Action = Action
    { _actionMember :: !Text
    , _actionAction :: !ActionEnum
    , _actionStamp  :: !UTCTime
    }
    deriving (Eq, Ord, Show)

instance Postgres.FromRow Action where
    fromRow = Action <$> Postgres.field <*> Postgres.field <*> Postgres.field

data InsertAction = InsertAction
    { _insActionMember :: !Text
    , _insActionAction :: !ActionEnum
    }
    deriving (Eq, Ord, Show)

instance Postgres.ToRow InsertAction where
    toRow (InsertAction who what) = [Postgres.toField who, Postgres.toField what]

instance FromFormUrlEncoded InsertAction where
    fromFormUrlEncoded inputs =
        maybe (Left "Cannot parse InsertAction") Right $ do
            who  <- lookup "who" inputs
            whatText <- lookup "what" inputs
            what <- actionEnumFromText whatText
            pure $ InsertAction who what

type ActionUrl = Text

data IndexPage = IndexPage
    { _indexPageActionUrl :: !ActionUrl
    , _indexPageActions   :: ![Action]
    }

data Ctx = Ctx
    { _ctxPostgresPool :: !(Pool Postgres.Connection)
    , _ctxActionUrl    :: !ActionUrl
    }

type KarmaAPI =
    Get '[HTML] IndexPage
    :<|> ReqBody '[FormUrlEncoded] InsertAction :> Post '[HTML] IndexPage

karmaApi :: Proxy KarmaAPI
karmaApi = Proxy

instance ToHtml IndexPage where
    toHtmlRaw _ = pure ()
    toHtml (IndexPage actionUrl as) = page_ "SatO Karma" $ do
        form_ [action_ $ actionUrl, method_ "POST"] $ do
            -- Kuka
            div_ [class_ "row"] $ do
                div_ [class_ "large-3 columns"] $
                    label_ [class_ "text-right middle", for_ "who"] $ "Kuka?"
                div_ [class_ "large-9 columns"] $
                    input_ [type_ "text", name_ "who", id_ "who" ]

            div_ [class_ "row" ] $
                div_ [class_ "large-12 columns"] $
                    forM_ (sort $ take 10 $ nub $ _actionMember <$> as) $ \am -> do
                        button_ [class_ "medium button magic-auto-fill", onclick_ "return false;"] $ toHtml am
                        span_ " "

            -- Mitä?
            div_ [class_ "row"] $ do
                div_ [class_ "large-3 columns"] $
                    label_ [class_ "text-right middle", for_ "what"] $ "Mitä?"
                div_ [class_ "large-9 columns"] $ forM_ [minBound..maxBound] $ \e ->
                    label_ $ do
                        input_ [type_ "radio", name_ "what", value_ $ actionEnumToText e]
                        span_ $ toHtmlRaw $ actionEnumToHuman e

            -- Submit
            div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $
                input_ [class_ "medium success button", type_ "submit", value_ "Lähetä"]

        hr_ []

        div_ [class_ "row"] $
            div_ [class_ "large-12 columns"] $
                span_ $ "Statsit tulee sit kun on mistä tehdä statsit"

        hr_ []

        table_ $ do
            tr_ $ do
                th_ "Kuka"
                th_ "Mitä"
                th_ "Koska"
            forM_ (take 50 as) $ \(Action member enum stamp) -> tr_ $ do
                td_ $ toHtml member
                td_ $ toHtml $ actionEnumToHuman enum
                td_ $ toHtml $ show stamp

-------------------------------------------------------------------------------
-- Enspoints
-------------------------------------------------------------------------------

indexPage :: Ctx -> IO IndexPage
indexPage (Ctx pool actionUrl) = withResource pool $ \conn -> do
    as <- Postgres.query_ conn "SELECT member, action, stamp FROM karma ORDER BY stamp DESC;"
    pure $ IndexPage actionUrl as

submitPage :: Ctx -> InsertAction -> IO IndexPage
submitPage ctx@(Ctx pool _) ia = do
    withResource pool $ \conn ->
        void $ Postgres.execute conn "INSERT INTO karma (member, action) VALUES (?, ?)" ia
    indexPage ctx

-------------------------------------------------------------------------------
-- HTML stuff
-------------------------------------------------------------------------------

-- | Page template.
page_ :: Monad m => Text -> HtmlT m () -> HtmlT m ()
page_ t b = doctypehtml_ $ do
    head_ $ do
        title_ $ toHtml t
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        meta_ [httpEquiv_ "x-ua-compatible", content_"ie=edge"]
        style_ [type_ "text/css"] ($(embedStringFile "foundation-6/css/foundation.min.css") :: String)
        style_ [type_ "text/css"] ($(embedStringFile "style.css") :: String)
    body_ $ do
        b
        script_ $(embedStringFile "script.js")

-------------------------------------------------------------------------------
-- WAI boilerplate
-------------------------------------------------------------------------------

server :: Ctx -> Server KarmaAPI
server ctx = liftIO (indexPage ctx) :<|> liftIO . (submitPage ctx)

app :: Ctx -> Application
app ctx = serve karmaApi (server ctx)

lookupEnvWithDefault :: Read a => a -> String -> IO a
lookupEnvWithDefault def v = do
    x <- lookupEnv v
    return $ fromMaybe def (x >>= readMaybe)

defaultMain :: IO ()
defaultMain = do
    port <- lookupEnvWithDefault 8080 "PORT"
    postgresConnectInfo <- fromMaybe (error "no karma postgresurl") . (>>= parseDatabaseUrl)
        <$> lookupEnv "KARMA_POSTGRES_URL"
    actionUrl           <- maybe "/" T.pack
        <$> lookupEnv "KARMA_ACTIONURL"
    pool <- createPool (Postgres.connect postgresConnectInfo) Postgres.close 1 60 5
    let ctx = Ctx pool actionUrl
    hPutStrLn stderr "Hello, karma is alive"
    hPutStrLn stderr "Starting web server"
    Warp.run port (app ctx)
