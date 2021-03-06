{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module SatO.Karma (
    defaultMain,
    -- * Other stuff
    Action,
    ) where

import Control.AutoUpdate        (defaultUpdateSettings, mkAutoUpdate,
                                  updateAction, updateFreq)
import Control.Exception         (SomeException)
import Control.Monad             (forM_, void)
import Control.Monad.IO.Class    (MonadIO (..))
import Data.Aeson                (ToJSON (..), object, (.=))
import Data.FileEmbed            (embedStringFile)
import Data.Function             (on, (&))
import Data.List                 (nub, sortBy)
import Data.Maybe                (fromMaybe)
import Data.Monoid               ((<>))
import Data.Pool                 (Pool, createPool, withResource)
import Data.String               (fromString)
import Data.Text                 (Text)
import Data.Time                 (UTCTime, defaultTimeLocale, diffUTCTime,
                                  formatTime, getCurrentTime)
import Data.Time.Format.Human    (HumanTimeLocale (..), defaultHumanTimeLocale,
                                  humanReadableTimeI18N')
import Data.Time.Zones           (TZ, loadSystemTZ, utcToLocalTimeTZ)
import Lucid
import Network.HTTP.Types.Status (status500)
import Network.Wai
import Servant
import Servant.HTML.Lucid
import System.Environment        (lookupEnv)
import System.IO                 (hPutStrLn, stderr)
import Text.Read                 (readMaybe)

import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)

import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Network.HTTP.Media         as M
import qualified Network.Wai.Handler.Warp   as Warp

import System.IO.Unsafe (unsafePerformIO)

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Backend.Types    (vectorAlignmentFns)
import Graphics.Rendering.Chart.Renderable       (ToRenderable (..))

import SatO.Karma.Chart
import SatO.Karma.Graph
import SatO.Karma.Types

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

type ActionUrl = Text

data IndexPage = IndexPage
    { _indexPageActionUrl :: !ActionUrl
    , _indexPageTz        :: !TZ
    , _indexPageNow       :: !UTCTime
    , _indexPageActions   :: ![Action]
    }

data Ctx = Ctx
    { _ctxPostgresPool :: !(Pool Postgres.Connection)
    , _ctxActionUrl    :: !ActionUrl
    , _ctxTz           :: !TZ
    , _ctxAllGraph     :: !(IO Chart)
    }

data ActionsResponse = ActionsResponse !ActionUrl !TZ !UTCTime ![Action]

instance ToJSON ActionsResponse where
    toJSON (ActionsResponse actionUrl tz now as) = object
        [ "status" .= ("ok" :: Text)
        , "data"   .= renderText (actionTableToHtml actionUrl tz now as)
        ]

data SVG

instance Accept SVG where
   contentType _ = "image" M.// "svg+xml"

instance ToRenderable a => MimeRender SVG a where
   mimeRender _ x = fst $ renderableToSVGString' (toRenderable x) denv

denv :: DEnv Double
denv = unsafePerformIO $ defaultEnv vectorAlignmentFns 1000 700
{-# NOINLINE denv #-}

type KarmaAPI =
    Get '[HTML] IndexPage
    :<|> ReqBody '[FormUrlEncoded] InsertAction :> Post '[HTML] IndexPage
    :<|> "ajax" :> ReqBody '[JSON] InsertAction :> Post '[JSON] ActionsResponse
    :<|> "table" :> Get '[JSON] ActionsResponse
    :<|> "chart" :> Get '[SVG] Chart
    :<|> "chart" :> Capture "who" Text :> Get '[SVG] Chart

karmaApi :: Proxy KarmaAPI
karmaApi = Proxy

instance ToHtml IndexPage where
    toHtmlRaw _ = pure ()
    toHtml (IndexPage actionUrl tz now as) = page_ "SatO Karma" $ do
        div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $
            h1_ "SatO Karma"

        form_ [action_ $ actionUrl, method_ "POST"] $ do
            -- Kuka
            div_ [class_ "row"] $ do
                div_ [class_ "large-1 columns"] $
                    label_ [class_ "text-right middle", for_ "who"] $ "Kuka?"
                div_ [class_ "large-11 columns"] $ do
                    input_ [type_ "text", name_ "who", placeholder_ "Ei ole pakko täyttää, jätä tyhjäksi niin olet \"joku muu\"", id_ "who" ]

                    forM_ (sortBy (compare `on` T.toLower) $ take 20 $ nub $ _actionMember <$> as) $ \am -> do
                        button_ [class_ "medium button magic-auto-fill", onclick_ "return false;"] $ toHtml am
                        span_ " "

            -- Mitä?
            div_ [class_ "row"] $ do
                div_ [class_ "large-1 columns"] $
                    label_ [class_ "text-right middle", for_ "what"] $ "Mitä?"
                div_ [class_ "large-11 columns"] $ do
                    forM_ [minBound..maxBound] $ \e -> label_ $ do
                        input_ [type_ "radio", name_ "what", value_ $ actionEnumToText e]
                        span_ $ do
                            toHtmlRaw $ actionEnumToHuman e
                            maybe (pure ()) (small_ . toHtmlRaw . (" " <>)) $ actionEnumToHuman2 e
                    hr_ []

            -- Submit
            div_ [class_ "row"] $ do
                div_ [class_ "large-1 columns"] $ pure ()
                div_ [class_ "large-11 columns"] $
                    input_ [class_ "medium success button", type_ "submit", value_ "Lähetä"]

        hr_ []

        div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
            img_ [id_ "graph-image", src_ $ actionUrl <> "chart" ]
            br_ []
            span_ $ small_ "Päivittyy noin minuutin välein"

        hr_ []

        div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $
            actionTableToHtml actionUrl tz now as

actionTableToHtml :: Monad m => ActionUrl -> TZ -> UTCTime -> [Action] -> HtmlT m ()
actionTableToHtml actionUrl tz now as =
    table_ [ id_ "actions-table" ] $ do
        tr_ $ do
            th_ "Kuka"
            th_ "Mitä"
            th_ "Koska"
        forM_ (take 50 as) $ \(Action member enum stamp) ->
            tr_ [class_ $ actionEnumToText enum <> " " <> cls stamp ] $ do
                td_ $ a_ [href_ $ actionUrl <> "chart/" <> member ] $ toHtml member
                td_ $ toHtml $ actionEnumToHuman enum
                td_ $ toHtml $ humanTime now tz stamp
  where
    cls stamp | d < 45 * 60       = "recent"
              | d > 20 * 60 * 60  = "very-old"
              | otherwise         = "old"
      where d = now `diffUTCTime` stamp

-------------------------------------------------------------------------------
-- Human Finissh time
-------------------------------------------------------------------------------

humanTime :: UTCTime -> TZ -> UTCTime -> String
humanTime now tz stamp
    | d < 5 * 60 * 60   = humanReadableTimeI18N' loc now stamp
    | otherwise         = formatTime defaultTimeLocale "%c" $ utcToLocalTimeTZ tz stamp
  where
    d   = now `diffUTCTime` stamp
    loc = defaultHumanTimeLocale
        { justNow = "juur äskö"
        , secondsAgo = \_ i -> i <> " sekunttia sitte"
        , oneMinuteAgo = \_ -> "minuutti sitte"
        , minutesAgo = \_ i -> i <> " minuuttia sitte"
        , oneHourAgo = \_ -> "tunti sitte"
        , aboutHoursAgo = \_ i -> i <> " tuntia sitte"
        }

-------------------------------------------------------------------------------
-- Enspoints
-------------------------------------------------------------------------------

indexPage :: Ctx -> IO IndexPage
indexPage (Ctx pool actionUrl tz _) = withResource pool $ \conn -> do
    as <- Postgres.query_ conn "SELECT member, action, stamp FROM karma ORDER BY stamp DESC;"
    now <- getCurrentTime
    pure $ IndexPage actionUrl tz now as

actionsResponse :: Ctx -> IO ActionsResponse
actionsResponse (Ctx pool actionUrl tz _) = withResource pool $ \conn -> do
    as <- Postgres.query_ conn "SELECT member, action, stamp FROM karma ORDER BY stamp DESC;"
    now <- getCurrentTime
    pure $ ActionsResponse actionUrl tz now as

submitPage :: Ctx -> InsertAction -> IO IndexPage
submitPage ctx@(Ctx pool _ _ _) ia = do
    withResource pool $ \conn ->
        void $ Postgres.execute conn "INSERT INTO karma (member, action) VALUES (?, ?)" ia
    indexPage ctx

submitAjax :: Ctx -> InsertAction -> IO ActionsResponse
submitAjax ctx@(Ctx pool _ _ _) ia = do
    withResource pool $ \conn ->
        void $ Postgres.execute conn "INSERT INTO karma (member, action) VALUES (?, ?)" ia
    actionsResponse ctx

allChartEndpoint :: Ctx -> IO Chart
allChartEndpoint (Ctx _ _ _ action) = action

allChart :: Pool Postgres.Connection -> IO Chart
allChart pool = do
    now <- getCurrentTime
    withResource pool $ \conn -> do
        ps <- f conn now
        pure $ chart ps
  where
    f :: Postgres.Connection -> UTCTime -> IO (Map.Map Text Graph)
    f conn now = do
        as <- Postgres.query_ conn "SELECT member, action, stamp FROM karma ORDER BY stamp ASC;"
        pure
            . fmap (karmaGraph now . sortBy (compare `on` _actionStamp))
            . Map.fromListWith (++)
            . map (\a -> (_actionMember a, [a]))
            $ as

chartEndpoint :: Ctx -> Text -> IO Chart
chartEndpoint (Ctx pool _ _ _) whos = do
    now <- getCurrentTime
    withResource pool $ \conn -> do
        ps <- traverse (f conn now) (T.split (==',') whos)
        pure $ chart $ Map.fromList ps
  where
    f :: Postgres.Connection -> UTCTime -> Text -> IO (Text, Graph)
    f conn now who = do
        as <- Postgres.query conn "SELECT member, action, stamp FROM karma WHERE member = ? ORDER BY stamp ASC;" (Postgres.Only who)
        return (who, karmaGraph now as)

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
server ctx = liftIO (indexPage ctx)
    :<|> liftIO . (submitPage ctx)
    :<|> liftIO . (submitAjax ctx)
    :<|> liftIO (actionsResponse ctx)
    :<|> liftIO (allChartEndpoint ctx)
    :<|> liftIO . (chartEndpoint ctx)

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
    tz <- loadSystemTZ "Europe/Helsinki"
    chartAction <- mkAutoUpdate $ defaultUpdateSettings
        { updateAction = allChart pool
        , updateFreq   = 60 * 1000000  -- One minute
        }

    let ctx = Ctx pool actionUrl tz chartAction:: Ctx
    hPutStrLn stderr "Hello, karma is alive"
    hPutStrLn stderr "Starting web server"
    Warp.run port (app ctx)
    let settings = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setOnExceptionResponse onExceptionResponse
    Warp.runSettings settings (app ctx)

onExceptionResponse :: SomeException -> Response
onExceptionResponse exc =
    responseLBS status500 [] $ fromString $ "Exception: " ++  show exc
