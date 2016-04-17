{-# LANGUAGE OverloadedStrings #-}
module SatO.Karma.Types where

import Control.Exception (Exception)
import Data.Text         (Text)
import Data.Time         (UTCTime)
import Data.Typeable     (Typeable)
import Servant           (FromFormUrlEncoded (..))

import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple           as Postgres
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.FromRow   as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Database.PostgreSQL.Simple.ToRow     as Postgres

data ActionEnum
    = Coffee
    | DishwasherIn
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
actionEnumToText Coffee        = "brewed-coffee"
actionEnumToText DishwasherIn  = "dishwasher-in"
actionEnumToText DishwasherOut = "dishwasher-out"
actionEnumToText Dishes        = "dishes"
actionEnumToText TablesPori    = "tables-pori"
actionEnumToText TablesRauma   = "tables-rauma"

actionEnumToHuman :: ActionEnum -> Text
actionEnumToHuman Coffee        = "Keitti kahvit"
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
            who'  <- lookup "who" inputs
            let who = if T.null who' then "joku muu" else who'
            whatText <- lookup "what" inputs
            what <- actionEnumFromText whatText
            pure $ InsertAction who what
