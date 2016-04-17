module SatO.Karma.Chart (Chart, chart) where

import Data.Map (Map, foldWithKey)
import Data.Text (Text, unpack)
import Graphics.Rendering.Chart.Easy

newtype Chart = Chart (EC (Layout Double Double) ())

instance ToRenderable Chart where
    toRenderable (Chart x) = toRenderable x

chart :: Map Text [(Double, Double)] -> Chart 
chart m = Chart $ do
    layout_title .= "SatO Karma"
    foldWithKey f (pure ()) m 
  where
    f n d p = p >> plot (line (unpack n) [d])
