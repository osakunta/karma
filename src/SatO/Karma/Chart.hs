module SatO.Karma.Chart (Chart, chart) where

import Data.Map  (Map, foldWithKey)
import Data.Text (Text, unpack)

import Graphics.Rendering.Chart.Easy

import SatO.Karma.Types (Graph (..))

newtype Chart = Chart (EC (Layout Double Double) ())

instance ToRenderable Chart where
    toRenderable (Chart x) = toRenderable x

chart :: Map Text Graph -> Chart
chart m = Chart $ do
    layout_title .= "SatO Karma"
    foldWithKey f (pure ()) m
  where
    f n (Graph _ prev next) p = do
        _ <- p
        plot (dline [next])
        plot (line (unpack n) [prev'])
      where
        prev' = case filter ((> (-30)) . fst) prev of
            []         -> [(-30,0), (0, 0)]
            ((t, _):_)
                | t > -30   -> (-30, 0) : (t, 0) : prev
                | otherwise -> prev

dline :: [[(x,y)]]  -> EC l (PlotLines x y)
dline values = liftEC $ do
    color <- currColor
    plot_lines_style . line_dashes .= [5,5]
    plot_lines_style . line_color .= color
    plot_lines_values .= values

-- | Return the curr color from the state
currColor :: EC l (AlphaColour Double)
currColor = liftCState $ do
  (c:_) <- use colors
  return c
