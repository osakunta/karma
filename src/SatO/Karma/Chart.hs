module SatO.Karma.Chart (Chart, chart) where

import Data.Map  (Map, foldWithKey)
import Data.Text (Text, unpack)
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant (d65)

import Graphics.Rendering.Chart.Easy

import SatO.Karma.Types (Graph (..))

newtype Chart = Chart (EC (Layout Double Double) ())

instance ToRenderable Chart where
    toRenderable (Chart x) = toRenderable x

chart :: Map Text Graph -> Chart
chart m = Chart $ do
    setColors . fmap (opaque . mkXYZTriple . (/16)) $ [0,2..14] ++ [1,3..15]
    layout_title .= "SatO Karma"
    foldWithKey f (pure ()) m
  where
    f n (Graph curr prev next) p = do
        plot (dline [next])
        plot (line n' [prev'])
        p
      where
        n' = unpack n ++ " " ++ show (round $ curr * 1000 :: Int)
        prev' = case filter ((> mint) . fst) prev of
            []         -> [(mint, 0), (0, 0)]
            t'@((t, _):_)
                | t > mint   -> (mint, 0) : (t, 0) : t'
                | otherwise -> t'
        mint = negate 60

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

mkXYZTriple :: Double -> Colour Double
mkXYZTriple p = cieLAB d65 l x y
  where
    l = 60
    x = 20 + 70 * cos (2 * pi * p)
    y = 20 + 70 * sin (2 * pi * p)
