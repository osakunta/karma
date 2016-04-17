module SatO.Karma.Graph (karmaGraph) where

import SatO.Karma.Types

karmaGraph :: [Action] -> [(Double, Double)]
karmaGraph _ = signal [0, 0.5 .. 400]
  where
    signal :: [Double] -> [(Double,Double)]
    signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]
