{-# LANGUAGE OverloadedStrings #-}
module SatO.Karma.Graph (karmaGraph) where

import Control.Monad.Trans.State.Strict (State, get, put, runState)

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

import Numeric.GSL.ODE            (odeSolve)
import Numeric.LinearAlgebra.Data (linspace, toList, toLists)

import SatO.Karma.Types

karmaGraph :: UTCTime -> [Action] -> Graph
karmaGraph _ []   = Graph 0 [(0, 0)] [(0, 0), (30, 0)]
karmaGraph now as =
    let ps = zip as (tail as ++ [pseudoAction])
        (parts, curr) = runState (traverse calcPart ps) 0
        (next, _) = part curr (0, 30)
    in Graph curr (concat parts) next
 where
    pseudoAction :: Action
    pseudoAction = Action "" Coffee now

    calcPart :: (Action, Action) -> State Double [(Double, Double)]
    calcPart (a, b) = do
        curr <- get
        let a' = diffTimeToDays $ diffUTCTime (_actionStamp a) now
        let b' = diffTimeToDays $ diffUTCTime (_actionStamp b) now
        let (xs, x) = part (curr + actionValue a) (a', b')
        put x
        return $ (a', curr) : xs

actionValue :: Action -> Double
actionValue (Action _ Coffee _) = 0.3
actionValue _      = 1.0

diffTimeToDays :: NominalDiffTime -> Double
diffTimeToDays = fromRational . (/86400) . toRational

part :: Double -> (Double, Double) -> ([(Double, Double)], Double)
part start interval =
    let ini  = [start]
        ts   = linspace 100 interval
        ts' =  toList ts
        sol  = concat $ toLists $ odeSolve model ini ts
        l    = last sol
    in (zip ts' sol, l)

model :: Double -> [Double] -> [Double]
model _ = fmap (negate . decay)

decay :: Double -> Double
decay x = (k1 * exp (negate k2 * x ^ (3 :: Int)) + log (x + 1) - k1) * 1.5
  where
    k1 = 0.753395
    k2 = 2.37257
