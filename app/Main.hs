module Main where

import qualified Linha.Lib as L
import qualified Linha.Types as T

inference :: Float -> T.Coefficients -> Float
inference x cs = t0 + t1 * x
  where
    T.Coefficients (t0, t1) = cs

main :: IO ()
main = do
  let thetas = T.Coefficients (0, 0)
  let lr = 0.3
  let ts = T.TrainingSet [T.Datapoint (0, 50), T.Datapoint (1, 60), T.Datapoint (2, 70)]
  let iters = 500
  let lc = T.LinearConfig { T.learningRate = lr, T.trainingSet = ts, T.iterations = iters }
  let cs = L.linearRegression thetas lc
  let found = inference 3 cs
  print $ show cs
  print $ show found

