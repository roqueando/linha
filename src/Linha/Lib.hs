module Linha.Lib where

import Data.List
import Linha.Types

{- | Essa funcao define o modelo por si so

Ela pega como parametro os coeficientes que sao uma tupla de floats: Coefficients (Float, Float), e tambem uma configuracao para o modelo

A funcao consiste em usar um alpha, seu conjunto de dados e um numero de iteracoes para calcular novos parametros (thetas)
-}
linearRegression :: Coefficients -> LinearConfig -> Coefficients
linearRegression cs (LinearConfig _ _ 0 ) = cs
linearRegression cs linearConfig = linearRegression thetas newLinearConfig
  where
    thetas = newTheta cs lr ts
    newLinearConfig = LinearConfig { learningRate = lr, trainingSet = ts, iterations = (it - 1)}
    (LinearConfig lr ts it) = linearConfig

{-| Essa funcao consiste em gerar novos parametros

Ela pega os coeficientes gerados anteriormente (ou os iniciais randomizados), um alpha e o conjunto de dados e gera novos coeficientes

Primeiro ele calcula o delta (a funcao que diferencia x de y)
Depois ele aplica o gradiente descendente para theta0 e theta1
usando o adjustedDeltas para ajustar a diferenca
-}
newTheta :: Coefficients -> LearningRate -> TrainingSet -> Coefficients
newTheta cs lr ts = Coefficients (newt0, newt1)
    where
      deltas = map (calculateDelta cs) ts'
      newt0 = t0 - lr * avg deltas
      newt1 = t1 - lr * avg adjustedDeltas
      adjustedDeltas = adjustDelta deltas ts'
      Coefficients (t0, t1) = cs
      TrainingSet ts' = ts

calculateDelta :: Coefficients -> Datapoint -> Float
calculateDelta thetas dps = t0 + t1 * x - y -- testar com (t0 + t1 * x - y)
  where
    Coefficients (t0, t1) = thetas
    Datapoint (x, y) = dps

adjustDelta :: [Float] -> [Datapoint] -> [Float]
adjustDelta deltas datapoints = map (uncurry (*)) zipped
  where
    xs = map (\(Datapoint (x, _)) -> x) datapoints
    zipped = zip deltas xs

avg :: [Float] -> Float
avg xs = realToFrac (sum xs) / genericLength xs
