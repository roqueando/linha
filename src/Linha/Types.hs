module Linha.Types  where

{- thetax is the parameter that the model will find for us -}
{- Coeficients will be a wrapper for theta0 and theta1 -}
{- can be called Thetas but I'm from brazil and my 5th grade not help -}
newtype Coefficients =
  Coefficients (Float, Float)
  deriving (Show)

{- it's self explanatory -}
newtype Datapoint = Datapoint (Float, Float)

{- list of datapoints -}
newtype TrainingSet = TrainingSet [Datapoint]

data LinearConfig = LinearConfig
  {
    learningRate :: Float,
    trainingSet :: TrainingSet,
    iterations :: Int
  }

type LearningRate = Float
