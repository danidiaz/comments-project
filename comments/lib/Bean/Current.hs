
module Bean.Current (Current (..)) where

newtype Current v = Current {askCurrent :: IO v}

