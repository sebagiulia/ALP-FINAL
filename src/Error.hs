module Error where

import Control.Exception

newtype Error = Error String deriving (Show)
instance Exception Error