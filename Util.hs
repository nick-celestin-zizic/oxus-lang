{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances#-}
module Util where
import Text.Printf
import Numeric.Natural

type Result a = Either String a

data Location = Location { file :: String
                         , line :: Natural
                         , col  :: Natural
                         } deriving (Eq, Ord)

instance Show Location where
  show Location{file, line, col} = printf "%s:%03d:%03d:" file line col

instance PrintfArg Location where
  formatArg = const . showString . show
