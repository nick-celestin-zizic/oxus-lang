{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, FlexibleContexts, LambdaCase #-}
module Util where
import Text.Printf
import Numeric.Natural

type UInt = Natural
type Result a = Either String a

data Location = Location { file :: String
                         , line :: UInt
                         , col  :: UInt
                         } deriving (Eq, Ord)

instance Show Location where
  show Location{file, line, col} = printf "%s:%03d:%03d:" file line col

instance PrintfArg Location where
  formatArg = const . showString . show

dump :: (Show s) => s -> a
dump = error . show


genEnum :: (Enum a) => [a]
genEnum = enumFrom (toEnum 0)
