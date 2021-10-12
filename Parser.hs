{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances#-}
module Parser where

import Data.Functor.Compose
import Text.Printf

import qualified Control.Monad.Fail as F

import Scanner
import Util

type Parser = Scanner Char Location
instance F.MonadFail Parser where
  fail msg = Scanner $ \s@ScanState{scanState} ->
    Compose $ Left (s, printf "%s '%s'" scanState msg)
