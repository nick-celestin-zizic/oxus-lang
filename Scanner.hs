{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
module Scanner where
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor.Compose
import Data.Tuple (swap)

import Util

data ScanState i s = ScanState { lhs :: [i]
                               , rhs :: [i]
                               , scanState :: s
                               } deriving (Show, Eq)

-- NOTE this compose shit might be completely useless
newtype ScanResult i s a = ScanResult { _getScanResult :: Either (ScanState i s, String) (ScanState i s, a)
                                      } deriving Functor
getScanResult :: ScanResult i s a -> Result a
getScanResult (ScanResult (Left (_, msg))) = Left msg
getScanResult (ScanResult (Right (_, a))) = Right a
newtype Scanner i s a = Scanner { runScanner :: ScanState i s -> ScanResult i s a }

instance Functor (Scanner i s) where
  fmap f (Scanner scan) = Scanner $ \s -> f <$> (scan s)

instance Applicative (Scanner i s) where
  pure x = Scanner $ \s ->  ScanResult $ Right (s, x)
  (Scanner sc1) <*> (Scanner sc2) = Scanner $ \s -> case sc1 s of
    ScanResult ((Right (s', f))) -> f <$>  (sc2 s')
    ScanResult ((Left failed)) ->  ScanResult (Left failed)

instance Alternative (Scanner i s) where
  empty = Scanner $ \s -> ScanResult (Left (s, "empty scanner"))
  (Scanner sc1) <|> (Scanner sc2) = Scanner $ \s -> case sc1 s of
     ScanResult ((Right good)) ->  ScanResult (Right good)
     ScanResult ((Left _)) -> sc2 s

instance Monad (Scanner i s) where
  (Scanner sc) >>= f = Scanner $ \s -> case sc s of
    ScanResult ((Left fail)) -> ScanResult (Left fail)
    ScanResult ((Right (s', a))) -> runScanner (f a) s'

instance MonadPlus (Scanner i s)

instance MonadState (ScanState i s) (Scanner i s) where
  state action = Scanner (ScanResult . Right . swap . action)

instance Semigroup a => Semigroup (Scanner i s a) where
  p1 <> p2 = do a <- p1
                b <- p2
                return (a <> b)

instance Monoid a => Monoid (Scanner i s a) where
  mempty = return mempty
