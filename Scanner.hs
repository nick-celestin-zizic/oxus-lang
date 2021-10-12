{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances #-}
module Scanner where
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor.Compose
import Data.Tuple (swap)

data ScanState i s = ScanState { lhs :: [i]
                               , rhs :: [i]
                               , scanState :: s
                               } deriving (Show, Eq)

-- NOTE this compose shit might be completely useless
type ScanResult i s a = Compose (Either (ScanState i s, String)) ((,) (ScanState i s)) a
newtype Scanner i s a = Scanner { runScanner :: ScanState i s -> ScanResult i s a }

instance Functor (Scanner i s) where
  fmap f (Scanner scan) = Scanner $ \s -> fmap f (scan s)

instance Applicative (Scanner i s) where
  pure x = Scanner $ \s -> Compose (Right (s, x))
  (Scanner sc1) <*> (Scanner sc2) = Scanner $ \s -> case sc1 s of
    Compose (Right (s', f)) -> fmap f (sc2 s')
    Compose (Left failed) -> Compose (Left failed)

instance Alternative (Scanner i s) where
  empty = Scanner $ \s -> Compose (Left (s, "empty scanner"))
  (Scanner sc1) <|> (Scanner sc2) = Scanner $ \s -> case sc1 s of
    Compose (Right good) -> Compose (Right good)
    Compose (Left _) -> sc2 s

instance Monad (Scanner i s) where
  (Scanner sc) >>= f = Scanner $ \s -> case sc s of
    Compose (Left fail) -> Compose (Left fail)
    Compose (Right (s', a)) -> runScanner (f a) s'

instance MonadPlus (Scanner i s)

instance MonadState (ScanState i s) (Scanner i s) where
  state action = Scanner (Compose . Right . swap . action)

instance Semigroup a => Semigroup (Scanner i s a) where
  p1 <> p2 = do a <- p1
                b <- p2
                return (a <> b)

instance Monoid a => Monoid (Scanner i s a) where
  mempty = return mempty
