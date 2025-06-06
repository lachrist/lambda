{-# LANGUAGE LambdaCase #-}

module Memory (Memory (..), HoleArray (..)) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Array.ST (STArray, getBounds, readArray, writeArray)
import Data.Ix (Ix, inRange, range)

newtype HoleArray x a v = HoleArray {array :: STArray x a (Maybe v)}

class Memory s where
  load :: Ix a => a -> s x a v -> ExceptT String (ST x) v
  save :: Ix a => a -> v -> s x a v -> ExceptT String (ST x) ()
  init :: Ix a => s x a v -> ExceptT String (ST x) a

findHole :: Ix a => STArray x a (Maybe v) -> [a] -> ExceptT String (ST x) a
findHole _ [] = throwError "out of memory"
findHole store (curr : rest) =
  lift (readArray store curr) >>= \case
    Nothing -> return curr
    Just _ -> findHole store rest

instance Memory HoleArray where
  load address (HoleArray store) = do
    bounds <- lift $ getBounds store
    unless
      (inRange bounds address)
      (throwError "address out of range")
    optional <- lift $ readArray store address
    case optional of
      Nothing -> throwError "not value stored at that address"
      Just value -> return value
  save address value (HoleArray store) = do
    bounds <- lift $ getBounds store
    unless
      (inRange bounds address)
      (throwError "address out of range")
    lift $ writeArray store address (Just value)
  init (HoleArray store) = lift (getBounds store) >>= (findHole store . range)
