{-# LANGUAGE LambdaCase #-}

module Store where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Array.ST (STArray, getBounds, readArray, writeArray)
import Data.Ix (Ix, inRange, range)

newtype Memory x a v = Memory {array :: STArray x a (Maybe v)}

data StoreStatus a = Success a | Failure

class Store s where
  load :: Ix a => a -> s x a v -> ExceptT String (ST x) v
  save :: Ix a => a -> v -> s x a v -> ExceptT String (ST x) ()
  init :: Ix a => s x a v -> ExceptT String (ST x) a

toEitherValue :: Maybe v -> Either String v
toEitherValue Nothing = Left "no value at that address"
toEitherValue (Just val) = Right val

findHole :: Ix a => STArray x a (Maybe v) -> [a] -> ExceptT String (ST x) a
findHole _ [] = throwError "out of memory"
findHole store (curr : rest) =
  lift (readArray store curr) >>= \case
    Nothing -> return curr
    Just _ -> findHole store rest

instance Store Memory where
  load address (Memory store) = do
    bounds <- lift $ getBounds store
    unless
      (inRange bounds address)
      (throwError "address out of range")
    optional <- lift $ readArray store address
    case optional of
      Nothing -> throwError "not value stored at that address"
      Just value -> return value
  save address value (Memory store) = do
    bounds <- lift $ getBounds store
    unless
      (inRange bounds address)
      (throwError "address out of range")
    lift $ writeArray store address (Just value)
  init (Memory store) = lift (getBounds store) >>= (findHole store . range)
