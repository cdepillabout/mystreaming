{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Lib where

import Control.Comonad (Comonad(..))
import Control.Monad (ap)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------
-- Producer --
--------------

data Producer item m res
  = Yield item (Producer item m res)
  | M (m (Producer item m res))
  | Pure res
  deriving Functor

instance Functor m => Applicative (Producer item m) where
  pure :: a -> Producer item m a
  pure = Pure

  (<*>) :: Producer item m (a -> b) -> Producer item m a -> Producer item m b
  (<*>) = ap

instance Functor m => Monad (Producer item m) where
  (>>=) :: Producer item m a -> (a -> Producer item m b) -> Producer item m b
  Yield item nextProd >>= f = Yield item (nextProd >>= f)
  M action >>= f = M $ fmap (>>= f) action
  Pure res >>= f = f res

yield :: item -> Producer item m ()
yield item = Yield item (Pure ())

--------------
-- Consumer --
--------------

data Consumer item state
  = Await state (item -> Consumer item state)
  deriving Functor

instance Applicative (Consumer item) where
  pure :: state -> Consumer item state
  pure a = Await a (const $ pure a)

  (<*>) :: Consumer item (a -> b) -> Consumer item a -> Consumer item b
  (<*>) = ap

instance Monad (Consumer item) where
  (>>=) :: Consumer item a -> (a -> Consumer item b) -> Consumer item b
  consumer@(Await state next) >>= f = joinConsumer $ fmap f consumer

instance Comonad (Consumer item) where
  extract :: Consumer item state -> state
  extract (Await state _) = state

  extend :: (Consumer item a -> b) -> Consumer item a -> Consumer item b
  extend f consumer@(Await a next) =
    Await (f consumer) (\item -> extend f $ next item)

joinConsumer :: Consumer item (Consumer item state) -> Consumer item state
joinConsumer (Await (Await state _) next) =
  Await state $ \item -> joinConsumer $ fmap (tailConsumer item) $ next item

tailConsumer :: item -> Consumer item state -> Consumer item state
tailConsumer item (Await _ next) = next item

-----------------------------------
-- Connect Producer and Consumer --
-----------------------------------

(>->) :: Monad m => Producer item m res -> Consumer item state -> m (state, res)
Pure res >-> Await state _ = pure (state, res)
M action >-> consumer = do
  prod <- action
  prod >-> consumer
Yield item prod >-> Await _ consumerFunc =
  let !consumer = consumerFunc item
  in prod >-> consumer

---------------------
-- Example program --
---------------------

type LineInt = Int
type LineTotal = Int
type RunningTotal = Int

fileProducer1 :: Producer LineInt IO LineTotal
fileProducer1 = M . pure . Yield 1 . Yield 2 . Yield 3 $ Pure 6

fileProducer2 :: Producer LineInt IO LineTotal
fileProducer2 = do
  yield 1
  yield 2
  yield 3
  pure 6

totalConsumer :: RunningTotal -> Consumer LineInt RunningTotal
totalConsumer !currTotal =
  Await currTotal $ \lineInt -> totalConsumer (currTotal + lineInt)

lala :: IO (RunningTotal, LineTotal)
lala = fileProducer2 >-> totalConsumer 0
