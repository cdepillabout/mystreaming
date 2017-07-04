{-# LANGUAGE BangPatterns #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Producer item m res
  = Yield item (Producer item m res)
  | M (m (Producer item m res))
  | Ret res

data Consumer item state
  = Await state (item -> Consumer item state)

(>->) :: Monad m => Producer item m res -> Consumer item state -> m (state, res)
Ret res >-> Await state _ = pure (state, res)
M action >-> consumer = do
  prod <- action
  prod >-> consumer
Yield item prod >-> Await _ consumerFunc =
  let !consumer = consumerFunc item
  in prod >-> consumer

type LineInt = Int
type LineTotal = Int
type RunningTotal = Int

fileProducer :: Producer LineInt IO LineTotal
fileProducer = M . pure . Yield 1 . Yield 2 . Yield 3 $ Ret 6

totalConsumer :: RunningTotal -> Consumer LineInt RunningTotal
totalConsumer !currTotal =
  Await currTotal $ \lineInt -> totalConsumer (currTotal + lineInt)

lala :: IO (RunningTotal, LineTotal)
lala = fileProducer >-> totalConsumer 0
