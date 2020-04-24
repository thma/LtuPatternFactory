{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
module Proxy where

import Data.Char as C

type RealSubject = String

newtype Proxy a = Proxy a deriving (Show, Read, Functor)

instance (Num a) => Num (Proxy a) where
  (Proxy x) + (Proxy y) = Proxy (x + y)
  (Proxy x) * (Proxy y) = Proxy (x * y)
  abs (Proxy x)         = Proxy (abs x)
  signum (Proxy x)      = Proxy (signum x)
  fromInteger           = Proxy . fromInteger
  negate (Proxy x)      = Proxy (negate x)

upper :: RealSubject -> RealSubject
upper = map C.toUpper

demoProxy = do
  putStrLn "Proxy -> Functor"

  let realX = 7
      realY = 19
      proxyX = Proxy realX
      proxyY = Proxy realY
  print (proxyX + proxyY)
  --print $ fmap (upper . reverse) proxy


