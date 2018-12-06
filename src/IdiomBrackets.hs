{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module IdiomBrackets where

-- This module provides the Idiom Bracket syntax suggested by Conor McBride
-- 'iI f a b ... Ii' stands for '[[f a b ...]]' which denotes 'pure f <*> a <*> b <*> ...'
-- See also https://wiki.haskell.org/Idiom_brackets

class Applicative i => Idiomatic i f g | g -> f i where
    idiomatic :: i f -> g

iI :: Idiomatic i f g => f -> g
iI = idiomatic . pure

data Ii  =  Ii

instance Applicative i   => Idiomatic i x (Ii -> i x) where
    idiomatic xi Ii  = xi

instance Idiomatic i f g => Idiomatic i (s -> f) (i s -> g) where
    idiomatic sfi si = idiomatic (sfi <*> si)
