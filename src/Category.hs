module Category where

class Category arr where
    id  :: arr a a
    (.) :: arr b c -> arr a b -> arr a c
