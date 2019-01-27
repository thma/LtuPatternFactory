{-# LANGUAGE TypeSynonymInstances, DeriveFunctor #-}
module Objects where
import Control.Comonad

type Option = String
type Builder a = [Option] -> a

newtype Config = MakeConfig [Option] deriving (Show)

configBuilder :: Builder Config
configBuilder = MakeConfig

defaultConfig :: Builder Config
defaultConfig options = MakeConfig ("-Wall" : options)

profile :: Builder Config -> Config
profile builder = builder ["-prof", "-auto-all"]

goFaster :: Builder Config -> Config
goFaster builder = builder ["-O2"]

--instance Functor Builder where
--    fmap = undefined

{--
instance Comonad (Builder a ) b where
    extract = undefined
    extend = undefined

--}
extract' :: Builder a -> a
extract' builder = builder mempty

extend' :: (Builder a -> a) ->  Builder a -> Builder a
extend' setter builder opts2 = setter (\opts1 -> builder (opts1 ++ opts2))


(#) :: a -> (a -> b) -> b
x # f = f x
infixl 0 #

-- 
(°) :: a -> (a -> b) -> b
x ° f = f x
infixl 0 °

(°!) :: Comonad w => w a -> (w a -> b) -> w b
x °! f = extend f x
infixl 0 °!

fluentApiDemo = do 
    putStrLn "Objects -> Comonads"
    defaultConfig
        °! profile 
        °! goFaster
        ° extract 
        ° print

--}