{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
module Objects where
--import Control.Comonad
import Data.Char
import Builder

type Option = String

newtype Config = Conf [Option] deriving (Show)

configBuilder :: [Option] -> Config
configBuilder = Conf

defaultConfig :: [Option] -> Config
defaultConfig options = Conf ("-Wall" : options)

profile :: ([Option] -> Config) -> Config
profile builder = builder ["-prof", "-auto-all"]

optimize :: ([Option] -> Config) -> Config
optimize builder = builder ["-O2"]

logall :: ([Option] -> Config) -> Config
logall builder = builder ["-logall"]

toUpper' :: ([Option] -> Config) -> Config
toUpper' builder = 
    let Conf options = builder []
    in Conf (map (map toUpper) options)


class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    duplicate = extend id
  
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate


instance Comonad ((->) [Option]) where
    extract :: ([Option] -> a) -> a
    extract builder = builder mempty
    extend :: (([Option] -> a) -> b ) ->  ([Option] -> a) -> ([Option] -> b)
    extend setter builder o2 = setter (\o1 -> builder (o1 ++ o2))


(#) :: a -> (a -> b) -> b
x # f = f x
infixl 0 #


(#>) :: Comonad w => w a -> (w a -> b) -> w b
x #> f = extend f x
infixl 0 #>

setName :: String -> BankAccount -> BankAccount
setName name account = account {name=name}

setBranch :: String -> BankAccount -> BankAccount
setBranch branch account = account {branch=branch}

fluentApiDemo :: IO ()
fluentApiDemo = do 
    putStrLn "FluentApi> Comonads"
    defaultConfig
        #> profile 
        #> optimize
        #> toUpper'
        #> logall
        # extract 
        # print
{--    (buildAccount 7)
        #> \x -> setName "Test Account"
        #> \y -> setName "Real Name"
        #> \z -> setBranch "my bank"
        # extract
        # print
--}