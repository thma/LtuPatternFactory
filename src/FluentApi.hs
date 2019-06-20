{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module FluentApi where
import Control.Comonad
import Control.Arrow

{--
class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    duplicate = extend id
  
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate
--}

{--}
instance {-# OVERLAPPING #-} Comonad ((->) Options) where
    extract :: (Options -> a) -> a
    extract builder = builder mempty
    extend :: ((Options -> a) -> b ) ->  (Options -> a) -> (Options -> b)
    extend mutator builder opt2 = mutator (\opt1 -> builder (opt1 ++ opt2))
--}

type Options = [String]

newtype Config = Conf Options deriving (Show)

type ConfigBuilder = Options -> Config

configBuilder :: Options -> Config
configBuilder = Conf

withWarnings' :: ConfigBuilder -> Config
withWarnings' builder = builder ["-Wall"]

withProfiling' :: ConfigBuilder -> Config
withProfiling' builder = builder ["-prof", "-auto-all"]

withOptimization' :: ConfigBuilder -> Config
withOptimization' builder = builder ["-O2"]

withLogging' :: ConfigBuilder -> Config
withLogging' builder = builder ["-logall"]

-- ConfigBuilder -> ConfigBuilder versions
withWarnings :: ConfigBuilder -> (Options -> Config)
withWarnings builder opts = builder (opts ++ ["-Wall"])

withProfiling :: ConfigBuilder -> ConfigBuilder
withProfiling builder opts = builder (opts ++ ["-prof", "-auto-all"])

withOptimization :: ConfigBuilder -> ConfigBuilder
withOptimization builder opts = builder (opts ++ ["-O2"])

withLogging :: ConfigBuilder -> ConfigBuilder
withLogging builder opts = builder (opts ++ ["-logall"])

-- factoring out the option concatenation
withLogging'' :: ConfigBuilder -> ConfigBuilder
withLogging'' builder = extend' builder ["-logall"]

extend' :: ConfigBuilder -> Options -> ConfigBuilder
--extend' builder opts2 = \opts1 -> builder (opts1 ++ opts2)
extend' builder opts2 opts1 = builder (opts1 ++ opts2)

extend'' :: (ConfigBuilder -> Config) -> ConfigBuilder -> ConfigBuilder
extend'' mutator builder opt2 = mutator (\opt1 -> builder (opt1 ++ opt2))

-- extend :: ((Options -> a) -> b ) ->  (Options -> a) -> (Options -> b)
-- extend mutator builder opt2 = mutator (\opt1 -> builder (opt1 ++ opt2))



build :: ConfigBuilder -> Config
build builder = builder []


(#) :: a -> (a -> b) -> b
x # f = f x
infixl 0 #

(#>) :: Comonad w => w a -> (w a -> b) -> w b
x #> f = extend f x
infixl 0 #>

(#>>) :: ConfigBuilder -> (ConfigBuilder -> Config) -> ConfigBuilder
x #>> f = extend'' f x
infixl 0 #>>

data User = User {
      userId :: String
    , name   :: String
    , email  :: String
} deriving Show

emptyUser = User "" "" ""

setId :: String -> User -> User
setId id user = user {userId = id}

setName :: String -> User -> User
setName name user = user {name = name}

setMail :: String -> User -> User
setMail mail user = user {email = mail}

fluentApiDemo :: IO ()
fluentApiDemo = do 
    putStrLn "FluentApi -> Comonad"

    print $ build $ withOptimization $ withProfiling configBuilder

    configBuilder
        #>> withProfiling'
        #>> withOptimization'
        #>> withLogging'
        # build
        # print

    configBuilder
        #> withProfiling'
        #> withOptimization'
        #> withLogging'
        # extract 
        # print
    
    configBuilder    
        # withProfiling
        # withOptimization
        # withLogging
        # withWarnings
        # build
        # print

    emptyUser
        # setId "4610"
        # setName "tom"
        # setMail "tom@haskell.tv"
        # print
