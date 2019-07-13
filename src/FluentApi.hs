{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module FluentApi where
import Control.Comonad
import Control.Arrow

{--
instance {-# OVERLAPPING #-} Comonad ((->) Options) where
    extract :: (Options -> config) -> config
    extract builder = builder mempty
    extend :: ((Options -> config) -> config') ->  (Options -> config) -> (Options -> config')
    extend withFun builder opt2 = withFun (\opt1 -> builder (opt1 ++ opt2))
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
extend'' withFun builder opt2 = withFun (\opt1 -> builder (opt1 ++ opt2))


build :: ConfigBuilder -> Config
build builder = builder mempty


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
