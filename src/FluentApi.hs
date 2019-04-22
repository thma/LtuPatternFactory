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

{--
instance {-# OVERLAPPING #-} Comonad ((->) Options) where
    extract :: (Options -> a) -> a
    extract builder = builder mempty
    extend :: ((Options -> a) -> b ) ->  (Options -> a) -> (Options -> b)
    extend setter builder opt2 = setter (\opt1 -> builder (opt1 ++ opt2))
--}

type Options = [String]

newtype Config = Conf Options deriving (Show)

type ConfigBuilder = Options -> Config

configBuilder :: Options -> Config
configBuilder = Conf

withWarnings :: ConfigBuilder -> Config
withWarnings builder = builder ["-Wall"]

withProfiling :: ConfigBuilder -> Config
withProfiling builder = builder ["-prof", "-auto-all"]

withOptimization :: ConfigBuilder -> Config
withOptimization builder = builder ["-O2"]

withLogging :: ConfigBuilder -> Config
withLogging builder = builder ["-logall"]

(#) :: a -> (a -> b) -> b
x # f = f x
infixl 0 #

(#>) :: Comonad w => w a -> (w a -> b) -> w b
x #> f = extend f x
infixl 0 #>

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
    print . extract . extend withOptimization . extend withLogging . extend withWarnings $ configBuilder

    -- extend withWarnings $ configBuilder >>> extend withLogging >>> extend withOptimization >>> extract >>> print

    configBuilder
        #> withProfiling
        #> withOptimization
        #> withLogging
        # extract 
        # print

    emptyUser
        # setId "4610"
        # setName "tom"
        # setMail "tom@haskell.tv"
        # print
