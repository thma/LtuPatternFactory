{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
module JsonPersistence where
import           Data.Aeson   (FromJSON, ToJSON, eitherDecodeFileStrict,
                               encodeFile, toJSON)
--import           Data.Tagged
import           GHC.Generics (Generic)

type Id = String

class (ToJSON a, FromJSON a, Eq a, Show a) => Entity a where
    -- | store persistent entity of type a and identified by an Id to a json file
    store :: Id -> a -> IO ()
    store id entity = do
        -- compute file path based on entity id
        let jsonFileName = getPath id
        -- serialize entity as JSON and write to file
        encodeFile jsonFileName entity

    -- | load persistent entity of type a and identified by id
    retrieve :: Id -> IO a
    retrieve id = do
        -- compute file path based on id
        let jsonFileName = getPath id
        -- parse entity from JSON file
        eitherEntity <- eitherDecodeFileStrict jsonFileName
        case eitherEntity of
            Left msg -> fail msg
            Right e  -> return e

    -- | publish an entity (e.g. to a message bus, or just print it out)
    publish  :: a -> IO ()
    publish = print

-- | compute path of data file
getPath :: Id -> String
getPath id = ".stack-work/" ++ id ++ ".json"

data User = User {
      name  :: String
    , email :: String
} deriving (Show, Eq, Generic, ToJSON, FromJSON, Entity)

data Post = Post {
      userId :: Id
    , text   :: String
} deriving (Show, Eq, Generic, ToJSON, FromJSON, Entity)

jsonPersistenceDemo = do
    putStrLn "JsonPersistence"
    let user = User "Heinz Meier" "hm@meier.com"
    let post = Post "1" "My name is Heinz, this is my first post"
    store "1" user
    store "4711" post
    user' <- retrieve "1" :: IO User
    publish user'
    (retrieve "4711" :: IO Post) >>= publish

