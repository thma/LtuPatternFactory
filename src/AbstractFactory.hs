{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings             #-}
module AbstractFactory where
import GHC.Generics (Generic) -- needed to derive type class instances declaratively
import Data.Aeson   (ToJSON, FromJSON, eitherDecodeFileStrict, toJSON, encodeFile) -- JSON encoding/decoding
import Data.Tagged            -- used to tag type information to Ids

type Id a = Tagged a Integer
data Identified a = Identified
    { ident :: Id a
    , val :: a
    } deriving (Eq, Ord, Read, Show, Generic, ToJSON, FromJSON)

class (ToJSON a, FromJSON a, Eq a, Show a) => Entity a where
    -- | store persistent entity of type a to a json file
    store :: Identified a -> IO ()
    store identified@(Identified id val) = do
        -- compute file path based on entity id
        let jsonFileName = getPath id
        -- serialize entity as JSON and write to file
        encodeFile jsonFileName identified

    -- | load persistent entity of type a and identified by id
    retrieve :: Id a -> IO (Identified a)
    retrieve id = do
        -- compute file path based on id
        let jsonFileName = getPath id
        -- parse entity from JSON file
        eitherEntity <- eitherDecodeFileStrict jsonFileName
        case eitherEntity of
            Left msg -> fail msg
            Right e  -> return e

    -- | compute path of data file
    getPath :: Id a -> String
    getPath id = ".stack-work/" ++ show i ++ ".json"
        where (Tagged i) = id

    -- | publish an entity (e.g. to a message bus, or just print it out)
    publish  :: Identified a -> IO ()
    publish = print

    -- | produce a tagged id
    taggedId :: Integer -> Id a
    taggedId id = Tagged id :: Id a


data User = User {
      name      :: String
    , email     :: String
} deriving (Show, Eq, Generic, ToJSON, FromJSON, Entity)

data Post = Post {
      userId    :: Integer
    , text      :: String
} deriving (Show, Eq, Generic, ToJSON, FromJSON, Entity)

abstractFactoryDemo = do
    putStrLn "AbstractFactory -> type class polymorphism"
    let user = Identified 1 (User "Heinz Meier" "hm@meier.com")
    let post = Identified 4711 (Post 1 "My name is Heinz, this is my first post")
    store user
    store post
    user' <- retrieve (ident user)
    publish user'
    retrieve (ident post) >>= publish
    retrieve (taggedId (userId (val post)) :: Id User) >>= publish