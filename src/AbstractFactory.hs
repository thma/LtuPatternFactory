{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings             #-}
module AbstractFactory where
import           GHC.Generics (Generic) -- needed to derive type class instances declaratively
import           Data.Aeson   (ToJSON, FromJSON, eitherDecodeFileStrict, toJSON, encodeFile) -- JSON encoding/decoding
import           Data.UUID
import           System.Random

class (ToJSON e, FromJSON e, Eq e) => Entity e where 
    getId :: e -> UUID

data User = User {
      userId    :: UUID
    , firstName :: String
    , lastName  :: String
    , email     :: String
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Entity User where
    getId = userId 

data Post = Post {
      postId :: UUID
    , user   :: UUID
    , text   :: String
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Entity Post where
    getId = postId 

-- | load persistent entity of type a and identified by id
retrieveEntity :: (Entity a) => UUID -> IO a
retrieveEntity id = do
    -- compute file path based on id
    let jsonFileName = getPath id
    -- parse entity from JSON file
    eitherEntity <- eitherDecodeFileStrict jsonFileName
    case eitherEntity of
        Left msg -> fail msg
        Right e  -> return e

-- | store persistent entity of type a to a json file
storeEntity :: (Entity a) => a -> IO ()
storeEntity entity = do
    -- compute file path based on entity id
    let jsonFileName = getPath (getId entity)
    -- serialize entity as JSON and write to file
    encodeFile jsonFileName entity

-- | compute path of data file
getPath :: UUID -> String
getPath id = ".stack-work/" ++ toString id ++ ".json"

-- | compute random UUID        
newUUID :: IO UUID
newUUID = randomIO

abstractFactoryDemo = do
    putStrLn "AbstractFactory -> type class polymorphism"
    idUser <- newUUID
    idPost <- newUUID
    let user1 = User idUser "Heinz" "Meier" "hm@meier.com"
    let post1 = Post idPost idUser "My name is Heinz, this is my first post"
    storeEntity user1
    storeEntity post1
    user2 <- retrieveEntity idUser -- 
    if user1 == user2 
        then putStrLn "user data successfully restored"
        else putStrLn "user data restore failed"
    print user2
    post2 <- retrieveEntity idPost :: IO Post
    print post2
    