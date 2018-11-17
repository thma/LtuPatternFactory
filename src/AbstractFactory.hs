{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings             #-}
module AbstractFactory where
import           GHC.Generics              (Generic) -- needed to derive type class instances declaratively
import           Data.Aeson                (ToJSON, FromJSON, eitherDecode, toJSON) -- JSON encoding/decoding
import           Data.Text                 (unpack)
import           Data.Aeson.Text           (encodeToTextBuilder)
import           Data.Text.Lazy            (toStrict)
import           Data.Text.Lazy.Builder    (toLazyText)
import qualified Data.ByteString.Lazy as B (readFile)
import           Data.UUID
import           System.Random

class (ToJSON e, FromJSON e) => Entity e where 
    getId :: e -> UUID

data User = User {
      userId    :: UUID
    , firstName :: String
    , lastName  :: String
    , email     :: String
} deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Entity User where
    getId = userId 

data Post = Post {
      postId :: UUID
    , user   :: UUID
    , text   :: String
} deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Entity Post where
    getId = postId 

-- | load persistent entity of type a and identified by id
retrieveEntity :: (Entity a) => UUID -> IO a
retrieveEntity id = do
    -- compute file path based on type and id
    let jsonFileName = getPath id
    parseFromJsonFile jsonFileName

-- | store persistent entity of type a to a json file
storeEntity :: (Entity a) => a -> IO ()
storeEntity entity = do
    let jsonFileName = getPath (getId entity)
    writeFile jsonFileName (showJson entity) where
        -- create a JSON representation of an entity
        showJson :: (ToJSON a) => a -> String
        showJson = unpack . toStrict . toLazyText . encodeToTextBuilder . toJSON

-- | compute random UUID        
newUUID :: IO UUID
newUUID = randomIO

-- | compute path of data file
getPath :: UUID -> String
getPath id = ".stack-work/" ++ toString id ++ ".json"

-- | read from file fileName and then parse the contents as an Entity instance.
parseFromJsonFile :: Entity a => FilePath -> IO a
parseFromJsonFile fileName = do
    contentBytes <- B.readFile fileName
    case eitherDecode contentBytes of
        Left msg -> fail msg
        Right x  -> return x

abstractFactoryDemo = do
    putStrLn "AbstractFactory -> type class polymorphism"
    idUser <- newUUID
    idPost <- newUUID
    let user1 = User {userId = idUser, firstName = "Heinz", lastName = "Meier", email = "hm@meier.com"}
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