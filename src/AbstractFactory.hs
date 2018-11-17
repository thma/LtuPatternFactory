{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings         #-}
module AbstractFactory where
import           GHC.Generics    (Generic)              -- needed to derive type class instances declaratively
import           Data.Aeson      (ToJSON, FromJSON, eitherDecode, toJSON)     -- needed to provide JSON encoding/decoding
import           Data.Text
import           Data.Aeson.Text (encodeToTextBuilder)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.ByteString.Lazy as B (readFile)
import           Data.Typeable (Typeable, TypeRep, typeRep) -- runtime type reflection

data User = User Integer String String String deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

data Post = Post Integer Integer String deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

-- | load persistent entity of type a and identified by id
retrieveEntity :: forall a. (FromJSON a, Read a, Typeable a) => Text -> IO a
retrieveEntity id = do
    -- compute file path based on type and id
    let jsonFileName = getPath (typeRep ([] :: [a])) id
    parseFromJsonFile jsonFileName :: FromJSON a => IO a

-- | store persistent entity of type a and identified by id to the backend
storeEntity :: forall a. (ToJSON a, Show a, Typeable a) => Text -> a -> IO ()
storeEntity id entity = do
  let jsonFileName = getPath (typeRep ([] :: [a])) id
  writeFile jsonFileName (showJson entity)
    where
        -- create a JSON representation of an entity
        showJson :: (ToJSON a) => a -> String
        showJson = unpack . toStrict . toLazyText . encodeToTextBuilder . toJSON

-- | compute path of data file
getPath :: TypeRep -> Text -> String
getPath tr id = ".stack-work/" ++ show tr ++ "." ++ unpack id ++ ".json"

-- | read from file fileName and then parse the contents as a FromJSON instance.
parseFromJsonFile :: FromJSON a => FilePath -> IO a
parseFromJsonFile fileName = do
    contentBytes <- B.readFile fileName
    case eitherDecode contentBytes of
        Left msg -> fail msg
        Right x  -> return x

abstractFactoryDemo = do
    putStrLn "AbstractFactory -> type class polymorphism"
    let user1 = User 4711 "Heinz" "Meier" "hm@meier.com"
    let post1 = Post 1 4711 "This is my first post"
    storeEntity "4711" user1
    storeEntity "1" post1
    user2 <- retrieveEntity "4711" -- 
    if user1 == user2 
        then
            putStrLn "user data successfully restored"
        else putStrLn "user data restore failed"
    print user2
    post2 <- retrieveEntity "1" :: IO Post
    print post2