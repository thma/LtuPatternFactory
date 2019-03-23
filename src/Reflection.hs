{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
module Reflection where

import           JsonPersistence (Id, Entity, store, retrieve)
import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data User = User {
      name  :: String
    , email :: String
} deriving (Show, Eq, Generic, ToJSON, FromJSON, Entity)

data Post = Post {
      userId :: Id
    , text   :: String
} deriving (Show, Eq, Generic, ToJSON, FromJSON, Entity)

retrieveUser :: Id -> IO User
retrieveUser = retrieve

retrievePost :: Id -> IO Post
retrievePost = retrieve

reflectionDemo = do
    putStrLn "Reflection"
    let user = User "Heinz Meier" "hm@meier.com"
    let post = Post "1" "My name is Heinz, this is my first post"

    store "1" user
    store "4711" post

    user' <- retrieve "1" :: IO User
    user' <- retrieveUser "1"
    print user'

    retrievePost "4711" >>= print

