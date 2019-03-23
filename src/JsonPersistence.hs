{-# LANGUAGE ScopedTypeVariables   #-}
module JsonPersistence 
    ( Id
    , Entity
    , store
    , retrieve
    ) where
import           Data.Aeson   (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile, toJSON)
import           Data.Typeable

-- | Identifier for an Entity
type Id = String

-- | The Entity type class provides generic persistence to JSON files
class (ToJSON a, FromJSON a, Eq a, Show a, Typeable a) => Entity a where

    -- | store entity of type a and identified by an Id to a json file
    store :: Id -> a -> IO ()
    store id entity = do
        -- compute file path based on concrete type and entity id
        let jsonFileName = getPath (typeRep ([] :: [a])) id
        -- serialize entity as JSON and write to file
        encodeFile jsonFileName entity

    -- | load persistent entity of type a and identified by an Id
    retrieve :: Id -> IO a
    retrieve id = do
        -- compute file path based on entity type and entity id
        let jsonFileName = getPath (typeRep ([] :: [a])) id
        -- parse entity from JSON file
        eitherEntity <- eitherDecodeFileStrict jsonFileName
        case eitherEntity of
            Left msg -> fail msg
            Right e  -> return e

-- | compute path of data file
getPath :: TypeRep -> String -> String
getPath tr id = ".stack-work/" ++ show tr ++ "." ++ id ++ ".json"