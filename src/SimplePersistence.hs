{-# LANGUAGE ScopedTypeVariables   #-}
module SimplePersistence 
    ( Id
    , Entity
    , getId
    , persist
    , retrieve
    ) where
import           Data.Typeable

-- | Identifier for an Entity
type Id = String

-- | The Entity type class provides generic persistence to txt files
class (Show a, Read a, Typeable a) => Entity a where

    -- | return the unique Id of the entity. This function must be implemented by type class instances.
    getId :: a -> Id

    -- | persist an entity of type a and identified by an Id to a file
    persist :: a -> IO ()
    persist entity = do
        -- compute file path based on entity type and id
        let fileName = getPath (typeOf entity) (getId entity)
        -- serialize entity as JSON and write to file
        writeFile fileName (show entity)

    -- | load persistent entity of type a and identified by an Id
    retrieve :: Id -> IO a
    retrieve id = do
        -- compute file path based on entity type and id
        let fileName = getPath (typeOf (undefined :: a)) id
        -- read file content into string
        contentString <- readFile fileName
        -- parse entity from string
        return (read contentString)
        

-- | compute path of data file
getPath :: TypeRep -> String -> FilePath
getPath tr id = ".stack-work/" ++ show tr ++ "." ++ id ++ ".txt"