module SimplePersistence 
    ( Id
    , Entity
    , getId
    , persist
    , retrieve
    ) where
import           Text.Read

-- | Identifier for an Entity
type Id = String

-- | The Entity type class provides generic persistence to JSON files
class (Show a, Read a) => Entity a where

    -- | return the unique Id of the entity. This function must be implemented by type class instances.
    getId :: a -> Id

    -- | persist an entity of type a and identified by an Id to a json file
    persist :: a -> IO ()
    persist entity = do
        -- compute file path based on entity id
        let fileName = getPath (getId entity)
        -- serialize entity as JSON and write to file
        writeFile fileName (show entity)

    -- | load persistent entity of type a and identified by an Id
    retrieve :: Id -> IO a
    retrieve id = do
        -- compute file path based on entity id
        let fileName = getPath id
        -- read file content into string
        contentString <- readFile fileName
        -- parse entity from string
        let eitherEntity = readEither contentString
        case eitherEntity of
            Left msg -> fail msg
            Right e  -> return e

-- | compute path of data file
getPath :: String -> String
getPath id = ".stack-work/" ++ id ++ ".txt"