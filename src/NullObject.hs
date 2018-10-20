module NullObject where
import           Data.Map    
import qualified Data.Map as Map 

type Song   = String
type Album  = String
type Artist = String
type URL    = String

songDB :: Map Song Album
songDB =   Map.fromList
    [("no woman no cry","Kaya")]

findAlbum :: Song -> Maybe Album
findAlbum song = Map.lookup song songDB

nullObjectDemo = do
    putStrLn "NullObject -> Maybe"
    print $ findAlbum "no woman no cry"
    print $ findAlbum "a ram sam sam"