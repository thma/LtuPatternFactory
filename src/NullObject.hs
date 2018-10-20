module NullObject where
import           Data.Map (Map, fromList) 
import qualified Data.Map as Map (lookup) -- avoid clash with Prelude.lookup

type Song   = String
type Album  = String
type Artist = String
type URL    = String

songMap :: Map Song Album
songMap = fromList
    [("Baby Satellite","Microgravity")
    ,("An Ending", "Apollo: Atmospheres and Soundtracks")]

albumMap :: Map Album Artist
albumMap = fromList
    [("Microgravity","Biosphere")
    ,("Apollo: Atmospheres and Soundtracks", "Brian Eno")]    

artistMap :: Map Artist URL
artistMap = fromList
    [("Biosphere","http://www.biosphere.no//")
    ,("Brian Eno", "http://www.brian-eno.net")]    

loookup' :: Ord a => Map a b -> a -> Maybe b
loookup' = flip Map.lookup    

findAlbum :: Song -> Maybe Album
findAlbum = loookup' songMap 

findArtist :: Album -> Maybe Artist
findArtist = loookup' albumMap

findWebSite :: Artist -> Maybe URL
findWebSite = loookup' artistMap

findUrlFromSong :: Song -> Maybe URL
findUrlFromSong song = 
    case findAlbum song of
        Nothing    -> Nothing
        Just album -> 
            case findArtist album of
                Nothing     -> Nothing
                Just artist ->
                    case findWebSite artist of
                        Nothing  -> Nothing
                        Just url -> Just url

findUrlFromSongDo :: Song -> Maybe URL
findUrlFromSongDo song = do
    album   <- findAlbum song
    artist  <- findArtist album
    return findWebSite artist

findUrlFromSong' :: Song -> Maybe URL
findUrlFromSong' song =
    findAlbum song   >>= \album ->
    findArtist album >>= \artist ->
    findWebSite artist  

findUrlFromSong'' :: Song -> Maybe URL
findUrlFromSong'' song =
    findAlbum song >>= findArtist >>= findWebSite      

nullObjectDemo = do
    putStrLn "NullObject -> Maybe"

    print $ Map.lookup "Baby Satellite" songMap
    print $ Map.lookup "The Fairy Tale" songMap

    case Map.lookup "Ancient Campfire" songMap of
        Nothing -> print "sorry, could not find your song"
        Just s  -> print s

    print $ findUrlFromSong' "An Ending"
    print $ findUrlFromSong'' "Baby Satellite"

