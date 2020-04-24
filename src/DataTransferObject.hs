module DataTransferObject where

import           Codec.Compression.GZip
import           Data.ByteString.Lazy.Char8 hiding (map)
import           Control.Arrow ((>>>)) 

data Album = Album {
    title       :: String
  , publishDate :: Int
  , labelName   :: String
  , artist      :: Artist
} deriving (Show)

data Artist = Artist {
    publicName :: String
  , realName   :: Maybe String
} deriving (Show)

data AlbumDTO = AlbumDTO {
    albumTitle  :: String
  , published   :: Int
  , label       :: String
  , artistName  :: String
} deriving (Show, Read)

toAlbumDTO :: Album -> AlbumDTO
toAlbumDTO Album {title = t, publishDate = d, labelName = l, artist = a} =
  AlbumDTO {albumTitle = t, published = d, label = l, artistName = (publicName a)}

toAlbum :: AlbumDTO -> Album
toAlbum AlbumDTO {albumTitle = t, published = d, label = l, artistName = n} =
  Album {title = t, publishDate = d, labelName = l, artist = Artist {publicName = n, realName = Nothing}}

albums :: [Album]
albums =
        [
          Album {title = "Microgravity",
                 publishDate = 1991,
                 labelName = "Origo Sound",
                 artist = Artist {publicName = "Biosphere", realName = Just "Geir Jenssen"}}
        , Album {title = "Apollo - Atmospheres & Soundtracks",
                 publishDate = 1983,
                 labelName = "Editions EG",
                 artist = Artist {publicName = "Brian Eno", realName = Just "Brian Peter George St. John le Baptiste de la Salle Eno"}}
        ]
        
album1 = albums !! 0
album2 = albums !! 1        

dTODemo :: IO ()
dTODemo = do
  print "DataTransferObject -> Functor"
  print albums

  let albumDTOs = fmap toAlbumDTO albums
  print albumDTOs

  let albums = fmap toAlbum albumDTOs
  print albums
  
  let singlemarshalled = (show . toAlbumDTO) album1
  let gzipped = (compress . pack . show . toAlbumDTO) album1
  
  let marshalled   = fmap (toAlbumDTO >>> show >>> pack >>> compress) albums
  let unmarshalled = fmap (decompress >>> unpack >>> read >>> toAlbum) marshalled

  print marshalled
  print unmarshalled

