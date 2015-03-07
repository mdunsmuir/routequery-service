import Control.Applicative
import Data.Char (isSpace)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Network.HTTP
import Text.ProtocolBuffers.WireMessage
import Com.Google.Transit.Realtime.FeedMessage (FeedMessage)

import RoutequeryService.GTFS

main = do
  writeJsonForRequest "http://api.pugetsound.onebusaway.org/api/gtfs_realtime/trip-updates-for-agency/1.pb?key=" "trip_updates"
  writeJsonForRequest "http://api.pugetsound.onebusaway.org/api/gtfs_realtime/vehicle-positions-for-agency/1.pb?key=" "vehicle_positions"
 
writeJsonForRequest request name = do
  key <- loadKey
  http <- simpleHTTP (getRequest (request ++ key))
  message <- B.pack <$> getResponseBody http
  let getted = messageGet message :: Either String (FeedMessage, B.ByteString)
  case getted of
    Left error -> putStrLn error
    Right (update, _) -> B.writeFile (name ++ ".json") (encode update)

loadKey :: IO String
loadKey = trim <$> readFile "key.txt"
  where
    trim :: String -> String
    trim = f . f
       where f = reverse . dropWhile isSpace
