import Control.Applicative
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as B
import Network.HTTP
import Text.ProtocolBuffers.WireMessage
import Com.Google.Transit.Realtime

sampleRequest key
  = getRequest $ "http://api.pugetsound.onebusaway.org/api/gtfs_realtime/trip-updates-for-agency/1.pb?key=" ++ key

loadKey :: IO String
loadKey = trim <$> readFile "key.txt"
  where
    trim :: String -> String
    trim = f . f
       where f = reverse . dropWhile isSpace

main = do
  key <- loadKey
  http <- simpleHTTP (sampleRequest key)
  code <- getResponseCode http
  putStrLn $ show code
  --response <- B.pack <$> getResponseBody http
  --putStrLn $ show response
