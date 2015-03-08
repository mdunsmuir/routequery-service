{-
    Copyright (C) 2015  Michael Dunsmuir

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.Reader
import Data.Aeson
import Data.SafeCopy
import Data.Acid
import Data.Typeable
import Data.Char (isSpace)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import Network.HTTP
import Text.ProtocolBuffers.WireMessage
import Com.Google.Transit.Realtime.FeedMessage

import RoutequeryService.GTFSRealtime
import Com.Google.Transit.Realtime.FeedEntity
import Com.Google.Transit.Realtime.TripUpdate

data TripUpdateMap = TripUpdateMap (M.Map Integer [TripUpdate]) deriving Typeable

insertTripUpdate :: TripUpdate -> Update TripUpdateMap ()
insertTripUpdate tu@(TripUpdate _ _ _ (Just timestamp) _ _) = do
  TripUpdateMap map <- get
  let map' = M.insertWith (++) timestamp' [tu] map
  put $ TripUpdateMap map'
  where timestamp' = toInteger timestamp
insertTripUpdate _ = return ()

tripUpdatesForTimestamp :: Integer -> Query TripUpdateMap [TripUpdate]
tripUpdatesForTimestamp ts = do
  TripUpdateMap map <- ask
  return $ M.findWithDefault [] ts map

getMap :: Query TripUpdateMap (M.Map Integer [TripUpdate])
getMap = do
  TripUpdateMap map <- ask
  return map

$(deriveSafeCopy 0 'base ''TripUpdateMap)
$(makeAcidic ''TripUpdateMap ['insertTripUpdate, 'tripUpdatesForTimestamp, 'getMap])
  
loadKey :: IO String
loadKey = trim <$> readFile "key.txt"
  where
    trim :: String -> String
    trim = f . f
       where f = reverse . dropWhile isSpace

requestWithKey
  = ("http://api.pugetsound.onebusaway.org/api/gtfs_realtime/trip-updates-for-agency/1.pb?key=" ++)

main = do
  key <- loadKey
  http <- simpleHTTP (getRequest (requestWithKey key))
  message <- B.pack <$> getResponseBody http
  let getted = messageGet message :: Either String (FeedMessage, B.ByteString)
  case getted of
    Left error -> putStrLn error
    Right (message, _) -> storeUpdates message

storeUpdates :: FeedMessage -> IO ()
storeUpdates (FeedMessage _ entities _) = do
  let updates = map trip_update $ F.toList entities
  acid <- openLocalState $ TripUpdateMap M.empty
  F.forM_ updates $ \tripUpdate -> case tripUpdate of
                                     (Just tripUpdate) -> update acid $ InsertTripUpdate tripUpdate
                                     Nothing -> return ()
  updates <- query acid $ TripUpdatesForTimestamp 1425790759
  B.putStrLn $ encode updates
  
