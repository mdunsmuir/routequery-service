module RoutequeryService.GTFS.TH where

import Control.Monad
import Language.Haskell.TH
import Data.Aeson.TH

typeNames = map mkName $ [
  "Alert", "Cause", "Effect",
  "EntitySelector",
  "FeedEntity",
  "FeedHeader",
  "Incrementality",
  "FeedMessage",
  "Position",
  "TimeRange",
  "TranslatedString", "Translation",
  "TripDescriptor", "TDSR.ScheduleRelationship",
  "TripUpdate", "StopTimeEvent", "StopTimeUpdate", "STUSR.ScheduleRelationship",
  "VehicleDescriptor",
  "VehiclePosition", "CongestionLevel", "OccupancyStatus", "VehicleStopStatus"]
  

dec = liftM concat $ forM typeNames (deriveToJSON defaultOptions)
