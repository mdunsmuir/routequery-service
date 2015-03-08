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

{-# LANGUAGE TemplateHaskell #-}

module RoutequeryService.GTFSRealtime.TH where

import Control.Monad
import Language.Haskell.TH
import Data.Aeson.TH
import Data.SafeCopy

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
  
multiDec :: (Name -> Q [Dec]) -> Q [Dec]
multiDec = liftM concat . forM typeNames

aesonDec = multiDec $ deriveToJSON defaultOptions
safeCopyDec = multiDec $ deriveSafeCopy 0 'base
