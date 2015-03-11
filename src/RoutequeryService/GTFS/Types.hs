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

{-# LANGUAGE TemplateHaskell,
             ScopedTypeVariables,
             OverloadedStrings #-}

-- | Types representing various GTFS data
module RoutequeryService.GTFS.Types (
  WheelchairBoarding(..),
  Agency(..),
  Stop(..),
  LocationType(..),
  Route(..),
  RouteType(..),
  Trip(..),
  TripDirection(..),
  BikesAllowed(..),
  StopTime(..),
  GTFSTime(..),
  PickupType(..),
  DropoffType(..),
  TimepointInfo(..)
) where

import Control.Applicative (empty)
import Data.Csv
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Text.Read
import Data.List.Split (splitOn)

import RoutequeryService.GTFS.Types.TH

-- * Types

-- Common Enums

$(makeEnum "WheelchairBoarding"
    ["WheelchairUnknown", "WheelchairAvailable", "WheelchairNotAvailable"])

-- Agency

$(makeGTFSType "Agency" [
  Optional "agency_id" ''T.Text,
  Required "agency_name" ''T.Text,
  Required "agency_url" ''T.Text,
  Required "agency_timezone" ''T.Text,
  Optional "agency_lang" ''T.Text,
  Optional "agency_phone" ''T.Text,
  Optional "agency_fare_url" ''T.Text])

-- Stop

$(makeEnum "LocationType" ["StopLocation", "StationLocation"])

$(makeGTFSType "Stop" [
  Required "stop_id" ''T.Text,
  Optional "stop_code" ''T.Text,
  Required "stop_name" ''T.Text,
  Optional "stop_desc" ''T.Text,
  Required "stop_lat" ''Double,
  Required "stop_lon" ''Double,
  Optional "zone_id" ''T.Text,
  Optional "stop_url" ''T.Text,
  Optional "location_type" ''LocationType,
  Optional "parent_station" ''T.Text,
  Optional "stop_timezone" ''T.Text,
  Optional "wheelchair_boarding" ''WheelchairBoarding])

-- Route

$(makeEnum "RouteType"
  ["LightRail", "Subway", "Rail", "Bus", 
   "Ferry", "CableCar", "Gondola", "Funicular"])

$(makeGTFSType "Route" [
  Required "route_id" ''T.Text,
  Optional "agency_id" ''T.Text,
  Required "route_short_name" ''T.Text,
  Required "route_long_name" ''T.Text,
  Optional "route_desc" ''T.Text,
  Required "route_type" ''RouteType,
  Optional "route_url" ''T.Text,
  Optional "route_color" ''T.Text,
  Optional "route_text_color" ''T.Text ])

-- Trip

$(makeEnum "TripDirection" ["ThisDirection", "ThatDirection"])
$(makeEnum "BikesAllowed" ["BikesUnknown", "BikesYes", "BikesNo"])

$(makeGTFSType "Trip" [
  Required "route_id" ''T.Text,
  Required "service_id" ''T.Text,
  Required "trip_id" ''T.Text,
  Optional "trip_headsign" ''T.Text,
  Optional "trip_short_name" ''T.Text,
  Optional "direction_id" ''TripDirection,
  Optional "block_id" ''T.Text,
  Optional "shape_id" ''T.Text,
  Optional "wheelchair_accessible" ''WheelchairBoarding,
  Optional "bikes_allowed" ''BikesAllowed])

-- StopTime

data GTFSTime = GTFSTime {
                  hour :: Int,
                  minute :: Int,
                  second :: Int } deriving (Eq, Ord, Show)

instance FromField GTFSTime where
  parseField f
    | length splitF /= 3 = empty
    | otherwise = case maybeTime of
                    Just t -> return t
                    Nothing -> empty
    where
      f' = map (toEnum . fromEnum) $ BS.unpack f :: [Char]
      splitF = splitOn ":" f'
      maybeTime :: Maybe GTFSTime
      maybeTime = do
        h <- readMaybe $ splitF !! 0
        m <- readMaybe $ splitF !! 1 
        s <- readMaybe $ splitF !! 2
        return $ GTFSTime h m s

$(makeEnum "PickupType" [
  "RegularPickup",
  "NoPickup",
  "PhoneAgencyPickup",
  "CoordinateWithDriverPickup"])

$(makeEnum "DropoffType" [
  "RegularDropoff",
  "NoDropoff",
  "PhoneAgencyDropoff",
  "CoordinateWithDriverDropoff"])

$(makeEnum "TimepointInfo" [
  "ApproximateTimes",
  "ExactTimes"])
   
$(makeGTFSType "StopTime" [
  Required "trip_id" ''T.Text,
  Optional "arrival_time" ''GTFSTime,
  Required "departure_time" ''GTFSTime,
  Required "stop_id" ''T.Text,
  Required "stop_sequence" ''Int,
  Optional "stop_headsign" ''T.Text,
  Optional "pickup_type" ''PickupType,
  Optional "dropoff_type" ''DropoffType,
  Optional "shape_dist_traveled" ''Double,
  Optional "timepoint" ''TimepointInfo])
