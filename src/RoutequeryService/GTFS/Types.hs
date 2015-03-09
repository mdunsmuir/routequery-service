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

{-# LANGUAGE ScopedTypeVariables,
             OverloadedStrings #-}

-- | Types representing various GTFS data
module RoutequeryService.GTFS.Types where

import Control.Applicative
import Data.Csv
import Text.Read
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Text as T

-- * Types

-- | Type representing an agency (from agency.txt)
data Agency = Agency {
  agencyId :: !(Maybe T.Text),
  agencyName :: !T.Text,
  agencyUrl :: !T.Text,
  agencyTimezone :: !T.Text,
  agencyLanguage :: !(Maybe T.Text),
  agencyPhone :: !(Maybe T.Text),
  agencyFareUrl :: !(Maybe T.Text) } deriving Show

instance FromNamedRecord Agency where
  parseNamedRecord m =
    Agency <$>
    m .: "agency_id" <*>
    m .: "agency_name" <*>
    m .: "agency_url" <*>
    m .: "agency_timezone" <*>
    m .: "agency_lang" <*>
    m .: "agency_phone" <*>
    m .: "agency_fare_url"

-- | Type representing a stop (from stops.txt)
data Stop = Stop {
  stopId :: !T.Text,
  stopCode :: !(Maybe T.Text),
  stopName :: !T.Text,
  stopDescription :: !(Maybe T.Text),
  stopLatitude :: !Double,
  stopLongitude :: !Double,
  stopZoneId :: !(Maybe T.Text),
  stopUrl :: !(Maybe T.Text),
  stoplocationType :: !(Maybe LocationType),
  stopParentStation :: !(Maybe T.Text),
  stopTimezone :: !(Maybe T.Text),
  stopWheelchairBoarding :: !(Maybe Bool) } deriving Show

instance FromNamedRecord Stop where
  parseNamedRecord m =
    Stop <$>
    m .: "stop_id" <*>
    m .: "stop_code" <*>
    m .: "stop_name" <*>
    m .: "stop_desc" <*>
    m .: "stop_lat" <*>
    m .: "stop_lon" <*>
    m .: "zone_id" <*>
    m .: "stop_url" <*>
    m .: "location_type" <*>
    m .: "parent_station" <*>
    m .: "stop_timezone" <*>
    m .: "wheelchair_boarding"

enumFromField :: forall a. (Enum a, Bounded a) => BS.ByteString -> Parser a
enumFromField s
  | readValue < 0 = empty
  | readValue > fromEnum (maxBound :: a) = empty
  | otherwise = return $ toEnum readValue
  where 
    readValue = case readMaybe $ map (toEnum . fromEnum) $ BS.unpack s of
      Just n -> n
      Nothing -> (-1)
  
data LocationType = StopLocation | StationLocation deriving (Enum, Bounded, Show)

instance FromField LocationType where
  parseField = enumFromField 

instance FromField Bool where
  parseField "0" = return False
  parseField "1" = return True
  parseField _ = empty
