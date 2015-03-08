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

{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}

module RoutequeryService.GTFSRealtime () where

import Control.Monad
import Data.Serialize
import Data.SafeCopy
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Char
import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Text.ProtocolBuffers.Basic as PB
import qualified Text.ProtocolBuffers.Extensions as PE
import Data.Aeson
import Language.Haskell.TH
import Data.Aeson.TH

import RoutequeryService.GTFSRealtime.TH

import Com.Google.Transit.Realtime.Alert
import Com.Google.Transit.Realtime.Alert.Cause
import Com.Google.Transit.Realtime.Alert.Effect
import Com.Google.Transit.Realtime.EntitySelector
import Com.Google.Transit.Realtime.FeedEntity
import Com.Google.Transit.Realtime.FeedHeader
import Com.Google.Transit.Realtime.FeedHeader.Incrementality
import Com.Google.Transit.Realtime.FeedMessage
import Com.Google.Transit.Realtime.Position
import Com.Google.Transit.Realtime.TimeRange
import Com.Google.Transit.Realtime.TranslatedString
import Com.Google.Transit.Realtime.TranslatedString.Translation
import Com.Google.Transit.Realtime.TripDescriptor
import qualified Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship as TDSR
import Com.Google.Transit.Realtime.TripUpdate
import Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent
import Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate
import qualified Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship as STUSR
import Com.Google.Transit.Realtime.VehicleDescriptor
import Com.Google.Transit.Realtime.VehiclePosition
import Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel
import Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus
import Com.Google.Transit.Realtime.VehiclePosition.VehicleStopStatus

-- * Aeson Instances

instance ToJSON a => ToJSON (S.Seq a) where
  toJSON = toJSON . map toJSON . F.toList

instance ToJSON PB.Utf8 where
  toJSON (PB.Utf8 s) = toJSON s

instance ToJSON PE.ExtField where
  toJSON _ = Null

instance ToJSON PB.ByteString where
  toJSON = toJSON . map (toEnum . fromEnum :: Word8 -> Char) . B.unpack

$aesonDec

-- * Safecopy Instances for Acid

instance SafeCopy PE.ExtField where
  putCopy = (\_ -> contain $ return ())
  getCopy = contain $ return $ PE.ExtField M.empty

$(deriveSafeCopy 0 'base ''PB.Utf8)
$safeCopyDec
