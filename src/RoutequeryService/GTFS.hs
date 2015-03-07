{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}

module RoutequeryService.GTFS () where

import Control.Monad
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Char
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Text.ProtocolBuffers.Basic as PB
import qualified Text.ProtocolBuffers.Extensions as PE
import Data.Aeson
import Language.Haskell.TH
import Data.Aeson.TH

import RoutequeryService.GTFS.TH

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

instance ToJSON a => ToJSON (S.Seq a) where
  toJSON = toJSON . map toJSON . F.toList

instance ToJSON PB.Utf8 where
  toJSON (PB.Utf8 s) = toJSON s

instance ToJSON PE.ExtField where
  toJSON _ = Null

instance ToJSON PB.ByteString where
  toJSON = toJSON . map (toEnum . fromEnum :: Word8 -> Char) . B.unpack

$dec
