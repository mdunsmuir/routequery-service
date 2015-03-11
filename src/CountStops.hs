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

{-# LANGUAGE BangPatterns #-}

import Data.Csv
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import RoutequeryService.GTFS.Types

main = do
  stopTimesData <- B.readFile "agencies-gtfs/1/stop_times.txt"
  let !(Right (_, stopTimes)) = 
        decodeByName stopTimesData :: Either String (Header, V.Vector StopTime)
  putStrLn $ "There are " ++ show (V.length stopTimes) ++ " stops!"
