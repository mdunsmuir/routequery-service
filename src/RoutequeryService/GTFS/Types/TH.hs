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
             ScopedTypeVariables #-}

module RoutequeryService.GTFS.Types.TH (
  makeEnum, 
  makeGTFSType,
  DataColumn(..)
) where

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Text.Read
import qualified Data.ByteString as BS
import Data.Csv hiding (Name)
import Data.List (isPrefixOf)
import Data.Char (toLower)
import Cases
import Data.Text (pack, unpack)

-- | Type for specifying the structure of a GTFS CSV file and associated type
data DataColumn = Required { 
                    dataColumnName :: String,
                    dataColumnType :: Name }
                | Optional {
                    dataColumnName :: String,
                    dataColumnType :: Name }

-- | Create a GTFS feed type
makeGTFSType :: String -> [DataColumn] -> Q [Dec]
makeGTFSType nameStr cols = do
  typeName <- newName nameStr
  consName <- newName nameStr

  let cols' = map (colToCon nameStr) cols
      constructor = recC consName cols'

  typeDec <- dataD (return []) typeName [] [constructor] [(mkName "Show")]
  instDec <- makeFromNamedRecordInstance typeName consName cols
  return [typeDec, instDec]

-- ok so I'm not so good at template haskell

colToCon :: String -> DataColumn -> Q (Name, Strict, Type)
colToCon typeNameStr col =
  let t = case col of
            Required _ _ -> ConT typeName
            Optional _ _ -> AppT (ConT ''Maybe) (ConT typeName)
      nameStr = dataColumnName col
      typeName = dataColumnType col
      typeNameStr' = map toLower typeNameStr
      nameStr' = unpack $ camelize $ pack $ 
        if typeNameStr' `isPrefixOf` nameStr
          then nameStr
          else typeNameStr' ++ "_" ++ nameStr
  in  return (mkName nameStr', IsStrict, t)

-- :(

makeFromNamedRecordInstance :: Name -> Name -> [DataColumn] -> Q Dec
makeFromNamedRecordInstance typeName consName cols = do
  recName <- newName "m"
  let instanceAppT = appT (conT ''FromNamedRecord) (conT typeName)
      bodyExp = applicativeExpr consName recName (reverse cols)
      instanceDecClause = clause [varP recName] (normalB bodyExp) []
      instanceDec = funD 'parseNamedRecord [instanceDecClause]

  instDec <- instanceD (return []) instanceAppT [instanceDec]
  return instDec

  where
    applicativeExpr consName _ [] = conE consName
    applicativeExpr consName recName [c] =
      [| $(conE consName) <$> $(subExpr recName c) |]
    applicativeExpr consName recName (c:cs) =
      [| $(applicativeExpr consName recName cs) <*> $(subExpr recName c) |]

    subExpr recName (Required nameStr _) =
      [| $(varE recName) .: $(litE $ stringL nameStr) |]
    subExpr recName (Optional nameStr _) =
      [| $(varE recName) .:: $(litE $ stringL nameStr) |]

-- | Function to help with the definition of FromField instances
enumFromField :: forall a. (Enum a, Bounded a) => BS.ByteString -> Parser a
enumFromField s
  | readValue < 0 = empty
  | readValue > fromEnum (maxBound :: a) = empty
  | otherwise = return $ toEnum readValue
  where 
    readValue = case readMaybe $ map (toEnum . fromEnum) $ BS.unpack s of
      Just n -> n
      Nothing -> (-1)

-- | Create an enum type suitable for use in a GTFS type
makeEnum :: String -> [String] -> Q [Dec]
makeEnum nameStr constructorsStr = do
  typeName <- newName nameStr
  let constructors = map ((flip normalC [] =<<) . newName) constructorsStr
  dataDec <- 
    dataD (return []) typeName [] constructors $ map mkName ["Enum", "Bounded", "Show"]
  let instanceAppT = appT (conT ''FromField) (conT typeName)
      instanceDecClause = clause [] (normalB (varE 'enumFromField)) []
      instanceDec = funD 'parseField [instanceDecClause]
  instDec <- instanceD (return []) instanceAppT [instanceDec]
  return [dataDec, instDec]
