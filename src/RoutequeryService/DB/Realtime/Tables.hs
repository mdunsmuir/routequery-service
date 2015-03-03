module RoutequeryService.DB.Realtime.Tables where

import Control.Monad
import Database.HDBC

feedEntities = "create table feed_entities (\
  \id MEDIUMINT NOT NULL AUTO_INCREMENT PRIMARY KEY,\
  \is_deleted BOOLEAN,\
  \trip_update_id MEDIUMINT,\
  \vehicle_position_id MEDIUMINT,\
  \alert_id MEDIUMINT )"

tables = [("feed_entities", feedEntities)]

createTables :: IConnection c => c -> IO ()
createTables conn = do
  existingTables <- getTables conn
  forM_ tables $ \(tableName, tableCode) ->
    unless (elem tableName existingTables) $ do
      putStrLn $ "creating table " ++ tableName
      runRaw conn tableCode
