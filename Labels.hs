{-# LANGUAGE OverloadedStrings #-}
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.IO ( stderr, hPutStrLn )
import qualified Data.Vector as V
--import qualified Data.Text as T
import Data.Text ( Text )
import Control.Applicative ( (<$>), (<*>) )
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromRow ( FromRow, field )


data Label = Label
             { _lSerialNo              :: Text
             , _lName                  :: Text
             , _lCpuType               :: Text
             , _lCurrentProcessorSpeed :: Text
             , _lNumberProcessors      :: Text
             , _lPhysicalMemory        :: Text
             , _lMachineModel          :: Text
             , _lMachineName           :: Text
             }
             deriving ( Show, Eq )

instance FromRow Label where
  fromRow = Label <$>
            field <*>
            field <*>
            field <*>
            field <*>
            field <*>
            field <*>
            field <*>
            field

die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

q :: SQL.Query
q = "SELECT serial_no, name, cpu_type, current_processor_speed,\
    \ number_processors, physical_memory, machine_model, machine_name\
    \ FROM laptop ORDER BY last_update DESC"

printLabels :: V.Vector Label -> IO ()
printLabels = V.mapM_ print

getLabels :: SQL.Connection -> IO (V.Vector Label)
getLabels conn =
  V.fromList <$> SQL.query_ conn q

main :: IO ()
main = do
  db <- getArgs >>= \x -> case x of
    [db] -> return db
    _    -> die "USAGE: laptop-labels DATABASE"
  printLabels =<< SQL.withConnection db getLabels
