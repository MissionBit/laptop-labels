{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.IO ( stderr, hPutStrLn )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Applicative ( (<$>), (<*>) )
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromRow ( FromRow, field )
import Text.Blaze.Svg.Renderer.Utf8 ( renderSvg )
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Codec.Binary.QRCode as QR
import Network.HTTP.Types.URI ( renderQuery )
import Data.List.Split ( chunksOf )
import Text.Blaze.Svg11 ( (!) )
import qualified Data.Array as Array
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze ( toValue, toMarkup, preEscapedToMarkup )
import Control.Monad ( when, foldM, void )
import Data.Monoid ( mempty, (<>) )
import System.Console.GetOpt
  ( ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt, usageInfo )
import Safe ( readMay )
import Data.Csv ( FromNamedRecord(..), decodeByName, (.:) )
import Data.Foldable (toList)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAscii)

data DbType = SqliteDb | CsvDb

data Flags =
  Flags
  { fNames :: Maybe [String]
  , fDb    :: Maybe (DbType, String)
  , fCsv   :: Maybe String
  , fSkip  :: Maybe Int }

defaultFlags :: Flags
defaultFlags = Flags { fNames = Nothing, fDb = Nothing, fSkip = Nothing, fCsv = Nothing }

setDb :: DbType -> Maybe String -> Flags -> Flags
setDb dbType db f = f { fDb = (,) dbType <$> db }

addName :: Maybe String -> Flags -> Flags
addName n f = f { fNames = fmap (:[]) n <> fNames f }

setSkip :: Maybe String -> Flags -> Flags
setSkip s f = f { fSkip = s >>= readMay }

options :: [OptDescr (Flags -> Flags)]
options =
  [ Option ['d'] ["db"]   (OptArg (setDb SqliteDb) "FILE") "read labels from SQLite3 db"
  , Option ['c'] ["csv"]  (OptArg (setDb CsvDb)    "FILE") "read labels from Meraki CSV"
  , Option ['n'] ["name"] (OptArg addName          "NAME") "print label for only NAME"
  , Option ['s'] ["skip"] (OptArg setSkip          "SKIP") "number of labels to skip"
  ]

raw :: Text -> S.Svg
raw = preEscapedToMarkup

-- Units are 72 Pixel per inch
type Pixel = Double
data Layout =
  Layout
  { lPageSize     :: (Pixel, Pixel) -- width, height
  , lPageMargin   :: (Pixel, Pixel) -- left/right, top/bottom
  , lLabelMargin  :: (Pixel, Pixel)
  , lLabelSize    :: (Pixel, Pixel)
  , lLabelPadding :: (Pixel, Pixel)
  , lRows         :: Int
  , lCols         :: Int
  } deriving ( Show, Eq )

data Label =
  Label
  { lSerialNo              :: !Text
  , lName                  :: !Text
  , lCpuType               :: !Text
  , lCurrentProcessorSpeed :: !Text
  , lPhysicalMemory        :: !Text
  , lMachineModel          :: !Text
  }
  deriving ( Show, Eq )

inch :: Double -> Pixel
inch = (*72)

avery48160 :: Layout
avery48160 = Layout
  { lPageSize     = (inch 8.5, inch 11)
  , lPageMargin   = (inch (3 / 16), inch 0.5)
  , lLabelMargin  = (inch (1 / 8), inch 0)
  , lLabelSize    = (inch (2 + 5 / 8), inch 1)
  , lLabelPadding = (inch 0, inch 0)
  , lRows         = 10
  , lCols         = 3
  }

labelOrigins :: Layout -> [(Pixel, Pixel)]
labelOrigins layout =
  [ (l + (lW + lMW) * col, t + (lH + lMH) * row)
  | col <- map fromIntegral [0..cols-1],
    row <- map fromIntegral [0..rows-1]]
  where
    (l, t) = (max pMW lMW, max pMH lMH)
    (lW, lH) = lLabelSize layout
    (pMW, pMH) = lPageMargin layout
    (lMW, lMH) = lLabelMargin layout
    rows = lRows layout
    cols = lCols layout

labelUrl :: Label -> String
labelUrl label =
  B8.unpack $
  "https://www.missionbit.com/laptop/" `B.append`
  renderQuery True query
 where
   query = [(k, Just . T.encodeUtf8 . fv $ label) | (k, fv) <- fields]
   fields =
     [ ("name", lName)
     , ("serno", lSerialNo)
     , ("model", lMachineModel)
     , ("ram", lPhysicalMemory)
     , ("cpu", lCpuType)
     , ("cpu_speed", lCurrentProcessorSpeed)
     ]

instance FromRow Label where
  fromRow =
    Label <$>
    field <*> -- serial_no
    field <*> -- name
    field <*> -- cpu_type
    field <*> -- concurrent_processor_speed
    field <*> -- physical_memory
    field     -- machine_model

instance FromNamedRecord Label where
  parseNamedRecord m =
    Label <$>
    m .: "Serial"  <*> -- serial_no
    m .: "Name"    <*> -- name
    m .: "CPU"     <*> -- cpu_type
    pure ""        <*> -- concurrent_processor_speed
    m .: "RAM"     <*> -- physical_memory
    m .: "Model"       -- machine_model


die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

q :: SQL.Query
q = "SELECT serial_no, name, cpu_type, current_processor_speed,\
    \ physical_memory, machine_model\
    \ FROM laptop ORDER BY last_update DESC"

getLabels :: SQL.Connection -> IO [Label]
getLabels conn = SQL.query_ conn q

renderLabels :: Layout -> [Maybe Label] -> L8.ByteString
renderLabels layout ls =
  go $ map f ls
  where
    f = maybe mempty (\l -> renderLabel layout l $ labelQRCode l)
    go = renderSvg . combine . layoutPages layout
    combine pages = do
      raw
        "<!doctype html>\n\
        \<html>\n\
        \<head>\n\
        \<link rel=\"stylesheet\" href=\"css/labels.css\">\n\
        \</style>\n\
        \</head>\n\
        \<body>\n"
      sequence_ pages
      raw
        "</body>\n\
        \</html>\n"

layoutPages :: Layout -> [S.Svg] -> [S.Svg]
layoutPages layout = map (layoutPage layout) . chunksOf n
  where
    n = lCols layout * lRows layout

layoutPage :: Layout -> [S.Svg] -> S.Svg
layoutPage layout labels =
  S.svg
    ! A.version "1.1"
    ! A.width (toValue w)
    ! A.height (toValue h)
    ! A.viewbox (toValue . unwords $ map show [0, 0, w, h])
  $ sequence_ (zipWith move labels origins)
  where
    move label (x, y) = label ! A.transform (S.translate x y)
    origins = labelOrigins layout
    (w, h) = lPageSize layout

renderLabel :: Layout -> Label -> (Int, S.Svg) -> S.Svg
renderLabel layout label (qrSize, qr) =
  S.g
  ! A.class_ "label"
  $ do
    qr ! A.transform (S.translate (inch (1/8)) ((h - qw)/2))
    S.text_ (toMarkup (lSerialNo label))
      ! A.x tx
      ! A.y (ty 1)
    S.text_ (toMarkup (lMachineModel label))
      ! A.x tx
      ! A.y (ty 2)
    S.text_ (toMarkup (lPhysicalMemory label))
      ! A.x tx
      ! A.y (ty 3)
    S.text_ (toMarkup (lCpuType label))
      ! A.x tx
      ! A.y (ty 4)
    S.text_ (toMarkup (lName label))
      ! A.class_ "name"
      ! A.x tx
      ! A.y (toValue (h - th))
    S.image
      ! A.xlinkHref "img/mbit-logo.svg"
      ! A.height (inchValue (3/8))
      ! A.width (inchValue (3/8))
      ! A.x (toValue (w - inch (4/8)))
      ! A.y (inchValue (3/16))
  where
    ty n = toValue (inch (2/32) + n * th)
    th = inch (5/32)
    tx = toValue (qw + inch (5/32))
    inchValue = toValue . inch
    qw = fromIntegral qrSize
    (w, h) = lLabelSize layout

data RowState = RowState !Int !Bool

labelQRCode :: Label -> (Int, S.Svg)
labelQRCode label = (w, encodedQR)
  where
    Just ver = QR.version 10
    Just qrMatrix = QR.encode ver QR.M QR.EightBit (labelUrl label)
    qrArray = QR.toArray qrMatrix
    (w, h) = snd $ Array.bounds qrArray
    row y = map (\x -> qrArray Array.! (x, y)) [0..w]
    qrPath = mapM_ qrRow [0..h]
    qrRow y = S.m 0 y >> foldM rowFSM (RowState 0 True) (row y) >>= flushFSM
    rowFSM (RowState n l) l' = case (l, l') of
      -- Skip when transitioning from light to dark
      (True, False) -> when (n > 0) (S.mr n 0) >> pure (RowState 1 l')
      -- Draw when transitioning from dark to light
      (False, True) -> when (n > 0) (S.hr n) >> pure (RowState 1 l')
      _             -> pure (RowState (n + 1) l)
    flushFSM rs = void (rowFSM rs True)
    encodedQR = S.g $
      S.path
      ! A.class_ "qr"
      ! A.d (S.mkPath qrPath)

discardBOM :: BL.ByteString -> BL.ByteString
discardBOM bs
  | BL.take 3 bs == bom = discardBOM (BL.drop 3 bs)
  | otherwise = bs
  where bom = BL.pack [0xEF,0xBB,0xBF]

readLabels :: (DbType, String) -> IO [Label]
readLabels (dbType, db) = case dbType of
  SqliteDb -> SQL.withConnection db getLabels
  CsvDb    -> do
    csvData <- discardBOM <$> BL.readFile db
    case decodeByName csvData of
      Left err -> putStrLn (filter isAscii err) >> pure []
      Right (_, v) -> pure $ toList v

main :: IO ()
main = do
  let header = "Usage: laptop-labels [OPTION...]"
      info   = usageInfo header options
  opts <- getArgs >>= \args -> case getOpt Permute options args of
    (o, [], [])  -> return (foldl (flip id) defaultFlags o)
    (_, _, errs) -> die (concat errs ++ info)
  db <- maybe (die $ "db is a required argument\n" ++ info) return (fDb opts)
  let skips = maybe [] (`replicate` Nothing) (fSkip opts)
      labelFilter names = filter ((`elem` map T.pack names) . lName)
      keep = maybe id labelFilter (fNames opts)
  labels <- (skips++) . map Just . keep <$> readLabels db
  L8.putStrLn $ renderLabels avery48160 labels
