import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Char (isSpace)
import Data.Word (Word8, Word16, Word32, Word64)
import Parse
import Numeric (showHex)
import Data.Either (either)

-- Jpeg
data Jpeg = Jpeg {
    jpegWidth :: Int
  , jpegHeight :: Int
  , jpegDQTable :: DQTable
  , jpegHuffmanTable :: HuffmanTable
  , jpegBytes :: B.ByteString
  } deriving (Eq)

instance Show Jpeg where
  show (Jpeg w h _ _) = "Jpeg " ++ show w ++ "x" ++ show h

parseJpeg :: Parser Jpeg
parseJpeg bytes = do
  s <- expectMarker soiMarker bytes
  (s', seg0) <- peelApp0Segment s
  (s', segs) <- peelAny peelAppnSegment s'
  (s', dqt) <- peelDqt s'
  Left (s', "not done implementing parseJpeg: " ++ show seg0)

debug :: L.ByteString -> Either String Jpeg
debug s = case parseJpeg s of
  Left (s', err) -> Left $
                    lengthMessage s' s ++ "\n"
                    ++ show (L.take 15 s') ++ "...\n"
                    ++ err
  Right j -> Right j
  where lengthMessage s' s =
          let read = fromIntegral $ L.length s - L.length s'
          in "read " ++ show read ++ " (0x" ++ showHex read "" ++ ") bytes. " ++ show (L.length s') ++ " to go"

doDebug :: L.ByteString -> IO ()
doDebug s = putStrLn $ either id show $ debug s

-- AppnSegments
data AppnSegment = AppnSegment {
    appnN :: Int
  , appnBytes :: B.ByteString
  }

peelAppnSegment :: Peeler AppnSegment
peelAppnSegment s = do
  (s', m) <- peelMarker s
  let mb = fromIntegral $ markerByte m
  _ <- assertThat s $ 0xe0 <= mb && mb <= 0xef
  (s', length) <- peelWord16 s'
  (s', bytes) <- peelNBytes (fromIntegral length - 2) s'
  Right $ (,) s' $ AppnSegment (mb - 0xe0) (L.toStrict bytes)


data App0Segment = App0Segment {
    app0Major :: Word8
  , app0Minor :: Word8
  , app0DensityUnits :: DensityUnits
  , app0XDensity :: Int
  , app0YDensity :: Int
  , app0ThumbWidth :: Int
  , app0ThumbHeight :: Int
  , app0ThumbBytes :: B.ByteString
  } deriving (Eq)
data DensityUnits = NoUnits | DotsPerInch | DotsPerCm
  deriving (Eq, Show)
densityUnitsFromInt 0 = Just NoUnits
densityUnitsFromInt 1 = Just DotsPerInch
densityUnitsFromInt 2 = Just DotsPerCm
densityUnitsFromInt _ = Nothing

instance Show App0Segment where
  show seg = "App0Segment"
          ++ " v" ++ show (app0Major seg) ++ "." ++ show (app0Minor seg)
          ++ " " ++ show (app0DensityUnits seg)
          ++ " Density:" ++ show (app0XDensity seg) ++ ":" ++ show (app0YDensity seg)
          ++ " Thumb:" ++ show (app0ThumbWidth seg) ++ "x" ++ show (app0ThumbHeight seg)

peelApp0Segment :: Peeler App0Segment
peelApp0Segment s = do
  (s', seg) <- peelAppnSegment s
  _ <- assertThat s $ appnN seg == 0
  let bs = L.fromStrict $ appnBytes seg
  bs' <- expectBytes (L8.pack "JFIF\0") bs
  bs' <- expectBytes (L.pack [1]) bs'
  (bs', minorRevision) <- peelWord8 bs'
  (bs'', du) <- peelWord8 bs'
  densityUnits <- maybe (Left (bs', "expected 0-2 for density units")) Right $ densityUnitsFromInt du
  (bs', xDensity) <- peelWord16 bs''
  (bs', yDensity) <- peelWord16 bs'
  (bs', thumbWidth) <- peelWord8 bs'
  (bs', thumbHeight) <- peelWord8 bs'
  let thumbLength = 3 * (fromIntegral thumbWidth) * (fromIntegral thumbHeight)
  (bs', thumbBytes) <- peelNBytes thumbLength bs'
  _ <- assertThat bs' $ L.length bs' == 0
  return $ (,) s' $
    App0Segment 1 minorRevision densityUnits (fromIntegral xDensity) (fromIntegral yDensity) (fromIntegral thumbWidth) (fromIntegral thumbHeight) (L.toStrict thumbBytes)


-- Discrete Quantization Table
data DQTable = DQTable {
    dqtLength :: Int
  , dqtBytes :: B.ByteString
  }

peelDqt :: Peeler DQTable
peelDqt s = do
  s' <- expectMarker dqtMarker s
  (s', length) <- peelWord16 s'
  (s', bytes) <- peelNBytes (fromIntegral length - 2) s'
  return $ (,) s' $
    DQTable (fromIntegral length) (L.toStrict bytes)

-- Marker
data Marker = Marker { markerByte :: Word8 }
  deriving (Eq, Show, Read)

isMarker :: L.ByteString -> Bool
isMarker s = L.length s == 2 && L.head s == 0xff

soiMarker    = Marker 0xd8
appnMarker n = Marker $ 0xe0 + n
app0Marker   = appnMarker 0
dqtMarker    = Marker 0xdb
sofnMarker n = Marker $ 0xc0 + n
dhtMarker    = Marker 0xc4
sosMarker    = Marker 0xda
eoiMarker    = Marker 0xd9

expectMarker :: Expecter Marker
expectMarker = expecter peelMarker

peelMarker :: Peeler Marker
peelMarker = peeler "expected marker" maybeGetMarker
  where
    maybeGetMarker s =
      let (mbs, s') = L.splitAt 2 s in
        if not $ isMarker mbs
        then Nothing
        else Just $ (s', Marker $ L.index mbs 1)

stripToMarker :: L.ByteString
              -> L.ByteString
              -> L.ByteString
stripToMarker m xs | xs == L.empty = L.empty
stripToMarker m xs =
  if L.take 2 ffOn == m
  then ffOn
  else stripToMarker m $ L.drop 1 ffOn
  where
    ffOn = stripToFf xs
    stripToFf = L.dropWhile $ \w -> w /= 0xff


-- Huffman Coding --

data HuffmanTable = HuffmanTable {} deriving (Eq)
