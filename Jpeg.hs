module Jpeg where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Word (Word8, Word16, Word32, Word64)
import Parse
import Numeric (showHex)
import Data.Serializer (word16B)

-- tools
fromLeft (Left x) = x
fromRight (Right x) = x
toHex = flip showHex ""


class Bytten a where
  toBytesLazy :: a -> L.ByteString
  toBytes :: a -> B.ByteString
  toBytes = L.toStrict . toBytesLazy


-- Jpeg
data Jpeg = Jpeg {
    jpegApp0 :: App0Segment
  , jpegAppns :: [AppnSegment]
  , jpegDQTable :: [DQTable]
  , jpegSof :: SOFSegment
  , jpegDHTable :: [DHTable]
  , jpegSos :: SOSSegment
  , jpegBytes :: B.ByteString
  } deriving (Eq)

instance Show Jpeg where
  show (Jpeg _ _ _ _ _ _ _) = "Jpeg"

instance Bytten Jpeg where
  toBytesLazy j = L.concat [ toBytesLazy soiMarker
                           , toBytesLazy $ jpegApp0 j
                           , L.concat $ map toBytesLazy $ jpegAppns j
                           , L.concat $ map toBytesLazy $ jpegDQTable j
                           , toBytesLazy $ jpegSof j
                           , L.concat $ map toBytesLazy $ jpegDHTable j
                           , toBytesLazy $ jpegSos j
                           , L.concatMap expandFf $ L.fromStrict $ jpegBytes j
                           , toBytesLazy eoiMarker
                           ]
    where expandFf x = L.pack $ if x == 0xff then [0xff, 0] else [x]

parseJpeg :: Parser Jpeg
parseJpeg = parser peelJpeg

peelJpeg :: Peeler Jpeg
peelJpeg bytes = do
  s <- expectMarker soiMarker bytes
  (s', seg0) <- peelApp0Segment s
  (s', segs) <- peelAny peelAppnSegment s'
  (s', dqts) <- peelSome peelDqt s'
  (s', sof) <- peelSof s'
  (s', dht) <- peelSome peelDht s'
  (s', sos) <- peelSos s'
  (s', imageBytes) <- msnd L.toStrict <$> peelImageData s'
  s' <- expectMarker eoiMarker s'
  Right $ (,) s' $
    Jpeg seg0 segs dqts sof dht sos imageBytes

debug :: L.ByteString -> Either String Jpeg
debug s = case parseJpeg s of
  Left (s', err) -> Left $
                    lengthMessage s' s ++ "\n"
                    ++ "left: "
                    ++ (if L.length left < L.length s'
                         then show left ++ "..." else show left)
                    ++ "\n"
                    ++ err
                    where left = L.take 15 s'
  Right j -> Right j
  where lengthMessage s' s =
          let read = fromIntegral $ L.length s - L.length s'
          in "read: " ++ show read ++ " (0x" ++ showHex read "" ++ ") bytes. " ++ show (L.length s') ++ " to go"

doDebug :: String -> IO ()
doDebug fn = putStrLn . either id show . debug =<< L.readFile fn


-- AppnSegments
data AppnSegment = AppnSegment {
    appnN :: Int
  , appnBytes :: B.ByteString
  } deriving (Eq)

instance Bytten AppnSegment where
  toBytesLazy seg = L.concat [ toBytesLazy $ appnMarker $ appnN seg
                             , L.pack $ word16B $ fromIntegral $ 2 + B.length (appnBytes seg)
                             , L.fromStrict $ appnBytes seg
                             ]

peelAppnSegment :: Peeler AppnSegment
peelAppnSegment s = do
  (s', m) <- peelMarker s
  let mb = fromIntegral $ markerByte m
  _ <- assertThat s $ 0xe0 <= mb && mb <= 0xef
  (s', bs) <- peelSegment m s
  return $ (,) s' $
    AppnSegment (mb - 0xe0) (L.toStrict bs)


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
toNum NoUnits     = 0
toNum DotsPerInch = 1
toNum DotsPerCm   = 2

instance Show App0Segment where
  show seg = "App0Segment"
          ++ " v" ++ show (app0Major seg) ++ "." ++ show (app0Minor seg)
          ++ " " ++ show (app0DensityUnits seg)
          ++ " Density:" ++ show (app0XDensity seg) ++ ":" ++ show (app0YDensity seg)
          ++ " Thumb:" ++ show (app0ThumbWidth seg) ++ "x" ++ show (app0ThumbHeight seg)

instance Bytten App0Segment where
  toBytesLazy seg = L.concat [ toBytesLazy app0Marker
                             , L.pack $ word16B $ fromIntegral $ 16 + B.length (app0ThumbBytes seg)
                             , L8.pack "JFIF\0"
                             , L.singleton $ app0Major seg
                             , L.singleton $ app0Minor seg
                             , L.singleton $ toNum $ app0DensityUnits seg
                             , L.pack $ word16B $ fromIntegral $ app0XDensity seg
                             , L.pack $ word16B $ fromIntegral $ app0YDensity seg
                             , L.singleton $ fromIntegral $ app0ThumbWidth seg
                             , L.singleton $ fromIntegral $ app0ThumbHeight seg
                             , L.fromStrict $ app0ThumbBytes seg
                             ]

peelApp0Segment :: Peeler App0Segment
peelApp0Segment s = do
  (s', seg) <- peelAppnSegment s
  _ <- assertThat s $ appnN seg == 0
  let bs = L.fromStrict $ appnBytes seg
  bs' <- expectBytes (L8.pack "JFIF\0") bs
  bs' <- expectBytes (L.pack [1]) bs'
  (bs', minorRevision) <- peelWord8 bs'
  (bs'', du) <- peelWord8 bs'
  densityUnits <-
    maybe (Left (bs', "expected 0-2 for density units"))
          Right
          (densityUnitsFromInt du)
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
    dqtBytes :: B.ByteString
  } deriving (Eq)

instance Bytten DQTable where
  toBytesLazy x = L.concat [ toBytesLazy dqtMarker
                           , L.pack $ word16B $ fromIntegral $ 2 + B.length (dqtBytes x)
                           , L.fromStrict $ dqtBytes x
                           ]

peelDqt :: Peeler DQTable
peelDqt s = do
  (s', bs) <- peelSegment dqtMarker s
  return $ (,) s' $
    DQTable $ L.toStrict bs


-- Start of Frame Segment
data SOFSegment = SOFSegment {
    sofBytes :: B.ByteString
  } deriving (Eq)

instance Bytten SOFSegment where
  toBytesLazy x = L.concat [ toBytesLazy sofMarker
                           , L.pack $ word16B $ fromIntegral $ 2 + B.length (sofBytes x)
                           , L.fromStrict $ sofBytes x
                           ]

peelSof :: Peeler SOFSegment
peelSof s = do
  (s', bs) <- peelSegment sofMarker s
  return $ (,) s' $
    SOFSegment $ L.toStrict bs


-- Huffman Table
data DHTable = DHTable {
    dhtBytes :: B.ByteString
  } deriving (Eq)

instance Bytten DHTable where
  toBytesLazy x = L.concat [ toBytesLazy dhtMarker
                           , L.pack $ word16B $ fromIntegral $ 2 + B.length (dhtBytes x)
                           , L.fromStrict $ dhtBytes x
                           ]

peelDht :: Peeler DHTable
peelDht s = do
  (s', bs) <- peelSegment dhtMarker s
  return $ (,) s' $
    DHTable $ L.toStrict bs


-- Start of Scan (actual image)
data SOSSegment = SOSSegment {
    sosBytes :: B.ByteString
  } deriving (Eq)

instance Bytten SOSSegment where
  toBytesLazy x = L.concat [ toBytesLazy sosMarker
                           , L.pack $ word16B $ fromIntegral $ 2 + B.length (sosBytes x)
                           , L.fromStrict $ sosBytes x
                           ]

peelSos :: Peeler SOSSegment
peelSos s = do
  (s', bs) <- peelSegment sosMarker s
  return $ (,) s' $
    SOSSegment $ L.toStrict bs


-- Image data
peelImageData :: Peeler L.ByteString
peelImageData s = Right $ msnd (L.concat . reverse) $ helper [] s
  where helper l s =
          let (hd, tl) = L.break (==0xff) s
          in if L.index tl 1 == 0x00
             then helper ((L.pack [0xff]):hd:l) $ L.drop 2 tl
             else (tl, hd:l)


-- Marker
data Marker = Marker { markerByte :: Word8 }
  deriving (Eq, Read)
instance Show Marker where
  show (Marker b) = "Marker 0x" ++ showHex b ""
instance Bytten Marker where
  toBytesLazy m = L.pack [0xff, markerByte m]

isMarker :: L.ByteString -> Bool
isMarker s = L.length s == 2 && L.head s == 0xff

soiMarker    = Marker 0xd8
appnMarker n = Marker $ 0xe0 + fromIntegral n
app0Marker   = appnMarker 0
dqtMarker    = Marker 0xdb
sofMarker    = Marker $ 0xc0
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

peelSegment :: Marker -> Peeler L.ByteString
peelSegment m s = do
  s' <- expectMarker m s
  (s', l) <- peelWord16 s'
  (s', bs) <- peelNBytes (fromIntegral l - 2) s'
  return (s', bs)
