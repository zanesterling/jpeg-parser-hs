module Parse ( Parser
             , Peeler
             , Expecter
             , parser
             , peeler
             , expecter
             , peelAny
             , peelSome
             , peelNBytes
             , expectBytes
             , peelWord8
             , peelWord16
             , peelWord32
             , peelWord64
             , assertThat
             ) where

import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Tuple (swap)
import Data.Word (Word8, Word16, Word32, Word64)

-- Generic parsing types --
type Parser a = L.ByteString
             -> Either (L.ByteString, String) a

type Peeler a = L.ByteString
             -> Either (L.ByteString, String) (L.ByteString, a)
type Expecter a = a
               -> L.ByteString
               -> Either (L.ByteString, String) L.ByteString

peeler :: String
       -> (L.ByteString -> Maybe (L.ByteString, a))
       -> Peeler a
peeler err f s = maybe (Left (s, err)) Right $ f s

expecter :: Eq a => Show a
         => Peeler a -> Expecter a
expecter p a s = do
  (s', a') <- p s
  if a' == a then Right s' else Left (s, "expected " ++ show a ++ "found " ++ show a)

parser :: Peeler a -> Parser a
parser p s = do
  (s', a) <- p s
  if L.length s' /= 0
    then Left (s', "expected no more bytes")
    else Right a

peelSome :: Peeler a -> Peeler [a]
peelSome p s = peelAny p s >>=
  \(s', as) -> if length as == 0
               then Left (s, "expected more than one!")
               else Right (s', as)

peelAny :: Peeler a -> Peeler [a]
peelAny p s =
  case p s of
    Left (_, _) -> Right (s, [])
    Right (s', a) -> (\(s'', as) -> (s'', a:as)) <$> (peelAny p s')

assertThat :: L.ByteString
           -> Bool
           -> Either (L.ByteString, String) ()
assertThat _ True  = Right ()
assertThat s False = Left (s, "assert was false")


-- Handy peelers and parsers --
peelNBytes :: Int -> Peeler L.ByteString
peelNBytes n = peeler ("expected " ++ show n ++ " bytes") $
  \s -> if L.length s >= n'
        then Just $ swap $ L.splitAt n' s
        else Nothing
  where n' = fromIntegral n

expectBytes :: Expecter L.ByteString
expectBytes bs = expecter (peelNBytes $ fromIntegral $ L.length bs) bs

-- mfst :: (a -> a') -> (a, b) -> (a', b)
-- mfst f (a, b) = (f a, b)
msnd :: (b -> b') -> (a, b) -> (a, b')
msnd f (a, b) = (a, f b)

peelWord8 :: Peeler Word8
peelWord8 s = msnd L.head <$> peelNBytes 1 s

peelWord16 :: Peeler Word16
peelWord16 s = msnd toWord16 <$> peelNBytes 2 s
  where
    toWord16 bs = fromIntegral (L.index bs 1)
                  + shiftL (fromIntegral (L.head bs)) 8

peelWord32 :: Peeler Word32
peelWord32 s = msnd toWord32 <$> peelNBytes 4 s
  where
    toWord32 bs = fromIntegral (L.index bs 3)
                  + shiftL (fromIntegral (L.index bs 2)) 8
                  + shiftL (fromIntegral (L.index bs 1)) 16
                  + shiftL (fromIntegral (L.index bs 0)) 24

peelWord64 :: Peeler Word64
peelWord64 s = msnd toWord64 <$> peelNBytes 4 s
  where
    toWord64 bs = fromIntegral (L.index bs 7)
                  + shiftL (fromIntegral (L.index bs 6)) 8
                  + shiftL (fromIntegral (L.index bs 5)) 16
                  + shiftL (fromIntegral (L.index bs 4)) 24
                  + shiftL (fromIntegral (L.index bs 3)) 32
                  + shiftL (fromIntegral (L.index bs 2)) 40
                  + shiftL (fromIntegral (L.index bs 1)) 48
                  + shiftL (fromIntegral (L.index bs 0)) 56
