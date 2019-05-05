{-
   Honor Pledge

   I pledge on my honor that I have not given or received any
   unauthorized assistance on this assignment.

   [Joshua Fleming]
-}
module FPFF where
import Numeric (showHex)
import Control.Monad
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.List (intercalate)
import Data.Binary.Get
import Data.Word

-- FPFF top-level structure
data FPFF = FPFF {
    header :: FPFFHeader
  , body   :: FPFFBody
  }

-- FPFF header structure
data FPFFHeader = FPFFHeader {
    magic         :: Word32
  , version       :: Word32
  , timestamp     :: Word32
  , author        :: B.ByteString
  , sectionCount  :: Word32
  }

-- FPFF body structure
data FPFFBody = FPFFBody {
    sections      :: [FPFFSection]
  }

-- FPFF section structure
data FPFFSection = FPFFSection {
    stype         :: Word32
  , slen          :: Word32
  , svalue        :: FPFFSectionValue
}

-- FPFF section values
data FPFFSectionValue =
    ASCII String
  | UTF8 String
  | WORDS [Word32]
  | DWORDS [Word64]
  | DOUBLES [Double]
  | COORD (Double, Double)
  | REFERENCE Word32
  | PNG B.ByteString
  | GIF87 B.ByteString
  | GIF89 B.ByteString
  deriving (Show)

-- FPFF section types
data FPFFSectionType =
    SECTION_UNUSED
  | SECTION_ASCII
  | SECTION_UTF8
  | SECTION_WORDS
  | SECTION_DWORDS
  | SECTION_DOUBLES
  | SECTION_COORD
  | SECTION_REFERENCE
  | SECTION_PNG
  | SECTION_GIF87
  | SECTION_GIF89
  deriving (Enum, Show)

-- pretty printing FPFF data
instance Show FPFF where
  show (FPFF h b) =
    "=====BEGIN FPFF HEADER DATA=====\n" ++
    show h                             ++
    "===== END FPFF HEADER DATA =====\n" ++
    "\n" ++
    "===== BEGIN FPFF BODY DATA =====\n" ++
    show b                             ++
    "=====  END FPFF BODY DATA  =====\n"

-- pretty printing header data
instance Show FPFFHeader where
  show (FPFFHeader m v t a s) =
    "Magic: " ++ showHex m "\n" ++
    "Version: " ++ showHex v "\n" ++
    "Timestamp: " ++ show t ++ "\n" ++
    "Author: " ++ show (filter (/= '\NUL') (C.unpack a)) ++ "\n" ++
    "Section Count: " ++ showHex s "\n"

-- pretty printing FPFF body
instance Show FPFFBody where
  show (FPFFBody s) = intercalate "" (map f s)
    where f x = "=====BEGIN FPFF SECTION DATA=====\n" ++
                show x                                ++
                "===== END FPFF SECTION DATA =====\n\n"

-- pretty printing FPFF sections
instance Show FPFFSection where
  show (FPFFSection st sl sv) =
    let se = (toEnum $ fromIntegral st :: FPFFSectionType) in
    let dat = case sv of
          PNG p     -> "redacted PNG, SHA256 = " ++ show (hashToString p)
          GIF87 g87 -> "redacted GIF87, SHA256 = " ++ show (hashToString g87)
          GIF89 g89 -> "redacted GIF89, SHA256 = " ++ show (hashToString g89)
          otherwise -> show sv
    in
    "Section type: " ++ show se ++ "\n" ++
    "Section length: " ++ show sl ++ "\n" ++
    "Section data: " ++ dat ++ "\n"

-- parses out an FPFF file by calling the header and body parsers
parseFPFF :: Get FPFF
parseFPFF = do
  header <- parseHeader
  body <- parseBody
  return $! FPFF header body

-- parses out header information from an FPFF file
parseHeader :: Get FPFFHeader
parseHeader = do
  magic <- getWord32le
  version <- getWord32le
  timestamp <- getWord32le
  author <- getByteString 8
  sectionCount <- getWord32le
  return $! FPFFHeader magic version timestamp author sectionCount

-- parses out the body of an FPFF file
parseBody :: Get FPFFBody
parseBody = do
  sections <- parseSections
  return $! FPFFBody sections

-- parses FPFF sections based on their section types
parseSection :: Get FPFFSection
parseSection = do
  stype <- getWord32le
  slen <- getWord32le
  svalue <-
    let n = fromIntegral slen in
    case toEnum $ fromIntegral stype of
      SECTION_ASCII     -> parseASCII n
      SECTION_UTF8      -> parseUTF8 n
      SECTION_WORDS     -> parseWORDS n
      SECTION_DWORDS    -> parseDWORDS n
      SECTION_DOUBLES   -> parseDOUBLES n
      SECTION_COORD     -> parseCOORD
      SECTION_REFERENCE -> parseREFERENCE
      SECTION_PNG       -> parsePNG n
      SECTION_GIF87     -> parseGIF87 n
      SECTION_GIF89     -> parseGIF89 n
  return $! FPFFSection stype slen svalue

-- recursively parses out all sections in an FPFF file
parseSections :: Get [FPFFSection]
parseSections = do
  empty <- isEmpty
  if empty
    then return $! []
    else do section <- parseSection
            sections <- parseSections
            return (section:sections)

-- parses out ASCII-encoded text from a SECTION_ASCII
parseASCII :: Int -> Get FPFFSectionValue
parseASCII n = do
  ascii <- getByteString n
  return $! ASCII (C.unpack ascii)

-- parses out UTF8-encoded text from a SECTION_UTF8
parseUTF8 :: Int -> Get FPFFSectionValue
parseUTF8 n = do
  utf8 <- getByteString n
  return $! UTF8 (U.toString utf8)

-- parses out a list of words from a SECTION_WORDS
parseWORDS :: Int -> Get FPFFSectionValue
parseWORDS n = do
  words <- replicateM (n `div` 4) getWord32le
  return $! WORDS (words)

-- parses out a list of dwords from a SECTION_DWORDS
parseDWORDS :: Int -> Get FPFFSectionValue
parseDWORDS n = do
  dwords <- replicateM (n `div` 8) getWord64le
  return $! DWORDS (dwords)

-- parses out a list of doubles from a SECTION_DOUBLES
parseDOUBLES :: Int -> Get FPFFSectionValue
parseDOUBLES n = do
  doubles <- replicateM (n `div` 8) getDoublele
  return $! DOUBLES (doubles)

-- parses out coordinates from a SECTION_COORD
parseCOORD :: Get FPFFSectionValue
parseCOORD = do
  lat <- getDoublele
  long <- getDoublele
  return $! COORD (lat, long)

-- parses out a section reference from a SECTION_REFERENCE
parseREFERENCE :: Get FPFFSectionValue
parseREFERENCE = do
  ref <- getWord32le
  return $! REFERENCE ref

-- parses out a PNG from a SECTION_PNG
parsePNG :: Int -> Get FPFFSectionValue
parsePNG n = do
  body <- getByteString n
  let pngSig = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a] :: [Word8]
  return $! PNG $ B.append (B.pack pngSig) body

-- parses out a GIF87 from a SECTION_GIF87
parseGIF87 :: Int -> Get FPFFSectionValue
parseGIF87 n = do
  body <- getByteString n
  let gif87Sig = [0x47, 0x49, 0x46, 0x38, 0x37, 0x61] :: [Word8]
  return $! GIF87 $ B.append (B.pack gif87Sig) body

-- parses out a GIF89 file from a SECTION_GIF89
parseGIF89 :: Int -> Get FPFFSectionValue
parseGIF89 n = do
  body <- getByteString n
  let gif89Sig = [0x47, 0x49, 0x46, 0x38, 0x39, 0x61] :: [Word8]
  return $! GIF89 $ B.append (B.pack gif89Sig) body

-- main driver for parser
-- takes filename and parses out an FPFF file
open :: FilePath -> IO FPFF
open n = do
  input <- BL.readFile n
  return $ runGet parseFPFF input

-- helper function to convert image ByteStrings to a hash string
hashToString :: B.ByteString -> B.ByteString
hashToString s = BL.toStrict $ BB.toLazyByteString (BB.byteStringHex (hash s))

-- helper function to grab image data from sections
getImages :: FPFF -> [B.ByteString]
getImages fpff =
  let s = sections $ body $ fpff
      f sec = case svalue sec of
                PNG p     -> p
                GIF87 g87 -> g87
                GIF89 g89 -> g89
                otherwise -> B.empty
  in
  filter (/= B.empty) (map f s)

main :: IO ()
main = do
  putStrLn "Enter a filename:"
  file <- getLine
  fpff <- open file
  print fpff
  let imgs = getImages fpff
  let f x = B.writeFile (C.unpack $ hashToString x) x
  mapM_ f imgs
  print "All images were written out with their SHA256 sum as their filenames."
