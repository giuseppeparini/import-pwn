{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Control.Exception as E
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.Word (Word32, Word64)
import Network.Socket
import System.ByteOrder
import System.IO
import System.IO.Error (isEOFError)

data Value = U32 Word32 | U64 Word64
  deriving (Show)

instance Bytes Value where
  toBigEndian (U32 v) = U32 (toBigEndian v)
  toBigEndian (U64 v) = U64 (toBigEndian v)

  toLittleEndian (U32 v) = U32 (toLittleEndian v)
  toLittleEndian (U64 v) = U64 (toLittleEndian v)

data Request = Number Value ByteOrder | NewLine
  deriving (Show)

parse :: String -> Request
parse (stripPrefix "Please send me the number " -> Just rest) =
  (\(n, r) -> Number n (parseEndian r)) $
    parseBits $
      splitNum rest
  where
    splitNum = break isSpace
    parseBits (n, stripPrefix " as a 32-bit " -> Just rest) = (U32 (read n), rest)
    parseBits (n, stripPrefix " as a 64-bit " -> Just rest) = (U64 (read n), rest)
    parseBits _ = error "unreachable"
    parseEndian (stripPrefix "big-endian " -> Just _) = BigEndian
    parseEndian (stripPrefix "little-endian " -> Just _) = LittleEndian
    parseEndian _ = error "unreachable"
parse (stripPrefix "Please send me an empty line " -> Just _) = NewLine
parse _ = error "unreachable"

handle :: Request -> B.ByteString
handle (Number v end) = encodeValue $ trans v end
  where
    trans v BigEndian = toBigEndian v
    trans v LittleEndian = toLittleEndian v
    -- toBigEndian swaps bytes if the target machine is little endian because
    -- encode swaps them again
    encodeValue (U32 v) = encode $ toBigEndian v
    encodeValue (U64 v) = encode $ toBigEndian v
handle NewLine = B.pack [10]

tryhGetLine :: Handle -> IO (Maybe String)
tryhGetLine h = do
  eitherLine <- E.try $ hGetLine h
  case eitherLine of
    Left e ->
      if isEOFError e
        then return Nothing
        else E.throwIO e
    Right l -> return (Just l)

loop :: Handle -> IO ()
loop h = do
  maybeLine <- tryhGetLine h
  case maybeLine of
    Just line -> do
      putStrLn line
      B.hPut h $ handle $ parse line
      hGetLine h >>= putStrLn
      loop h
    Nothing -> return ()

main :: IO ()
main = runTCPClient "piecewise.challs.cyberchallenge.it" "9110" $ \s -> do
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  loop h

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $addrAddress addr
      return sock
    openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
