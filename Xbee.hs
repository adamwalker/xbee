{-# LANGUAGE RecordWildCards #-}
module Xbee where

import           Control.Monad
import           Data.Maybe
import           Data.Word
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Binary
import           Data.Binary.Get
import           Pipes
import qualified Pipes.Prelude as P
import Data.Attoparsec.ByteString as A

type Address = Word16
type Options = Word8
type FrameID = Word8

putEscaped :: Word8 -> Put
putEscaped dat
    | dat `elem` [0x7e, 0x7d, 0x11, 0x13] = putWord8 0x7d >> putWord8 (0x20 `xor` dat)
    | otherwise = putWord8 dat

csum :: [Word8] -> Word8
csum dat = 255 - sum dat

lsb :: Word16 -> Word8
lsb dat = fromIntegral $ dat .&. 0x00ff

msb :: Word16 -> Word8
msb dat = fromIntegral $ (dat .&. 0xff00) `shift` (-8)

send :: Maybe Address -> Maybe Options -> Maybe FrameID -> ByteString -> Put
send address options frameID dat = do
    let frameID'    = fromMaybe 0x00 frameID
        addressLSB' = lsb $ fromMaybe 0x00 address
        addressMSB' = msb $ fromMaybe 0x00 address
        options'    = fromMaybe 0x00 options
    putWord8   0x7e
    putEscaped 0x00
    putEscaped $ fromIntegral $ 5 + BS.length dat
    putEscaped 0x01
    putEscaped frameID'
    putEscaped addressMSB'
    putEscaped addressLSB'
    putEscaped options'
    mapM_ putEscaped $ BS.unpack dat
    putEscaped (csum (0x01 : frameID' : addressMSB' : addressLSB' : options' : BS.unpack dat))

checkCSum :: [Word8] -> Bool
checkCSum dat = sum dat == 255

data RecvPacket = RecvPacket {
    sourceAddr  :: Word16,
    rssi        :: Word8,
    recvOptions :: Word8,
    recvData    :: ByteString
} deriving (Show)

getWord8Escaped :: Parser Word8
getWord8Escaped = do
    res <- anyWord8
    case res of
        0x7d -> do
            res <- anyWord8
            return $ 0x20 `xor` res
        _    -> return res

receive :: Parser RecvPacket
receive = do
    word8 0x7e --start byte
    word8 0x00 --msb
    lsb         <- getWord8Escaped
    word8 0x81 --cmd
    addressMSB  <- getWord8Escaped
    addressLSB  <- getWord8Escaped
    let sourceAddr = (fromIntegral addressMSB `shift` 8) .|. fromIntegral addressLSB
    rssi        <- getWord8Escaped
    recvOptions <- getWord8Escaped
    recvData    <- liftM BS.pack $ replicateM (fromIntegral lsb - 5) getWord8Escaped
    cs          <- getWord8Escaped 
    unless (checkCSum $ 0x81 : addressMSB : addressLSB : rssi : recvOptions : cs : BS.unpack recvData) (fail "checksum failed")
    return RecvPacket {..}

data TXStatus = TXStatus {
    txFrameID :: Word8,
    txStatus  :: Word8
} deriving (Show)

txStat :: Parser TXStatus
txStat = do
    word8 0x7e 
    word8 0x00
    lsb       <- getWord8Escaped
    word8 0x89
    txFrameID <- getWord8Escaped
    txStatus  <- getWord8Escaped
    cs        <- getWord8Escaped 
    unless (checkCSum $ [0x89, txFrameID, txStatus, cs]) (fail "checksum failed")
    return TXStatus {..}

packetsPipe :: MonadIO m => Parser a -> Pipe ByteString a m r
packetsPipe parser = do
    res <- await
    packetsPipe' (parse parser) res
    where
    packetsPipe' p txt = 
        case p txt of
            A.Fail    input x y -> do
                liftIO $ putStrLn ("Parsing failed: " ++ show x ++ " : " ++ show y)
                case BS.uncons input of
                    Nothing        -> do
                        res <- await
                        packetsPipe' (parse parser) res
                    Just (_, rest) -> packetsPipe' (parse parser) rest
            A.Partial cont      -> do
                res <- await
                packetsPipe' cont res
            A.Done    input res -> do
                yield res
                res <- await
                packetsPipe' (parse parser) res

