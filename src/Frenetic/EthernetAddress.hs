module Frenetic.EthernetAddress
  ( EthernetAddress (..)
  , ethernetAddress
  , broadcastAddress
  , ethernetAddress64
  , unpackEthernetAddress
  ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Numeric (showHex)
import Data.Word
import Data.Bits
import Data.List (intersperse)

data EthernetAddress = EthernetAddress { unpackEth64 :: Word64 }
  deriving (Eq, Ord)

instance Show EthernetAddress where
  show eth = concat $ 
    intersperse ":" (map (\n -> showHex n "") [w0,w1,w2,w3,w4,w5])
      where (w0,w1,w2,w3,w4,w5) = unpackEthernetAddress eth

instance Enum EthernetAddress where
  toEnum n = EthernetAddress (toEnum n)
  fromEnum (EthernetAddress w64) = fromEnum w64

instance Binary EthernetAddress where
  
  get = do
    w32 <- getWord32be  
    w16 <- getWord16be
    let w64 = (fromIntegral w32 `shiftL` 16) .|. fromIntegral w16
    return (EthernetAddress w64)

  put (EthernetAddress w64) = do
    putWord32be (fromIntegral (shiftR w64 16))
    putWord16be (fromIntegral (w64 `mod` 0x010000))

ethernetAddress :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
                -> EthernetAddress                                
ethernetAddress w1 w2 w3 w4 w5 w6  = EthernetAddress w64
  where w64 = (shiftL (fromIntegral w1) 40) .|.
              (shiftL (fromIntegral w2) 32) .|.
              (shiftL (fromIntegral w3) 24) .|.                       
              (shiftL (fromIntegral w4) 16) .|.      
              (shiftL (fromIntegral w5) 8) .|.
              (fromIntegral w6)

broadcastAddress :: EthernetAddress
broadcastAddress = EthernetAddress 0xffffffffffff

unpackEthernetAddress :: EthernetAddress
                      -> (Word8,Word8,Word8,Word8,Word8,Word8)
unpackEthernetAddress (EthernetAddress w64) =
  let a1 = fromIntegral (shiftR w64 40)
      a2 = fromIntegral (shiftR w64 32 `mod` 0x0100)
      a3 = fromIntegral (shiftR w64 24 `mod` 0x0100)
      a4 = fromIntegral (shiftR w64 16 `mod` 0x0100)
      a5 = fromIntegral (shiftR w64 8 `mod` 0x0100)
      a6 = fromIntegral (w64 `mod` 0x0100)
  in (a1,a2,a3,a4,a5,a6)

ethernetAddress64 :: Word64 -> EthernetAddress
ethernetAddress64 w64 = EthernetAddress (w64 `mod` 0x01000000000000)