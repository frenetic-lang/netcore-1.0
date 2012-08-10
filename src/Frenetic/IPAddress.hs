module Frenetic.IPAddress
  ( IPAddr (..)
  , ipAddr
  , unpackIPAddr
  , IPAddrPrefix
  ) where

import Data.Word
import Data.Bits
import Text.Printf

data IPAddr = IPAddr { ipAddrToWord32 :: Word32 }
  deriving (Eq, Ord)

data IPAddrPrefix = IPAddrPrefix IPAddr Int

instance Show IPAddr where
  show addr = printf "%03d.%03d.%03d.%03d" b3 b2 b1 b0
    where (b3, b2, b1, b0) = unpackIPAddr addr

unpackIPAddr :: IPAddr -> (Word8, Word8, Word8, Word8)
unpackIPAddr (IPAddr w32) = (b3, b2, b1, b0)
  where b0 = fromIntegral $ w32 .&. 0xff 
        b1 = fromIntegral $ (w32 `shiftR` 8) .&. 0xff
        b2 = fromIntegral $ (w32 `shiftR` 16) .&. 0xff
        b3 = fromIntegral $ (w32 `shiftR` 24) .&. 0xff

ipAddr :: Word8 -> Word8 -> Word8 -> Word8 -> IPAddr
ipAddr b3 b2 b1 b0 =
  IPAddr $ (fromIntegral b3 `shiftL` 24) .|. 
           (fromIntegral b2 `shiftL` 16) .|. 
           (fromIntegral b1 `shiftL` 8) .|. 
           (fromIntegral b0)

ipAddrPrefix :: IPAddr -> Int -> IPAddrPrefix
ipAddrPrefix addr len = 
  if len < 0 || len > 32 then
    error $ "ipAddrPrefix: " ++ show len ++ " is an invalid prefix length"
  else
    IPAddrPrefix addr len

instance Show IPAddrPrefix where
  show (IPAddrPrefix addr pf) =
    if pf == 32 then
      show addr
    else if pf >= 24 then
      take 11 (show addr) ++ "/" ++ show pf
    else if pf >= 16 then
      take 7 (show addr) ++ "/" ++ show pf
    else
      take 3 (show addr) ++ "/" ++ show pf