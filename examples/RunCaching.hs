module RunCaching where

import Control.Concurrent
import Control.Monad (forever)
import Frenetic.NetCore
import Frenetic.NetCore.Types
-- import Frenetic.EthernetAddress
import qualified Data.Map as Map 
import Data.Word
import Data.Bits
--import System.Random 

convert :: Policy -> (IPAddressPrefix, Port) -> Policy
convert pol (ipAddr,port) = 
  case port==0 of
         True -> ite (NwDst (ipAddr) <&&> DlTyp 0x0800) ([]) pol
	 False -> ite (NwDst (ipAddr) <&&> DlTyp 0x0800) (forward [port]) pol
  
makeAddr w1 w2 w3 w4 pr = IPAddressPrefix (ipAddress w1 w2 w3 w4) pr 

ite :: Predicate -> [Action] -> Policy -> Policy
ite pr acts pol = (pr ==> acts) <+> (pol <%> (Not pr))

generateIPs =
  let getIPs i = 
		  map (\x -> (100+i,100+i,100+i,x,32,0)) [1..10] ++
		  --map (\x -> (100+i,100+i,100+i,rand(1,99),32,0)) [1..10] ++
                  [(100+i,100+i,100+i,100+i,32,4*i+1),
                  (100+i,100+i,100+i,0,24, 4*i+2),
                  (100+i,100+i,0,0,16, 4*i+3),
                  (100+i,0,0,0,8, 4*i+4)]
  in
    foldl (\ips i -> ips ++ getIPs i) [] [0..0]		  

ipMap ips = map (\(x1,x2,x3,x4,x5,x6) -> ((makeAddr x1 x2 x3 x4 x5), fromIntegral x6 :: Word16) ) ips

getPolicy :: Policy
getPolicy = 
  foldl convert (Any ==> allPorts unmodified) ((reverse.ipMap) generateIPs)
{--  
  ite (NwDst (IPAddressPrefix (ipAddress 101 101 101 101) 32) <&&> DlTyp 0x0800) (forward [5]) (
   --ite (NwDst (IPAddressPrefix (ipAddress 10 0 0 3) 32) <&&> DlTyp 0x0800) (forward [2]) (
   ite (NwDst (IPAddressPrefix (ipAddress 101 101 101 1) 32) <&&> DlTyp 0x0800) ([]) (
   (Any ==> allPorts unmodified) ))
--}


main addr = do
  controller addr (getPolicy) 

