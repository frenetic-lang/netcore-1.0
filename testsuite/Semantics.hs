module Semantics where

import qualified Data.Map as M
import Common
import Data.Maybe
import Nettle.OpenFlow (PacketInfo (..), matchAny)
import Frenetic.Switches.OpenFlow (toPacket)
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Compiler
import Frenetic.NetCore.Types
import Frenetic.NetCore.Reduce (isEmptyPredicate)

tests = $(testGroupGenerator)

prop_simpleFwd = do
  inp <- arbInPkt
  case evalPol (PolProcessIn (Switch 3) [ActFwd AllPorts unmodified]) inp of
    [OutPkt 3 AllPorts _ _] -> label "on 3" True
    [] -> label "trivially true" True
    out -> printTestCase (printf "inp=%s\nout=%s" (show inp) (show out)) False

prop_dropAll = do
  inp <- arbitrary
  let out = evalPol (PolProcessIn None [ActFwd AllPorts unmodified]) inp
  label "should drop" (null out)

simplCallbackDelayStream cbs = map f (callbackDelayStream cbs)
  where f v = case v of
          Left (id, _) -> Left id
          Right t -> Right t

case_callbackDelayStream1 = do
  let cbs = M.fromList [ (50, CallbackPktCounter 3 (\_ -> return ()))
                       ]
  let stream = take 4 (simplCallbackDelayStream cbs)
  let expected = [Right 3, Left 50, Right 3, Left 50]
  assertEqual "should be equal"  expected stream


case_callbackDelayStream2 = do
  let cbs = M.fromList [ (50, CallbackPktCounter 3 (\_ -> return ()))
                       , (75, CallbackPktCounter 5 (\_ -> return ()))
                       ]
  let expected = [ Right 3, Left 50
                 , Right 2, Left 75 
                 , Right 1, Left 50
                 , Right 3, Left 50
                 , Right 1, Left 75 
                 , Right 2, Left 50
                 , Right 3, Left 50, Left 75 -- 15 timesteps, both fire
                 ]
  let stream = take (length expected) (simplCallbackDelayStream cbs)
  assertEqual "should be equal" expected stream

prop_preimageAny = do
  pt <- arbitrary
  mod <- arbitrary
  let act = ActFwd (Physical pt) mod
  let preimg = preimgOfAct act matchAny
  printTestCase ("Action is: "++ show act ++ "Preimg is : " ++ show preimg) 
   (isJust preimg)

prop_compileOK = do
  pol <- arbitrary
  pkt <- arbitrary
  sw <- arbitrary
  pt <- arbitrary
  bufferID <- arbitrary
  let inp = InPkt (Loc sw pt) pkt bufferID
  let expected = evalPol pol inp
  let classifier = compile sw pol
  case classify sw pt pkt classifier of
    Nothing -> 
      printTestCase "classifier was not total" $
        printTestCase (show (pol,sw,pt,classifier,pkt,expected)) $
          False
    Just acts -> 
      printTestCase
        ("Policy: " ++ show  pol ++ 
         "\n\nClassifier: " ++ show classifier ++
         "\n\nSwitch: " ++ show sw ++
         "\n\nPort: " ++ show pt ++
         "\n\nPacket: " ++ show pkt ++
         "\nExpected: " ++ show expected ++
         "\n\nBut got: " ++ show acts)
        (expected == (map (\a -> evalAct a inp) acts))

isNotOutNothing OutNothing = False
isNotOutNothing _ = True

-- TODO(arjun): do not duplicate code
-- Forces lots of sequencing tests that actually process packets
prop_compileSeqOK = do
  pred1 <- arbitrary
  pred2 <- arbitrary
  fwds1 <- arbFwds
  fwds2 <- arbFwds
  pkt <- arbitrary
  sw <- arbitrary
  pt <- arbitrary
  bufferID <- arbitrary
  let mkPreimg act1 (Bone pat2 acts2) = case preimgOfAct act1 pat2 of
          Just pat2' -> Just (Bone pat2' (catMaybes (map (seqAct act1) acts2)))
          Nothing -> Nothing
  let pol1 = (PolProcessIn pred1 fwds1)
  let pol2 = (PolProcessIn pred2 fwds2)
  let pol = PolSeq pol1 pol2
  let inp = InPkt (Loc sw pt) pkt bufferID
  let expected = filter isNotOutNothing $ evalPol pol inp
  let classifier = compile sw pol
  let cf1 = compilePolicy sw pol1 
  let cf2 = compilePolicy sw pol2
  let acts1 = classifierActions cf1
  let cf2Preimages = map (\act1 -> mapMaybe (mkPreimg act1) cf2) acts1
  case classify sw pt pkt classifier of
    Nothing -> 
      printTestCase "classifier was not total" $
        printTestCase (show (pol,sw,pt,classifier,pkt,expected)) $
          False
    Just acts -> 
      let outs = filter isNotOutNothing  $ map (\a -> evalAct a inp) acts in
      printTestCase
        ("Policy: " ++ show  pol ++ 
         "\n\nClassifier: " ++ show classifier ++
         "\n\nClassifier 1: " ++ show (compile sw pol1) ++
         "\n\nClassifier 2: " ++ show (compile sw pol2) ++
         "\n\nacts1 = " ++ show acts1 ++
         "\n\ncf2Preimgs = " ++ show cf2Preimages ++
         "\n\nSwitch: " ++ show sw ++
         "\n\nPort: " ++ show pt ++
         "\n\nPacket: " ++ show pkt ++
         "\nExpected: " ++ show expected ++
         "\n\nBut got: " ++ show outs ++
         "\n\nexpected len: " ++ show (length expected) ++
         "\n\ngot len: " ++ show (length outs))
        (expected == outs)

{-
xcase_seq_bug1 = classify sw pt pkt classifier = Just acts
 where sw = 12671443700482
       pt = 6954
       pk = Packet {pktDlSrc = 36:5c:4d:64:5:2c, pktDlDst = 56:a1:8d:99:1b:68, pktDlTyp = 2054, pktDlVlan = Nothing, pktDlVlanPcp = 208, pktNwSrc = Just 008.168.045.002, pktNwDst = Just 000.160.069.226, pktNwProto = 144, pktNwTos = 64, pktTpSrc = Just 56649, pktTpDst = Just 24756}
      
    -}