module Frenetic.Update where

import Frenetic.Common
import Nettle.OpenFlow.Match
import Frenetic.NetCore.Compiler
import Frenetic.NetCore.Types

-- type PortMap = Switch -> [Port]

explode_all_ports ver m all_ports edge_ports =
  map (\ p -> (case (p `elem` edge_ports) of
                  True -> Forward (Physical p) (m {modifyDlVlan = Just Nothing})
                  False -> Forward (Physical p) (m {modifyDlVlan = Just (Just ver)})
                 )) all_ports

version_acts [] version all_ports edge_ports = []
version_acts (Forward (Physical p') m : actions) version all_ports edge_ports =
  case (p' `elem` edge_ports) of
    True -> Forward (Physical p') (m {modifyDlVlan = Just Nothing}) : (version_acts actions version all_ports edge_ports)
    False -> Forward (Physical p') (m {modifyDlVlan = Just (Just version)}) : version_acts actions version all_ports edge_ports

version_acts (Forward AllPorts m : actions) version all_ports edge_ports =                
  explode_all_ports version m all_ports edge_ports ++ version_acts actions version all_ports edge_ports
              
-- version_acts (GetPacket a b : actions) version all_ports edge_ports =
--   GetPacket a b : version_acts actions version all_ports edge_ports

version_internal_pol (PoUnion p1 p2) ver sw all_ports ext_ports =
  PoUnion (version_internal_pol p1 ver sw all_ports ext_ports) (version_internal_pol p2 ver sw all_ports ext_ports)
version_internal_pol (PoBasic pr acts) ver sw all_ports ext_ports = 
  PoBasic (And (Switch sw) (And (DlVlan (Just ver)) pr)) (version_acts acts ver all_ports ext_ports)
version_internal_pol (Restrict p pr) ver sw all_ports ext_ports =
  Restrict (version_internal_pol p ver sw all_ports ext_ports) pr

version_full_pol (PoUnion p1 p2) ver sw all_ports ext_ports =
  PoUnion (version_full_pol p1 ver sw all_ports ext_ports) (version_full_pol p2 ver sw all_ports ext_ports)
version_full_pol (PoBasic pr acts) ver sw all_ports ext_ports =
  PoUnion (PoBasic (And (Switch sw) (And (DlVlan (Just ver)) pr)) (version_acts acts ver all_ports ext_ports)) 
  (PoBasic (And (And (Switch sw) (DlVlan Nothing)) pr) (version_acts acts ver all_ports ext_ports))
version_full_pol (Restrict p pr) ver sw all_ports ext_ports =
  Restrict (version_full_pol p ver sw all_ports ext_ports) pr


reduce_pol (PoUnion (PoBasic _ []) p) = reduce_pol p
reduce_pol (PoUnion p (PoBasic _ [])) = reduce_pol p
reduce_pol (PoUnion p1 p2) =
  let p1' = reduce_pol p1 in
        let p2' = reduce_pol p2 in
        case (p1', p2') of
          (PoBasic _ [], p2') -> p2'
          (p1', PoBasic _ []) -> p1'
          _ -> PoUnion p1' p2'
          
reduce_pol p = p

gen_update_pols _ _ [] _ _ =  (PoBasic None [], PoBasic None [])
gen_update_pols orig ver (sw : switches) allPorts extPorts =
  let (p1,p2) = gen_update_pols orig ver switches allPorts extPorts in 
  ((PoUnion (version_internal_pol orig ver sw (allPorts sw) (extPorts sw)) p1),
   (PoUnion (version_full_pol orig ver sw (allPorts sw) (extPorts sw)) p2))
  