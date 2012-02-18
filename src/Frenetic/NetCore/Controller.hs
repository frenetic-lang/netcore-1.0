--------------------------------------------------------------------------------
-- The Frenetic Project                                                       --
-- frenetic@frenetic-lang.org                                                 --
--------------------------------------------------------------------------------
-- Licensed to the Frenetic Project by one or more contributors. See the      --
-- NOTICE file distributed with this work for additional information          --
-- regarding copyright and ownership. The Frenetic Project licenses this      --
-- file to you under the following license.                                   --
--                                                                            --
-- Redistribution and use in source and binary forms, with or without         --
-- modification, are permitted provided the following conditions are met:     --
-- * Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- * Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- * The names of the copyright holds and contributors may not be used to     --
--   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- /src/Frenetic/NetCore/Controller.hs                                        --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------

module Frenetic.NetCore.Controller where

import Control.Monad.State.Lazy

import Frenetic.NetCore.API
import Frenetic.NetCore.Compiler

type Hook = IO ()

-- FIX this should maybe be an interval of rules? rule ids?
type PolicyID = Int

data Controller = Controller {
  ctrlPacketIn :: [Hook],
  ctrlSwitchJoin :: [Hook],
  ctrlSwitchLeave :: [Hook]
     }
                       
type ControllerM a = StateT Controller IO a 

mkController :: ControllerM ()  -> IO () 
mkController m = runStateT m >> return () 

withController :: ControllerID -> ControllerM () -> IO ()

installPolicy :: Policy -> ControllerM ()
installPolicy = undefined

installPartialPolicy :: Policy -> ControllerM PolicyID
installPartialPolicy = undefined

deletePartialPolicy :: PolicyID -> ControllerM ()
deletePartialPolicy = undefined

