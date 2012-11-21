module UpdateTest
  ( main
  ) where

import Control.Concurrent
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import Frenetic.NetCore
import Control.Monad (forever)
import Frenetic.NetCore.Util (poDom)

dlDst a = DlDst $ EthernetAddress (fromInteger a)
switch a = Switch $ fromInteger a
physical a = Physical $ fromInteger a

-- Maybe add an 'else' policy combinator?
-- pol1 `else` pol2 = pol1 <+> (pol2 <%> dom pol1)

-- I'd like to use <%> and change <|> to mean restriction...
infixr 5 <!>
(<!>) pol acts = pol <+> (Not (poDom pol) ==> acts)

pol1 = 
    ((((dlDst 1)
       ==> [ Forward (physical 1) unmodified ]
       <+>
       (dlDst 2)
       ==> [ Forward (physical 2) unmodified ]
       <+>
       (dlDst 3)
       ==> [ Forward (physical 3) unmodified ]
       <+>
       (dlDst 4)
       ==> [ Forward (physical 4) unmodified ])
      <!> [ Forward (physical 5) unmodified ])
     <%> (switch 101))
    
    <+>

    ((((dlDst 5)
       ==> [ Forward (physical 1) unmodified ]
       <+>
       (dlDst 6)
       ==>  [ Forward (physical 2) unmodified ]
       <+>
       (dlDst 7)
       ==> [ Forward (physical 3) unmodified ]
       <+>
       (dlDst 8)
       ==> [ Forward (physical 4) unmodified ])
      <!> [ Forward (physical 5) unmodified ])
     <%> (switch 102))
    
    <+>

    ((((dlDst 9)
       ==> [ Forward (physical 1) unmodified ]
       <+>
       (dlDst 10)
       ==> [ Forward (physical 2) unmodified ]
       <+>
       (dlDst 11)
       ==> [ Forward (physical 3) unmodified ]
       <+>
       (dlDst 12)
       ==> [ Forward (physical 4) unmodified ])
      <!> [ Forward (physical 5) unmodified ])
     <%> (switch 103))
    
    <+>

    ((((dlDst 13)
      ==> [ Forward (physical 1) unmodified ]
      <+>
      (dlDst 14)
      ==> [ Forward (physical 2) unmodified ]
      <+>
      (dlDst 15)
      ==> [ Forward (physical 3) unmodified ]
      <+>
      (dlDst 16)
      ==>  [ Forward (physical 4) unmodified ])
      <!> [ Forward (physical 5) unmodified ])
     <%> (switch 104))
    
    <+>

    (((((dlDst 1) <||> (dlDst 2) <||> (dlDst 3) <||> (dlDst 4))
       ==> [ Forward (physical 1) unmodified ]
       <+>
       ((dlDst 5) <||> (dlDst 6) <||> (dlDst 7) <||> (dlDst 8))
       ==> [ Forward (physical 2) unmodified ])
      <!> [ Forward (physical 3) unmodified ])
     <%> (switch 105))
    
    <+>

    (((((dlDst 9) <||> (dlDst 10) <||> (dlDst 11) <||> (dlDst 12))
       ==> [ Forward (physical 1) unmodified ]
       <+>
       ((dlDst 13) <||> (dlDst 14) <||> (dlDst 15) <||> (dlDst 16))
       ==> [ Forward (physical 2) unmodified ])
      <!> [ Forward (physical 3) unmodified ])
     <%> (switch 106))

extPorts 101 = [1, 2, 3, 4]
extPorts 102 = [1, 2, 3, 4]
extPorts 103 = [1, 2, 3, 4]
extPorts 104 = [1, 2, 3, 4]
extPorts _ = []

pol2 = ((((dlDst 2)
       ==> [ Forward (physical 1) unmodified ]
       <+>
       (dlDst 3)
       ==> [ Forward (physical 2) unmodified ]
     <%> (switch 1))

main = do
  polChan <- newChan
  writeChan polChan (pol2, extPorts)
  consistentController polChan
