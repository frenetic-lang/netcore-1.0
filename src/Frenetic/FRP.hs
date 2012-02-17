

{-# LANGUAGE
Arrows
 #-}


module Language.FRP where

import FRP.Animas
import Frenetic.Language

type FreneticA a b = SF a b

-- TODO: fill in
type Headers = Int

query :: (ValidTransmission ptrn pkt) => Headers -> Predicate -> FreneticA () (Event (Transmission ptrn pkt))
query = undefined

-- Just trying to get some type signatures for now. You can feel free to add more combinators you feel should be in the language. Read the Animas docs!