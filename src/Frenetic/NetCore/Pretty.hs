module Frenetic.NetCore.Pretty
  ( prettyNetCore
  , putNetCore
  , putNetCoreLn
  , hPutNetCore
  ) where

import Data.List
import qualified Data.MultiSet as MS
import Frenetic.NetCore
import System.IO
import Text.PrettyPrint.ANSI.Leijen

-- |Pretty-print a netcore policy to a Doc
prettyNetCore = prettyPo
-- |Pretty-print a netcore policy to stdout
putNetCore = putDoc . prettyNetCore
putNetCoreLn p = do
  putNetCore p
  putChar '\n'
-- |Pretty-print a netcore policy to a handle
hPutNetCore h = hPutDoc h . prettyNetCore
hPutNetCoreLn h p = do
  hPutNetCore h p
  hPutChar h '\n'

prettyPr (PrPattern p) = prettyPattern " = " p
prettyPr (PrTo s) = text "switch = " <> (integer $ fromIntegral s)
prettyPr p@(PrUnion _ _) = text "Union " <>
                           (align . tupled . map prettyPr $ prUnUnion p)
prettyPr p@(PrIntersect _ _) = text "Intersect " <>
                           (align . tupled . map prettyPr $ prUnIntersect p)
prettyPr (PrNegate p) = text "Not " <> align (tupled [prettyPr p])

prettyAc (Action fwds _) = semiBraces . map prettyForward . MS.toAscList $ fwds

prettyPo PoBottom = text "Bottom"
prettyPo (PoBasic pr ac) = prettyPr pr </> text " ==> " <> align (prettyAc ac)
prettyPo p = list (map prettyPo (poUnUnion p)) -- safe because poUnUnion

-- |Render patterns as "{field<sep>value; field<sep>value}"
prettyPattern sep p = semiBraces . map text . interesting sep $ p

-- |Render a forwarding option as "port with {field := value; field := value}"
prettyForward (port, mods) = prettyPseudoPort port <> mods' where
  mods' = if interesting " := " mods == [] then empty
                         else text " with " <> prettyPattern " := " mods

-- |Render a pseudoport as "Port p" or "Flood"
prettyPseudoPort (Physical p) = text "Port " <> integer (fromIntegral p)
prettyPseudoPort PhysicalFlood = text "Flood"

-- |Get back all predicates in the union.  Does not return any naked unions.
prUnUnion :: Predicate -> [Predicate]
prUnUnion po = unfoldr f [po] where
  f predicates = case predicates of 
    [] -> Nothing
    (PrUnion p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)

-- |Get back all predicates in the intersection.  Does not return any naked intersections.
prUnIntersect :: Predicate -> [Predicate]
prUnIntersect po = unfoldr f [po] where
  f predicates = case predicates of 
    [] -> Nothing
    (PrIntersect p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)

-- |Get back all basic policies in the union.  Does not return any unions.
poUnUnion :: Policy -> [Policy]
poUnUnion po = unfoldr f [po] where
  f policies = case policies of 
    [] -> Nothing
    (PoUnion p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)
