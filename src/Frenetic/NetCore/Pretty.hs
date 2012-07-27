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

ribbonFrac = 0.8
lineWidth = 100

-- |Pretty-print a netcore policy to a Doc
prettyNetCore = prettyPo

-- |Pretty-print a netcore policy to stdout
putNetCore = hPutNetCore stdout
putNetCoreLn = hPutNetCoreLn stdout

-- |Pretty-print a netcore policy to a handle
hPutNetCore :: Handle -> Policy -> IO ()
hPutNetCore h p = do
  let rendered = renderPretty ribbonFrac lineWidth $ prettyPo p
  displayIO h rendered

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

prettyAc (Action fwds qs) =
  (semiBraces . map prettyForward . MS.toAscList $ fwds) </>
  text "emit " <>
  (semiBraces . map integer . map fromIntegral . map idOfQuery $ qs)

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
