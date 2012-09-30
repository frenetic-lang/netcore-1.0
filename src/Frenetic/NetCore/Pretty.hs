module Frenetic.NetCore.Pretty
  ( toString
  , putNetCore
  , putNetCoreLn
  , hPutNetCore
  ) where

import Data.List
import Frenetic.NetCore.Short
import Frenetic.NetCore.Types
import Frenetic.NetCore.Util
import System.IO
import Text.PrettyPrint.ANSI.Leijen

ribbonFrac = 0.8
lineWidth = 100

render = renderPretty ribbonFrac lineWidth

-- |Pretty-print a netcore policy to a String
toString p = displayS (render $ prettyPo p) ""

-- |Pretty-print a netcore policy to stdout
putNetCore = hPutNetCore stdout
putNetCoreLn = hPutNetCoreLn stdout

-- |Pretty-print a netcore policy to a handle
hPutNetCore :: Handle -> Policy -> IO ()
hPutNetCore h p = do
  let rendered = render $ prettyPo p
  displayIO h rendered

hPutNetCoreLn h p = do
  hPutNetCore h p
  hPutChar h '\n'

prettyPr (Switch s) = text "switch = " <> integer (fromIntegral s)
prettyPr p@(Or _ _) = text "Or " <>
                           (align . tupled . map prettyPr $ prUnUnion p)
prettyPr p@(And _ _) = text "And " <>
                           (align . tupled . map prettyPr $ prUnIntersect p)
prettyPr (Not p) = text "Not " <> align (tupled [prettyPr p])
prettyPr x = text (show x)

prettyPo PoBottom = text "Bottom"
prettyPo (PoBasic pr ac) = prettyPr pr </> text " ==> " <> align (text $ show ac)
prettyPo p = list (map prettyPo (poUnUnion p)) -- safe because poUnUnion

-- |Render a forwarding option as "port with {field := value; field := value}"
prettyForward (port, mods) = prettyPseudoPort port <> mods' where
  mods' = if mods == unmodified then empty
          else text " with " <> text (show mods)

-- |Render a pseudoport as "Port p" or "Flood"
prettyPseudoPort (Physical p) = text "Port " <> integer (fromIntegral p)
prettyPseudoPort AllPorts = text "AllPorts"
prettyPseudoPort (ToQueue q) = text "ToQueue " <> prettyQueue q
prettyQueue (Queue s p q w) = align . tupled $ 
                              [text (show s), integer (fromIntegral p), 
                               integer (fromIntegral q), integer (fromIntegral w)]