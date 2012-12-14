module Frenetic.NetCore.JSON 
  ( NetCoreMessage (..)
  , makeRuntime
  , hGetNetCoreMessage
  , hPutNetCoreMessage
  , hPutNetCoreError
  ) where

import Frenetic.Common
import Frenetic.NetCore.Types
import Frenetic.NetCore.Semantics
import Frenetic.Topo (Switch)
import Text.JSON
import Text.JSON.Generic
import Control.Monad
import qualified Data.Map as M
import System.IO
import Data.Generics
import Data.List


data NetCoreMessage 
  = MsgPolicy Pol -- ^Deploy a policy to the hypervisor.
  | MsgSendPacket Id Loc ByteString -- ^Inject a packet into the network.
  | MsgCountPackets Id Switch Integer -- ^Response from the hypervisor.
  | MsgCountBytes Id Switch Integer -- ^Response from the hypervisor.
  | MsgGetPacket Id Loc Packet -- ^Response from the hypervisor.
  | MsgMonitorSwitch Id SwitchEvent -- ^Response from the hypervisor.
  | MsgError String -- ^Error message.  Response from the hypervisor.
  deriving (Eq, Show, Data, Typeable)

-- |Build the runtime components necessary to deploy a semantic policy (for
-- example, if the policy arrived via a JSON message rather than being
-- generated from a Policy object).  That is, given a semantic policy, replace
-- Ids in queries with callbacks that package the query response as a 
-- NetCoreMessage and write it to a channel.  Create new channels for 
-- PolGenPacket policy fragments.
makeRuntime :: Chan NetCoreMessage
            -> Pol
            -> IO (Callbacks, 
                   [(Id, Chan (Loc, ByteString))])
makeRuntime toRemoteChan pol = do
  ((countIds, countBIds, getIds, monIds), chans) <- makeRuntime' pol
  responseChan <- newChan
  let m1 = foldl' (\m (id,int) -> M.insert id (countCb responseChan int id) m) 
                  M.empty 
                  countIds
  let m2 = foldl' (\m (id,int) -> M.insert id (countBCb responseChan int id) m) 
                  m1 
                  countBIds
  let m3 = foldl' (\m id -> M.insert id (getCb responseChan id) m) m2 getIds
  let m4 = foldl' (\m id -> M.insert id (monCb responseChan id) m) m3 monIds
  return (m4, chans)
  where
    -- Collect ids (and intervals) from actions, and create a new (id, chan)
    -- pair PolGenPacket policies.
    makeRuntime' :: Pol 
                 -> IO (([(Id,Int)], [(Id,Int)], [Id], [Id]), 
                        [(Id, Chan (Loc, ByteString))])
    makeRuntime' PolEmpty = return (([],[],[],[]),[])
    makeRuntime' (PolProcessIn _ acts) = do
      let actIds = foldl' extractIds ([],[],[],[]) acts
      return (actIds,[])
    makeRuntime' (PolUnion p1 p2) = do
      ((a, b, c, d), e)      <- makeRuntime' p1
      ((a', b', c', d'), e') <- makeRuntime' p2
      return ((a++a', b++b', c++c', d++d'), e++e')
    makeRuntime' (PolRestrict p _) = makeRuntime' p
    makeRuntime' (PolGenPacket id) = do
      injectChan <- newChan
      return (([],[],[],[]), [(id, injectChan)])

    -- Get IDs from actions.
    extractIds lists (ActFwd _ _) = lists
    extractIds (countIds, b, c, d) (ActQueryPktCounter id int) = 
      ((id, int):countIds, b, c, d)
    extractIds (a, countBIds, c, d) (ActQueryByteCounter id int) = 
      (a, (id, int):countBIds, c, d)
    extractIds (a, b, getIds, d) (ActGetPkt id) = (a, b, id:getIds, d)
    extractIds (a, b, c, monIds) (ActMonSwitch id) = (a, b, c, id:monIds)

    -- Make callbacks.
    countCb chan int id = 
      CallbackPktCounter int (\(s,i) -> writeChan chan $ MsgCountPackets id s i)
    countBCb chan int id = 
      CallbackByteCounter int (\(s,i) -> writeChan chan $ MsgCountBytes id s i)
    getCb chan id =
      CallbackGetPkt (\(loc, pkt) -> writeChan chan $ MsgGetPacket id loc pkt)
    monCb chan id =
      CallbackMonSwitch (\swe -> writeChan chan $ MsgMonitorSwitch id swe)


hGetCharTimeout :: Handle -> Int -> IO (Maybe Char)
hGetCharTimeout h t = do
  ready <- hWaitForInput h t
  if (not ready) 
    then return Nothing
    else do
      c <- hGetChar h
      return $ Just c

data JSONParserState
  = InObject
  | InString
  | InStringEscape

-- |Read a single JSON object from the handle, failing if the given
-- timeout elapses between any two character reads.
hReadJSONObjectTimeout :: Handle 
                       -> Int  -- ^Timeout (in milliseconds) to wait before 
                               -- receiving the first character of a new 
                               -- object.  A negative value indicates no 
                               -- timeout.
                       -> Int  -- ^Timeout (in milliseconds) to wait for each
                               -- next character after the first.
                       -> IO (Either String String)
hReadJSONObjectTimeout hdl firstTimeout charTimeout = do
  let makeReturn :: Either String String -> Char -> IO (Either String String)
      makeReturn rv c = case rv of 
      {Left msg -> return rv; Right str -> return $ Right (c:str)}
  let parse' :: Integer -> JSONParserState -> IO (Either String String)
      parse' depth InObject = do
          tok <- hGetCharTimeout hdl charTimeout
          case tok of
            Nothing -> return $ Left "Connection timed out."
            Just '}' 
              | depth == 0 -> return $ Right "}"
              | otherwise -> do
                  rv <- parse' (depth-1) InObject
                  makeReturn rv '}'
            Just '{' -> do
                rv <- parse' (depth+1) InObject
                makeReturn rv '{'
            Just '"' -> do
                rv <- parse' depth InString
                makeReturn rv '"'
            Just c -> do
                rv <- parse' depth InObject
                makeReturn rv c
      parse' depth InString = do
          tok <- hGetCharTimeout hdl charTimeout
          case tok of
            Nothing -> return $ Left "Connection timed out."
            Just '"' -> do
              rv <- parse' depth InObject
              makeReturn rv '"'
            Just '\\' -> do
              rv <- parse' depth InStringEscape
              makeReturn rv '\\'
            Just c -> do
              rv <- parse' depth InString
              makeReturn rv c
      parse' depth InStringEscape = do
          tok <- hGetCharTimeout hdl charTimeout
          case tok of
            Nothing -> return $ Left "Connection timed out."
            Just c -> do
              rv <- parse' depth InString
              makeReturn rv c
  let handlePreObjWhitespace = do
      tok <- hGetCharTimeout hdl firstTimeout
      case tok of
        Nothing   -> return $ Left "Connection timed out."
        Just '{'  -> do
          rv <- parse' 0 InObject
          makeReturn rv '{'
        Just ' '  -> handlePreObjWhitespace
        Just '\t' -> handlePreObjWhitespace
        Just '\r' -> handlePreObjWhitespace
        Just '\n' -> handlePreObjWhitespace
        Just c    -> 
          return $ Left ("The first character of a JSON object must be '{'.  " ++
                         "Received '" ++ show c ++ "'.")
  handlePreObjWhitespace

hGetNetCoreMessage :: Handle 
                   -> Int   -- ^Timeout (in milliseconds) to wait for the first
                            -- character of the object to arrive.
                   -> IO (Either String NetCoreMessage)
hGetNetCoreMessage hdl timeout = do
  tryReadObj <- hReadJSONObjectTimeout hdl timeout 30
  case tryReadObj of
    Left err -> return $ Left err
    Right objStr -> do
        -- TODO: do something smarter than decodeJSON for handling
        -- errors.
        return . Right $ decodeJSON objStr

hPutNetCoreMessage :: Handle -> NetCoreMessage -> IO ()
hPutNetCoreMessage hdl msg = hPutStr hdl $ encodeJSON msg

hPutNetCoreError :: Handle -> String -> IO ()
hPutNetCoreError hdl err = hPutNetCoreMessage hdl $ MsgError err

-- instance JSON EthernetAddress where
--   showJSON :: EthernetAddress -> JSValue
--   showJSON EthernetAddress{..} =
--     let intVal :: Integer
--         intVal = fromIntegral (unpackEth64 `mod` 0x01000000000000) in
--     showJSON intVal
-- 
--   readJSON :: JSValue -> Result EthernetAddress
--   readJSON (JSRational False i) 
--     | 0 < i && i < 0x01000000000000 = EthernetAddress i
--   readJSON _ = Error "Couldn't read ethernet address."

