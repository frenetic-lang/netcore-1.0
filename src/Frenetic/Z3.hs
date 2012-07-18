module Frenetic.Z3
  ( -- * Data types and constructors for running Z3
    Z3Input (..)
  , setUp
  , DeclConst (..)
  , Const (..)
  -- * Z3 formula components
  , Z3Packet (..)
  , Z3Int (..)
  , BoolExp (..)
  , IntExp (..)
  , nAnd
  , nOr
  -- * Tools to run Z3
  , check
  , checkBool
  ) where

import Data.List
import Data.Maybe
import System.Process
import System.IO

join = intercalate

data Z3Input = Input [Declaration] [DeclConst] [BoolExp]

data Declaration = PacketSort
                 | Header String

instance Show Declaration where
  show PacketSort = "(declare-sort Packet)"
  show (Header name) = "(declare-fun " ++ name ++ " (Packet) Int)"

data Z3Packet = Z3Packet String

data Z3Int = Z3Int String

data DeclConst = DeclConst Const

instance Show DeclConst where
  show (DeclConst (ConstPacket (Z3Packet name))) =
    "(declare-const " ++ name ++ " Packet)"
  show (DeclConst (ConstInt (Z3Int name))) =
    "(declare-const " ++ name ++ " Int)"

data Const = ConstPacket Z3Packet
           | ConstInt Z3Int

instance Show Const where
  show (ConstPacket (Z3Packet name)) = "(" ++ name ++ " Packet)"
  show (ConstInt (Z3Int name)) = "(" ++ name ++ " Int)"

data BoolExp = ZTrue
             | ZFalse
             | Not BoolExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Implies BoolExp BoolExp
             | Equals IntExp IntExp
             | ForAll [Const] BoolExp
             | Exists [Const] BoolExp

data IntExp = Primitive Integer
            | PktHeader String Z3Packet
            | Variable Z3Int

setUp = [PacketSort
        , Header "DlSrc"
        , Header "DlDst"
        , Header "DlTyp"
        , Header "DlVlan"
        , Header "DlVlanPcp"
        , Header "NwSrc"
        , Header "NwDst"
        , Header "NwProto"
        , Header "NwTos"
        , Header "TpSrc"
        , Header "TpDst"
        , Header "InPort"
        , Header "Switch"
        ]

instance Show Z3Input where
  show (Input decls consts exprs) = join "\n" lines where
    lines = (map show decls) ++
            (map show consts) ++
            (map (\ b -> "(assert " ++ b ++ ")") . map show $ exprs) ++
            ["(check-sat)", "(get-model)"]

instance Show BoolExp where
  show ZTrue = "true"
  show ZFalse = "false"
  show (Not b) = "(not " ++ (show b) ++ ")"
  show (And b1 b2) = "(and " ++ (show b1) ++ " " ++  (show b2) ++ ")"
  show (Or b1 b2) = "(or " ++ (show b1) ++ " " ++  (show b2) ++ ")"
  show (Implies b1 b2) = "(implies " ++ (show b1) ++ " " ++  (show b2) ++ ")"
  show (Equals i1 i2) = "(equals " ++ (show i1) ++ " " ++  (show i2) ++ ")"
  show (ForAll consts b) = "(forall (" ++ (join " " (map show consts)) ++ ") " ++
                                    (show b) ++ ")"
  show (Exists consts b) = "(exists (" ++ (join " " (map show consts)) ++ ") " ++
                                  (show b) ++ ")"

instance Show IntExp where
  show (Primitive i) = show i
  show (PktHeader field (Z3Packet packet)) = "(" ++ field ++ " " ++ packet ++ ")"
  show (Variable (Z3Int name)) = name

-- |Nary And, return True if empty
nAnd :: [BoolExp] -> BoolExp
nAnd [] = ZTrue
nAnd bs = foldr1 (\ b1 b2 -> And b1 b2) bs

-- |Nary Or, return False if empty
nOr :: [BoolExp] -> BoolExp
nOr [] = ZFalse
nOr bs = foldr1 (\ b1 b2 -> Or b1 b2) bs

-- |Convert true-is-Just-false-is-Nothing to boolean
checkBool :: IO (Maybe a) -> IO Bool
checkBool = fmap isJust

-- use smt2 syntax, disable warnings, emit the model, use stdin
z3Process = CreateProcess (ShellCommand "z3 -smt2 -nw -m -in")
                          Nothing -- No working directory
                          Nothing -- No special environment
                          CreatePipe -- Pipe to stdin
                          CreatePipe -- Pipe from stdout
                          Inherit -- Propagate errors upward
                          True -- Close other filehandles
                          False -- Don't create a new process group
-- |Determine if a formula is SAT, returning the model or nothing.
check :: Z3Input -> IO (Maybe String)
check z3input = do
  let assertions = show z3input
  (Just inh, Just outh, _, p) <- createProcess z3Process
  hPutStrLn inh assertions
  hClose inh
  output <- hGetContents outh
  let result : model = lines output
  if result == "unsat" then return Nothing
  else if result == "sat" then return (Just (join "\n" $ model))
  else error ("Unknown status from z3: " ++ result ++ "\n" ++
              "This was the input to z3: \n" ++ assertions)
