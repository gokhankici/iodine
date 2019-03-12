{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize ( modularize
                                    ) where

import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.List (foldl')

import Verylog.Language.Types

type States = (St, AnnotSt)

-- | modularize the module hierarchy
modularize :: States -> [AlwaysBlock]
modularize input = evalState (m_flattenToAlways input []) 0

--------------------------------------------------------------------------------
-- Flattening the Verilog file
--------------------------------------------------------------------------------

type HS = State Int

m_flattenToAlways :: States -> [AlwaysBlock] -> HS [AlwaysBlock]
m_flattenToAlways (st, ast) = flip (foldM (\as ir -> flattenIR st ir as)) (st^.irs)
  where
    flattenIR :: St -> IR -> [AlwaysBlock] -> HS [AlwaysBlock]
    flattenIR stt (Always{..}) l = do
      i <- get
      put (i+1)
      let stt' = filterSt alwaysStmt stt
          a    = AB { _aEvent   = event
                    , _aStmt    = alwaysStmt
                    , _aId      = i
                    , _aSt      = stt'
                    , _aMd      = buildMetadata stt' alwaysStmt
                    , _aLoc     = alwaysLoc
                    }
      return (a:l)
    flattenIR _  (ModuleInst{..}) l =
      m_flattenToAlways (modInstSt, ast) l

-- | Filter the ports and the uninterpreted functions of the state
filterSt :: Stmt -> St -> St
filterSt s st =
  over ports (filterVars vars') .
  set  ufs   ufs' .
  set  irs   [] $
  st
  where
    vars0 = HS.fromList $ foldVariables s -- all variables used in the statement
    ufs'  = filterMap vars0 (st^.ufs)
    vars' = vars0 `HS.union` (HS.fromList $ concat $ snd <$> HM.elems ufs')

-- | Keep the keys that are present in the given set
filterMap :: HS.HashSet Id -> UFMap -> UFMap
filterMap toKeep = HM.filterWithKey (\k _ -> HS.member k toKeep)

filterVars :: HS.HashSet Id -> [Var] -> [Var]
filterVars toKeep = filter (\v -> HS.member (varName v) toKeep)

--------------------------------------------------------------------------------
-- Flattening the Verilog file
--------------------------------------------------------------------------------

buildMetadata :: St -> Stmt -> BlockMetadata
buildMetadata st stmt =
  set mRegisters   rs .
  set mWires       ws .
  set mRegReadSet  regReadSet .
  set mRegWriteSet regWriteSet $
  mempty
  where
    (rs,ws) = foldl' f (mempty, mempty) (st^.ports)

    (regReadSet, regWriteSet) = readWriteSet rs st stmt

    f (regs, wires) (Register r) = (HS.insert r regs, wires)
    f (regs, wires) (Wire w)     = (regs, HS.insert w wires)

readWriteSet :: HS.HashSet Id -> St -> Stmt -> (HS.HashSet Id, HS.HashSet Id)
readWriteSet regs st stmt =
  let (r,w) = evalState (comp stmt >> get) ([], [])
      r'    = readVars r
      w'    = writeVars w
  in (r' `HS.intersection` regs, w' `HS.intersection` regs)

  where
    us = st^.ufs

    readVars, writeVars :: [Id] -> HS.HashSet Id
    readVars l = foldl' (\s v -> case HM.lookup v us of
                                   Nothing     -> HS.insert v s
                                   Just (_,vs) -> foldl' (flip HS.insert) s vs) HS.empty l
    writeVars = HS.fromList
    
    comp :: Stmt -> State ([Id],[Id]) ()
    comp (Block ss)            = sequence_ (comp <$> ss)
    comp (BlockingAsgn{..})    = do _1 %= (:) rhs
                                    _2 %= (:) lhs
    comp (NonBlockingAsgn{..}) = do _1 %= (:) rhs
                                    _2 %= (:) lhs
    comp (IfStmt{..})          = do _1 %= (:) ifCond
                                    comp thenStmt
                                    comp elseStmt
    comp Skip                  = return ()
