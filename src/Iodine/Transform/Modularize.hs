{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Iodine.Transform.Modularize ( modularize
                                    ) where

import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.HashSet               as HS
import           Data.Foldable
import qualified Data.Sequence as SQ

import Iodine.Language.Types

type ABS = SQ.Seq AlwaysBlock

type States = (St, AnnotSt)

-- | modularize the module hierarchy
modularize :: States -> ABS
modularize input = evalState (m_flattenToAlways input mempty) 0

--------------------------------------------------------------------------------
-- Flattening the Verilog file
--------------------------------------------------------------------------------

type HS = State Int

m_flattenToAlways :: States -> ABS -> HS ABS
m_flattenToAlways (st, ast) = flip (foldM (\as ir -> flattenIR st ir as)) (st^.irs)
  where
    flattenIR :: St -> IR -> ABS -> HS ABS
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
      return $ l SQ.|> a
    flattenIR _  (ModuleInst{..}) l =
      m_flattenToAlways (modInstSt, ast) l

-- | Filter the ports and the uninterpreted functions of the state
filterSt :: Stmt -> St -> St
filterSt s st =
  over ports (filterVars vars') .
  set  irs   mempty $
  st
  where
    vars' = foldVariablesSet s -- all variables used in the statement

filterVars :: HS.HashSet Id -> SQ.Seq Var -> SQ.Seq Var
filterVars toKeep = SQ.filter (\v -> HS.member (varName v) toKeep)

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

    (regReadSet, regWriteSet) = readWriteSet rs stmt

    f (regs, wires) (Register r) = (HS.insert r regs, wires)
    f (regs, wires) (Wire w)     = (regs, HS.insert w wires)

type S2 = (HS.HashSet Id, HS.HashSet Id)

readWriteSet :: HS.HashSet Id -> Stmt -> S2 
readWriteSet regs stmt =
  let (r,w) = evalState (comp stmt >> get) (mempty, mempty)
  in (r `HS.intersection` regs, w `HS.intersection` regs)

  where
    comp :: Stmt -> State S2 ()
    comp (Block ss)            = sequence_ (comp <$> ss)
    comp (BlockingAsgn{..})    = do _1 %= HS.union (foldVariablesSet rhs)
                                    _2 %= HS.insert lhs
    comp (NonBlockingAsgn{..}) = do _1 %= HS.union (foldVariablesSet rhs)
                                    _2 %= HS.insert lhs
    comp (IfStmt{..})          = do _1 %= HS.union (foldVariablesSet ifCond)
                                    comp thenStmt
                                    comp elseStmt
    comp Skip                  = return ()
