{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.InitialPass ( initialPass
                                     ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashSet             as S
import qualified Data.HashMap.Lazy        as M
import           Data.List
import           Text.PrettyPrint

import           Verylog.Language.Types
import           Verylog.Language.Utils

data St = St { _registers :: S.HashSet Id
             , _wires     :: S.HashSet Id
             , _ufs       :: M.HashMap Id [Id]
             , _sources   :: S.HashSet Id
             , _sinks     :: S.HashSet Id
             , _irs       :: [IR]
             }

makeLenses ''St

initialPass input = evalState pipeline initialSt
  where
    initialSt = St { _registers = S.empty
                   , _wires     = S.empty
                   , _ufs       = M.empty
                   , _sources   = S.empty
                   , _sinks     = S.empty
                   , _irs       = input
                   }
    pipeline  = collectVars >> dropIRs >> checkIR >> lastpass
    lastpass  = get -- use irs

-- -----------------------------------------------------------------------------
-- 1. Collect vars
-- -----------------------------------------------------------------------------

collectVars :: State St ()
collectVars = use irs >>= sequence_ . (map collectVar)

collectVar :: IR -> State St ()
collectVar (Register id) = registers %= S.insert id
collectVar (Wire id)     = wires     %= S.insert id
collectVar (UF id vars)  = ufs       %= M.insert id vars
collectVar (Source s)    = sources   %= S.insert s
collectVar (Sink s)      = sinks     %= S.insert s
collectVar _             = return ()

-- -----------------------------------------------------------------------------
-- 2. Drop vars from irs
-- -----------------------------------------------------------------------------

pass2Filter = isProcess .||. isGate

dropIRs :: State St ()
dropIRs = irs %= filter pass2Filter

-- -----------------------------------------------------------------------------
-- 3. Check IR elements
-- -----------------------------------------------------------------------------
checkIR :: State St ()
checkIR = do uses irs $ flip forM_ _checkIR
             return ()

_checkIR :: IR -> State St ()
_checkIR (Always{..})   = undefined
_checkIR (ContAsgn{..}) = undefined
_checkIR ir             = when (not $ pass2Filter ir)
                         (throw $ PassError "pass2Filter filter is wrong")

instance PPrint St where
  toDoc st = vcat $ stDoc : space : st^.irs.to (map toDoc)
    where
      stDoc = text "St" <+>
              vcat [ lbrace <+> text "regs " <+> equals <+> st^.registers.to printSet
                   , comma  <+> text "wires" <+> equals <+> st^.wires.to printSet
                   , comma  <+> text "ufs  " <+> equals <+> st^.ufs.to printMap
                   , comma  <+> text "srcs " <+> equals <+> st^.sources.to printSet
                   , comma  <+> text "sinks" <+> equals <+> st^.sinks.to printSet
                   , rbrace
                   ]
      printList   = brackets . text . (intercalate ", ")
      printSet    = printList . S.toList
      mapKV (k,l) = "(" ++ k ++ ", [" ++ (intercalate ", " l) ++ "])"
      printMap    = brackets
                    . text
                    . (intercalate ", ")
                    . (map mapKV)
                    . (filter (\(_,l) -> length l > 0))
                    . M.toList
