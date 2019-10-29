{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Iodine.Transform.SSA
  ( ssa
  , SSAIR
  )
where

import Iodine.Language.IRParser (ParsedIR)
import Iodine.Language.IR
import Iodine.Language.Types

import           Control.Lens
import           Control.Monad.State.Lazy
-- import qualified Data.HashMap.Strict as HM
import           GHC.Generics (Generic)

data St = St { _abId   :: Int   -- id for always blocks
             , _stmtId :: Int   -- id for statements
             -- , _varId  :: HM.HashMap Id Int -- id for variables
             }
        deriving (Generic)

makeLenses ''St

type S = State St
type SSAIR = L (Module Int)

ssa :: ParsedIR -> SSAIR
ssa = fmap $ (flip evalState initialSt) . ssaModule
  where
    initialSt = St 0 0 -- HM.empty

ssaModule :: Module a -> S (Module Int)
ssaModule Module{..} =
  Module moduleName ports variables <$>
  traverse ssaStmt gateStmts <*>
  traverse ssaAB alwaysBlocks <*>
  return 0

ssaAB :: AlwaysBlock a -> S (AlwaysBlock Int)
ssaAB AlwaysBlock{..} =
  AlwaysBlock (const 0 <$> abEvent) <$>
  ssaStmt abStmt <*>
  ((abId += 1) *> use abId)

-- after this step, each new variable will have an unique id
ssaStmt :: Stmt a -> S (Stmt Int)
ssaStmt Skip{..} = Skip <$> freshStmtId
ssaStmt _ = undefined

freshStmtId :: S Int
freshStmtId = stmtId += 1 >> use stmtId

-- ssaStmt stmt = trace (show $ snd <$> stmt') undefined
--   where
--     (stmt', _n') = runState (traverse act stmt) 0

--     act :: a -> S (a, Int)
--     act a = do
--       n <- get
--       put (n+1)
--       return (a, n)

-- other stuff
