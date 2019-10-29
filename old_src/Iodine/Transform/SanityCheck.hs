{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Iodine.Transform.SanityCheck ( sanityCheck
                                     ) where

import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.IntSet                as IS
import           Data.List
import           Text.Printf
import qualified Data.Sequence as SQ
-- import           Debug.Trace

import Iodine.Language.Types
import Iodine.Transform.Utils

data PassSt = PassSt { _bas    :: HM.HashMap Id Int
                     , _nbas   :: HM.HashMap Id Int
                     , _errors :: [PassError]
                     }

makeLenses ''PassSt

type ABS = SQ.Seq AlwaysBlock
type S = State PassSt

freshKeepErrs :: S ()
freshKeepErrs = bas .= HM.empty >> nbas .= HM.empty

--------------------------------------------------------------------------------
sanityCheck :: ABS -> ABS
--------------------------------------------------------------------------------
sanityCheck =
  check1 >>>
  checkAssignmentTypes >>>
  varSingleUpdate

  -- where
  --   printCount as = trace (printf "# blocks before merge: %d" (length as)) as

--------------------------------------------------------------------------------
check1    :: ABS -> ABS
--------------------------------------------------------------------------------
check1 as = evalState comp (PassSt { _bas    = HM.empty
                                   , _nbas   = HM.empty
                                   , _errors = []
                                   })
  where
    comp :: S ABS
    comp = do checkAssignments as
              es <- use errors
              case es of
                []    -> return as
                (e:_) -> throw e

--------------------------------------------------------------------------------
checkAssignments :: ABS -> S ()
--------------------------------------------------------------------------------
checkAssignments as = freshKeepErrs >> mapM_ (checkStmt . view aStmt) as
  where
    ----------------------------------------------------------------------------
    checkStmt :: Stmt -> S ()
    ----------------------------------------------------------------------------
    checkStmt Skip                  = return ()
    checkStmt (Block ss)            = sequence_ $ checkStmt <$> ss
    checkStmt (BlockingAsgn{..})    = bas %= incr lhs

    checkStmt (NonBlockingAsgn{..}) = do
      nbas %= incr lhs

    checkStmt (IfStmt{..}) = do
      oldBAs  <- use bas
      oldNBAs <- use nbas

      checkStmt thenStmt

      thBAs  <- use bas
      thNBAs <- use nbas
      bas    .= oldBAs
      nbas   .= oldNBAs

      checkStmt elseStmt

      elBAs  <- use bas
      elNBAs <- use nbas

      let keepNew this old = if this > old then Just this else Nothing

      let newThBAs  = HM.differenceWith keepNew thBAs oldBAs
      let newElBAs  = HM.differenceWith keepNew elBAs oldBAs
      let newThNBAs = HM.differenceWith keepNew thNBAs oldNBAs
      let newElNBAs = HM.differenceWith keepNew elNBAs oldNBAs

      let d1 = HM.intersection newThBAs  newElNBAs
      let d2 = HM.intersection newThNBAs newElBAs

      let pickFirstKey = fst . head . HM.toList

      when (not $ HM.null d1) $
        addError $ printf "different types of assignments to %s" (pickFirstKey d1)

      when (not $ HM.null d2) $
        addError $ printf "different types of assignments to %s" (pickFirstKey d2)

      bas  .= HM.unionWith max thBAs  elBAs
      nbas .= HM.unionWith max thNBAs elNBAs


--------------------------------------------------------------------------------
checkAssignmentTypes :: ABS -> ABS
--------------------------------------------------------------------------------
checkAssignmentTypes as =
  if   all rightAsgn as
  then as
  else throw $ PassError "checkAssignmentTypes failed"

  where
    rightAsgn :: AlwaysBlock -> Bool
    rightAsgn a =
      let s = a ^. aStmt
      in case a ^. aEvent of
           Star       -> h (not . isNonBlockingAsgn) s
           Assign     -> True
           NegEdge _  -> h isNonBlockingAsgn s
           PosEdge _  -> h isNonBlockingAsgn s

    isNonBlockingAsgn :: Stmt -> Bool
    isNonBlockingAsgn (NonBlockingAsgn{..}) = True
    isNonBlockingAsgn (BlockingAsgn{..})    = False
    isNonBlockingAsgn s                     = error $ "isBlockingAsgn called with " ++ show s

    h :: (Stmt -> Bool) -> Stmt -> Bool
    h _ Skip         = True
    h f (IfStmt{..}) = h f thenStmt && h f elseStmt
    h f (Block ss)   = all (h f) ss
    h f s            = f s

--------------------------------------------------------------------------------
varSingleUpdate :: ABS -> ABS
--------------------------------------------------------------------------------
varSingleUpdate as =
  case duplicateUpdates of
    []   -> as
    dups -> throw . PassError $
            "sanity check fail: varSingleUpdate\n" ++
            show dups
  where
    duplicateUpdates = filter ((/=) 1 . IS.size . snd) (HM.toList updateMap)

    updateMap :: HM.HashMap Id IS.IntSet
    updateMap = foldl' (HM.unionWith IS.union) HM.empty (h2 <$> as)

    h2 a = HM.fromList [ (l, IS.singleton (a^.aId)) | l <- HS.toList $ getLhss (a^.aStmt) ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------
incr     :: Id -> HM.HashMap Id Int -> HM.HashMap Id Int
incr v m = HM.alter f v m
  where
    f Nothing = Just 1
    f n       = (+1) <$> n

addError :: String -> S ()
addError s = errors %= (:) (PassError s)
