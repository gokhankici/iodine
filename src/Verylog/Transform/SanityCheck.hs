{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.SanityCheck ( sanityCheck
                                     ) where

import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.List
import           Data.Maybe
import qualified Data.HashMap.Strict        as M
import           Text.Printf

import Verylog.Language.Types
-- import Verylog.Transform.Utils

data PassSt = PassSt { _bas    :: M.HashMap Id Int
                     , _nbas   :: M.HashMap Id Int
                     , _errors :: [PassError]
                     }

makeLenses ''PassSt

type S = State PassSt

freshKeepErrs :: S ()
freshKeepErrs = bas .= M.empty >> nbas .= M.empty

--------------------------------------------------------------------------------
sanityCheck :: [AlwaysBlock] -> [AlwaysBlock]  
--------------------------------------------------------------------------------
sanityCheck = check1 -- >>> checkLHSIsVar

--------------------------------------------------------------------------------
check1    :: [AlwaysBlock] -> [AlwaysBlock]
--------------------------------------------------------------------------------
check1 as = evalState comp (PassSt { _bas    = M.empty
                                   , _nbas   = M.empty
                                   , _errors = []
                                   })
  where
    comp :: S [AlwaysBlock]
    comp = do checkAssignments as
              es <- use errors
              case es of
                []    -> return as
                (e:_) -> throw e

--------------------------------------------------------------------------------
checkAssignments :: [AlwaysBlock] -> S ()
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
      -- FIXME
      -- uses nbas ((> 1) . M.lookupDefault 0 lhs)
      --   >>= flip when
      --   (addError $ printf "multiple non-blocking assignments to %s" lhs)

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

      let newThBAs  = M.differenceWith keepNew thBAs oldBAs
      let newElBAs  = M.differenceWith keepNew elBAs oldBAs
      let newThNBAs = M.differenceWith keepNew thNBAs oldNBAs
      let newElNBAs = M.differenceWith keepNew elNBAs oldNBAs

      let d1 = M.intersection newThBAs  newElNBAs
      let d2 = M.intersection newThNBAs newElBAs

      let pickFirstKey = fst . head . M.toList 

      when (not $ M.null d1) $
        addError $ printf "different types of assignments to %s" (pickFirstKey d1)

      when (not $ M.null d2) $ 
        addError $ printf "different types of assignments to %s" (pickFirstKey d2)

      bas  .= M.unionWith max thBAs  elBAs
      nbas .= M.unionWith max thNBAs elNBAs

--------------------------------------------------------------------------------
checkLHSIsVar :: [AlwaysBlock] -> [AlwaysBlock]
--------------------------------------------------------------------------------
checkLHSIsVar as = case foldl' (\es a -> check (a ^. aStmt) (a ^. aSt ^. ports) (a ^. aSt ^. ufs) es) [] as of
                     []  -> as
                     e:_ -> throw e
  where
    ------------------------------------------------------------
    check :: Stmt -> [Var] -> (M.HashMap Id [Id]) -> [PassError] -> [PassError]
    ------------------------------------------------------------
    check      (Block ss)            vs u es = foldl' (\es' s -> check s vs u es') es ss 
    check stmt@(BlockingAsgn l _)    vs u es = helper stmt l vs u es
    check stmt@(NonBlockingAsgn l _) vs u es = helper stmt l vs u es
    check      (IfStmt _ th el)      vs u es = foldl' (\es' s -> check s vs u es') es [th,el]
    check      Skip                  _  _ es = es

    helper stmt v vs m es =
      let isBlocking = case stmt of
                         BlockingAsgn _ _ -> True
                         _                -> False
          isReg      = isJust $ find (\a -> case a of
                                              Register r -> r == v
                                              Wire _     -> False) vs
      in case M.lookup v m of
           Nothing ->
             if   isBlocking && isReg
             then (PassError $ printf "lhs of blocking assignment %s is a register" (show stmt)) : es
             else es
           Just _  -> (PassError $ printf "lhs of %s is not a var" (show stmt)) : es

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------
incr     :: Id -> M.HashMap Id Int -> M.HashMap Id Int
incr v m = M.alter f v m
  where 
    f Nothing = Just 1
    f n       = (+1) <$> n

addError :: String -> S ()
addError s = errors %= (:) (PassError s)
