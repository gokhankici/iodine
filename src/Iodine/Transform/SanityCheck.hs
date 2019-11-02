{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Iodine.Transform.SanityCheck
  ( sanityCheck
  )
where

import Iodine.Language.Types
import Iodine.Language.IR
import Iodine.Language.IRParser (ParsedIR)
import Iodine.Language.Annotation

import qualified Control.Exception as E
import           Control.Monad
import           Data.Foldable
import           Data.Function
import qualified Data.HashSet as HS
import           Data.Maybe
import qualified Data.Sequence as SQ
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.State
import           Text.Printf

type K1 = (Id, Id)
type S1 = HS.HashSet K1

data UniqueUpdateCheck m a where
  CheckPrevious :: S1 -> UniqueUpdateCheck m ()

makeSem ''UniqueUpdateCheck

type I = (ParsedIR, AnnotationFile ())

type FD r = Members '[ Error String                   -- sanity error
                     , Reader ParsedIR                -- parsed IR
                     , Reader (AnnotationFile ())     -- parsed annotation file
                     , State (Maybe (Module ()))      -- current module
                     , State (Maybe (AlwaysBlock ())) -- current always block
                     ] r

-- -----------------------------------------------------------------------------
sanityCheck :: I -> I
-- -----------------------------------------------------------------------------
sanityCheck input@(modules, af) = 
  case result of
    Left err -> E.throw $ SanityCheckError err
    Right _ -> input
  where result = allChecks
                 & runError @String
                 & runReader modules
                 & runReader af
                 & evalState @(Maybe (Module ())) Nothing
                 & evalState @(Maybe (AlwaysBlock ())) Nothing
                 & run

allChecks :: FD r => Sem r ()
allChecks = sequence_ [ checkAssignmentsAreToLocalVariables
                      , checkSameAssignmentType
                      , checkUniqueUpdateLocationOfVariables
                      ]

checkHelper :: FD r => (Stmt () -> Sem r ()) -> Sem r ()
checkHelper goS = ask @ParsedIR >>= traverse_ (checkModule goS)

checkModule :: FD r => (Stmt () -> Sem r ()) -> Module () -> Sem r ()
checkModule goS m@Module{..} = do
  put (Just m)
  put @(Maybe (AlwaysBlock ())) Nothing
  traverse_ goS gateStmts
  let goAB ab@AlwaysBlock{..} = put (Just ab) >> goS abStmt
  traverse_ goAB alwaysBlocks

getModuleName :: FD r => Sem r Id
getModuleName = (moduleName . fromJust) <$> get @(Maybe (Module ()))

-- -----------------------------------------------------------------------------
checkAssignmentsAreToLocalVariables :: FD r => Sem r ()
-- -----------------------------------------------------------------------------
checkAssignmentsAreToLocalVariables =
  checkHelper $ handleAssignment $
  \assignmentType assignmentLhs assignmentRhs -> do
    moduleName <- getModuleName
    when (varModuleName assignmentLhs /= moduleName) $
      let stmt = Assignment{ stmtData = (), .. }
      in throw @String $ printf "%s :: lhs is not from the module %s" (show stmt) moduleName

handleAssignment :: (AssignmentType -> Expr a -> Expr a -> Sem r ())
                 -> Stmt a
                 -> Sem r ()
handleAssignment handler = go
  where
    gos = traverse_ go
      
    go Block{..}          = gos blockStmts
    go IfStmt{..}         = gos (ifStmtThen SQ.<| ifStmtElse SQ.<| SQ.empty)
    go Assignment{..}     = handler assignmentType assignmentLhs assignmentRhs
    go ModuleInstance{..} = pure ()
    go PhiNode{..}        = error "phinode encountered in sanity check"
    go Skip{..}           = pure ()

-- -----------------------------------------------------------------------------
checkSameAssignmentType :: FD r => Sem r ()
-- -----------------------------------------------------------------------------
checkSameAssignmentType =
  checkHelper $ handleAssignment $
  \assignmentType assignmentLhs assignmentRhs -> do
    let stmt = Assignment{stmtData = (), ..}
    mAB <- get @(Maybe (AlwaysBlock ()))
    case mAB of
      Nothing ->
        when (assignmentType /= Continuous) $
        throw @String $ printf "%s :: Assignments outside always blocks should be continous" (show stmt)
      Just ab@AlwaysBlock{..} ->
        let err  = throw @String $ printf
                   "%s does not match the event in %s" (show stmt) blockStr
            err2 = throw @String $ printf
                   "continuous assignment should not appear in an always block %s" blockStr
            blockStr = let maxLength = 200
                           str = show ab
                       in if length str > maxLength
                          then (take maxLength str) ++ " ..."
                          else str
        in case (abEvent, assignmentType) of
             (_, Continuous)         -> err2
             (Star{..}, NonBlocking) -> err
             (PosEdge{..}, Blocking) -> err
             (NegEdge{..}, Blocking) -> err
             _                       -> pure ()

-- -----------------------------------------------------------------------------
checkUniqueUpdateLocationOfVariables :: FD r => Sem r ()
-- -----------------------------------------------------------------------------
checkUniqueUpdateLocationOfVariables =
  checkHelper (checkPrevious . asgnVars)
  & runUniqueUpdateCheck
  & evalState HS.empty

  where
    asgnVars :: Show a => Stmt a -> S1
    asgnVars Block{..}          = foldMap asgnVars blockStmts
    asgnVars IfStmt{..}         = asgnVars ifStmtThen <> asgnVars ifStmtElse
    asgnVars Assignment{..}     = HS.singleton (varName assignmentLhs, varModuleName assignmentLhs)
    asgnVars ModuleInstance{..} = mempty
    asgnVars PhiNode{..}        = error "phinode encountered in sanity check"
    asgnVars Skip{..}           = mempty

runUniqueUpdateCheck :: FD r => Sem (UniqueUpdateCheck ': r) a -> Sem (State S1 ': r) a
runUniqueUpdateCheck = reinterpret $ \case
  CheckPrevious assignments -> do
    oldAssignments <- get @S1
    modify (HS.union assignments)
    newAssignments <- get @S1
    when (HS.size oldAssignments + HS.size assignments /= HS.size newAssignments) $
      throw "FOO"

newtype SanityCheckError = SanityCheckError {eMsg :: String}

instance E.Exception SanityCheckError

instance Show SanityCheckError where
  show SanityCheckError{..} = eMsg
