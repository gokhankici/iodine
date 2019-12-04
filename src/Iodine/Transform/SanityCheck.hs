{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Iodine.Transform.SanityCheck (sanityCheck) where

import Iodine.Language.IR
import Iodine.Language.IRParser (ParsedIR)
import Iodine.Language.Annotation
import Iodine.Types

import           Control.Lens
import           Control.Monad
import           Data.Foldable
import qualified Data.HashSet   as HS
import qualified Data.Sequence  as SQ
import           Polysemy
import qualified Polysemy.Error as PE
import           Polysemy.Reader
import           Polysemy.State
import           Text.Printf

type K1 = (Id, Id)
type S1 = HS.HashSet K1

data UniqueUpdateCheck m a where
  CheckPrevious :: S1 -> UniqueUpdateCheck m ()

makeSem ''UniqueUpdateCheck

type SC r = Members '[ PE.Error IodineException   -- sanity error
                     , Reader ParsedIR            -- parsed IR
                     , Reader AnnotationFile      -- parsed annotation file
                     ] r

type S  = Stmt ()
type M  = Module ()
type MA = Maybe (AlwaysBlock ())
type FD r = ( SC r
            , Members '[ Reader (Module ())
                       , Reader MA
                       ] r
            )

-- | Type alias for the Sanity Check Monad
type SCM r a = Sem (Reader MA ': Reader M ': r) a

-- -----------------------------------------------------------------------------
sanityCheck :: SC r => Sem r ()
-- -----------------------------------------------------------------------------
sanityCheck =
  sequence_ [ checkAssignmentsAreToLocalVariables
            , checkSameAssignmentType
            , checkUniqueUpdateLocationOfVariables
            , checkVariables
            ]

checkHelper :: SC r
            => (S -> SCM r ()) -- | checks the statement
            -> Sem r ()
checkHelper goS = ask @ParsedIR >>= traverse_ (checkModule goS)

checkModule :: (S -> SCM r ())
            -> Module ()
            -> Sem r ()
checkModule goS m@Module{..} =
  ( do (traverse_ goS gateStmts) & runReader @MA Nothing
       let goAB ab@AlwaysBlock{..} = goS abStmt & runReader (Just ab)
       traverse_ goAB alwaysBlocks
  ) & runReader m

getModuleName :: FD r => Sem r Id
getModuleName = asks moduleName

-- -----------------------------------------------------------------------------
checkAssignmentsAreToLocalVariables :: SC r => Sem r ()
-- Check that all assignments in a single module are to the variables
-- of that module
-- -----------------------------------------------------------------------------
checkAssignmentsAreToLocalVariables =
  checkHelper $ handleAssignment $
  \assignmentType assignmentLhs assignmentRhs -> do
    moduleName <- getModuleName
    when (varModuleName assignmentLhs /= moduleName) $
      let stmt = Assignment{ stmtData = (), .. }
      in throw $ printf "%s :: lhs is not from the module %s" (show stmt) moduleName

handleAssignment :: (AssignmentType -> Expr a -> Expr a -> Sem r ())
                 -> Stmt a
                 -> Sem r ()
handleAssignment handler = go
  where
    gos = traverse_ go

    go Block{..}          = gos blockStmts
    go IfStmt{..}         = gos (ifStmtThen SQ.<| ifStmtElse SQ.<| SQ.empty)
    go Assignment{..}     = handler assignmentType assignmentLhs assignmentRhs
    go Skip{..}           = pure ()

-- -----------------------------------------------------------------------------
checkSameAssignmentType :: SC r => Sem r ()
-- check that always blocks with * events only have blocking assignments, and
-- the ones with @posedge or @negedge has non-blocking assignments
-- -----------------------------------------------------------------------------
checkSameAssignmentType =
  checkHelper $ handleAssignment $
  \assignmentType assignmentLhs assignmentRhs -> do
    let stmt = Assignment{stmtData = (), ..}
    mAB <- ask @MA
    case mAB of
      Nothing ->
        when (assignmentType /= Continuous) $
        throw $ printf "%s :: Assignments outside always blocks should be continous" (show stmt)
      Just ab@AlwaysBlock{..} ->
        let err  = throw $ printf
                   "%s does not match the event in %s" (show stmt) blockStr
            err2 = throw $ printf
                   "continuous assignment should not appear in an always block %s" blockStr
            blockStr = let maxLength = 200
                           str = show ab
                       in if length str > maxLength
                          then (take maxLength str) ++ " ..."
                          else str
        in case (abEvent, assignmentType) of
             (_, Continuous)         -> err2
             (Star, NonBlocking)     -> err
             (PosEdge{..}, Blocking) -> err
             (NegEdge{..}, Blocking) -> err
             _                       -> pure ()

-- -----------------------------------------------------------------------------
checkUniqueUpdateLocationOfVariables :: SC r => Sem r ()
-- -----------------------------------------------------------------------------
checkUniqueUpdateLocationOfVariables =
  checkHelper (checkPrevious . asgnVars)
  & runUniqueUpdateCheck
  & evalState HS.empty

  where
    asgnVars :: Stmt a -> S1
    asgnVars Block{..}          = foldMap asgnVars blockStmts
    asgnVars IfStmt{..}         = asgnVars ifStmtThen <> asgnVars ifStmtElse
    asgnVars Assignment{..}     = HS.singleton (varName assignmentLhs, varModuleName assignmentLhs)
    asgnVars Skip{..}           = mempty

runUniqueUpdateCheck :: SC r => Sem (UniqueUpdateCheck ': r) a -> Sem (State S1 ': r) a
runUniqueUpdateCheck = reinterpret $ \case
  CheckPrevious assignments -> do
    oldAssignments <- get @S1
    modify (HS.union assignments)
    newAssignments <- get @S1
    when (HS.size oldAssignments + HS.size assignments /= HS.size newAssignments) $
      throw $ printf "found multiple assignments to variables from %s" (show $ toList assignments)

-- -----------------------------------------------------------------------------
checkVariables :: SC r => Sem r ()
-- -----------------------------------------------------------------------------
checkVariables = do
  ask @ParsedIR >>= traverse_
    (\Module{..} -> do
        af <- getAnnotations moduleName
        let srcs = af ^. sources
            snks = af ^. sinks
            vars = foldr' HS.insert HS.empty (variableName <$> variables)
            isVar vs = vs `subset` vars

        when (HS.null srcs) $
          throw "No source variable is given!"
        when (HS.null snks) $
          throw "No sink variable is given!"

        -- all annotation variables actually exist
        forM_ [ srcs
              , snks
              , af ^. initialEquals
              , af ^. alwaysEquals
              , af ^. assertEquals
              ] $ \vs ->
          unless (isVar vs) $
          throw $ printf "element(s) in %s is not a valid variable" (show vs)

        clk  <- getClock moduleName
        let isNotClock name = maybe True (name /=) clk

        -- all inputs must be a source
        for_ ports $ \case
          Input v ->
            let name = variableName v
            in when (isNotClock name && not (HS.member name srcs)) $
               throw $ printf "The input port %s is not declared as a taint source!" name
          Output _ -> return ()

        -- sinks have to be registers
        for_ snks $ \snk ->
          when (Register snk `SQ.elemIndexL` variables == Nothing) $
          throw $ printf "Sink %s is not found or not a register" snk

        -- always block events only refer to specified clocks
        for_ alwaysBlocks $ \AlwaysBlock{..} ->
          case abEvent of
            Star -> return ()
            _    ->
              case eventExpr abEvent of
                Variable{..} ->
                  when (isNotClock varName || varModuleName /= moduleName) $
                  throw $ "always block has bad event: " ++ show abEvent
                _ ->
                  throw $ "always block has bad event: " ++ show abEvent

    )
  where
    s1 `subset` s2 = HS.null (HS.difference s1 s2)

throw :: Member (PE.Error IodineException) r => String -> Sem r a
throw = PE.throw . IE SanityCheck
