{-# LANGUAGE RecordWildCards #-}

module Iodine.Transform.SanityCheck
  ( sanityCheck
  )
where

-- import Iodine.Language.IR
import Iodine.Language.IRParser (ParsedIR)
import Iodine.Language.Annotation

import Control.Exception
import Control.Monad

type M = Either SanityCheckError
type I = (ParsedIR, AnnotationFile ())
type Checker = I -> M I

-- -----------------------------------------------------------------------------
sanityCheck :: I -> I
-- -----------------------------------------------------------------------------
sanityCheck input =
  case allChecks input of
    Left err -> throw err
    Right result -> result

allChecks :: Checker
allChecks = checkSameAssignmentType >=>
            checkAlwaysBlockAssignments >=>
            checkUniqueUpdateLocationOfVariables

-- -----------------------------------------------------------------------------
checkSameAssignmentType :: Checker
-- -----------------------------------------------------------------------------
checkSameAssignmentType = undefined

-- -----------------------------------------------------------------------------
checkAlwaysBlockAssignments :: Checker
-- -----------------------------------------------------------------------------
checkAlwaysBlockAssignments = undefined

-- -----------------------------------------------------------------------------
checkUniqueUpdateLocationOfVariables :: Checker
-- -----------------------------------------------------------------------------
checkUniqueUpdateLocationOfVariables  = undefined

newtype SanityCheckError = SanityCheckError {eMsg :: String}

instance Exception SanityCheckError

instance Show SanityCheckError where
  show SanityCheckError{..} = eMsg
