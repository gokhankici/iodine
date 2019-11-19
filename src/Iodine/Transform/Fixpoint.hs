module Iodine.Transform.Fixpoint
    ( printSolution
    )
where

import qualified Language.Fixpoint.Types       as FT
import qualified Data.HashMap.Strict           as HM
import           Control.Monad

printSolution :: FT.FixSolution -> IO ()
printSolution = void . HM.traverseWithKey go where go k v = print k >> putStrLn (FT.showpp v)
