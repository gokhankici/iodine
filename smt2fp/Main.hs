module Main where

import Control.Arrow
import Control.Exception  (catch)
import Control.Lens
import Control.Monad
import System.IO
import System.Environment (getArgs)
import System.Exit
import System.Console.ANSI
import qualified Data.HashMap.Strict      as M

import SMT2FP.SMT.Parser
import SMT2FP.Fixpoint.Types
import SMT2FP.Fixpoint.Transform

type PRType = FilePath -> FilePath -> IO ()

qualifiers :: [String]
qualifiers = [ "qualif Eq(x:int, y:int) : (x = y)"
             ]

pipeline f = parse f >>> smt2fp

printResults          :: PRType
printResults fin fout = do
  s <- readFile fin
  let st = pipeline fin s

  withFile fout WriteMode $ \h -> do
    let prStr  = hPutStrLn h
        pr   x = prStr (show x)
        prLn x = pr x >> prStr ""
    forM_ qualifiers prStr
    prStr ""

    forM_ (st^.binds^.to M.elems) pr
    prStr ""

    forM_ (st^.constraints)   prLn
    forM_ (st^.wfConstraints) prLn
  return ()

runMain              :: PRType -> IO ()
runMain printResults = (getFiles >>= uncurry printResults) `catch` peHandle 

main :: IO ()
main = runMain printResults

peHandle :: CmdParseError -> IO ()
peHandle e = do msg <- renderError e 
                hSetSGR stderr [ SetColor Foreground Vivid Red
                               , SetConsoleIntensity BoldIntensity
                               ]
                hPutStrLn stderr msg
                hSetSGR stderr []
                exitFailure

passHandle :: PassError -> IO ()
passHandle (PassError msg) = do
  hSetSGR stderr [ SetColor Foreground Vivid Red
                 , SetConsoleIntensity BoldIntensity
                 ]
  hPutStrLn stderr msg
  hSetSGR stderr []
  exitFailure

getFiles :: IO (FilePath, FilePath)
getFiles = do
  args <- getArgs
  case args of
    [f1,f2] -> return (f1,f2)
    _       -> error "Please run with two files (input & output)"
