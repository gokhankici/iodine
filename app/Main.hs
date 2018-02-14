module Main where

import Control.Exception  (catch)
import Control.Monad
import Control.Arrow ((>>>))
import System.Environment (getArgs)
import System.Exit
import System.IO (stderr, hPutStrLn)

import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.Transform.InitialPass

main :: IO ()
main =  vcgen `catch` peHandle `catch` passHandle

peHandle :: IRParseError -> IO ()
peHandle e = renderError e >>= hPutStrLn stderr >> exitFailure

passHandle :: PassError -> IO ()
passHandle (PassError msg) = hPutStrLn stderr msg >> exitFailure

pipeline f = parse f >>> initialPass

vcgen :: IO ()
vcgen = do
  f <- getSrcFile
  s <- readFile f
  let out = pipeline f s
  putStrLn (pprint out)
  exitSuccess

getSrcFile :: IO FilePath
getSrcFile = do
  args <- getArgs
  case args of
    [f] -> return f
    _   -> error "Please run with a single file as input"
