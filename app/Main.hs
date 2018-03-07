module Main where

import Control.Exception  (catch)
import Control.Monad
import System.IO
import System.Environment (getArgs)
import System.Exit
import System.Console.ANSI

import SMT2FP.Parser
import SMT2FP.Types

peHandle :: CmdParseError -> IO ()
peHandle e = do msg <- renderError e 
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

type PRType = FilePath -> FilePath -> IO ()

printResults          :: PRType
printResults fin fout = do
  s <- readFile fin
  let cmds = parse fin s

  withFile fout WriteMode $ \h -> do
    let pr     = hPutStrLn h
        prLn c = pr (show c) >> pr ""
    forM_ cmds prLn
  return ()

runMain              :: PRType -> IO ()
runMain printResults = (getFiles >>= uncurry printResults) `catch` peHandle 

main :: IO ()
main = runMain printResults
