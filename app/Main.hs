
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad


import qualified Asm.Ast as Ast
import qualified Asm.Compile as Compile
import System.Console.GetOpt
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import Data.List (find)
import Data.Bifunctor (first)
import qualified Text.Parsec as P

type FileName = String

data Flag = HelpFlag | VersionFlag | OutputFlag FileName
  deriving (Show, Eq)

flags :: [OptDescr Flag]
flags = [
  Option ['h'] ["help"] (NoArg HelpFlag) "Show this help message",
  Option ['v'] ["version"] (NoArg VersionFlag) "Display compiler version information",
  Option ['o'] ["output"] (ReqArg OutputFlag "(output file)") "The output file name"]

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: epasm [options] file"
  putStrLn "Options:"
  putStrLn "  --help, -h  Show this help message"
  putStrLn "  --version, -v  Show version"
  putStrLn "  --output, -o, Name of the output file to compile"

printVersion :: IO ()
printVersion = do
  putStrLn "epasm 0.0.1"
  putStrLn "figurantpp - Renan Ribeiro Marcelino - Universidade de São Paulo"

printAdvice :: IO ()
printAdvice = putStrLn "Use --help for help"

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute flags args of
    (_, _, errors@(_:_)) -> do
      putStrLn "Invalid Usage:"
      printAdvice
      mapM_ putStrLn errors

    (matches, [], _) -> do
      when (HelpFlag `elem` matches) $ do
        printHelp
        exitWith ExitSuccess

      when (VersionFlag `elem` matches) $ do
        printVersion
        exitWith ExitSuccess

      putStrLn "Missing input file."
      printAdvice

    (matches, [input], _) -> do
      case find (\case (OutputFlag _) -> True; _ -> False) matches of
        Nothing -> putStrLn "Missing output file." >> printAdvice
        (Just (OutputFlag output)) -> runCompiler input output
        _ -> undefined

    (_, (_:_:_), _ ) -> do
      putStrLn "Multiple input files"
      printAdvice

data Issue = ParseIssue P.ParseError | ComplilationIssue Compile.CompilationError
  deriving (Show)


runCompiler :: String -> String -> IO ()
runCompiler input output = do
  source <- readFile input

  putStrLn $ "Compiling " ++ input ++ " to " ++ output

  case first ParseIssue $ Ast.parseWithFilename input source of
    (Left issue) -> do
      putStrLn "Parse Error:"
      print issue
    (Right _) -> do
      putStrLn "Source Code OK!"

  return ()
