
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Data.ByteString as B

import Data.Foldable (toList)

import qualified Asm.Ast as Ast
import qualified Asm.Integrity as Integrity
import qualified Asm.Compile as Compile
import System.Console.GetOpt (OptDescr(..), getOpt, ArgDescr(..), ArgOrder(..))
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import Data.List (find)
import Data.Bifunctor (first)
import qualified Text.Parsec as P

import Data.Sequence (Seq)

import Data.Word (Word8)

import Control.Monad.Except

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
  putStrLn "Usage: asmep [options] file"
  putStrLn "Options:"
  putStrLn "  --help, -h  Show this help message"
  putStrLn "  --version, -v  Show version"
  putStrLn "  --output, -o, Name of the output file to compile"

printVersion :: IO ()
printVersion = do
  putStrLn "asmep 0.1.0.0"
  putStrLn "figurantpp - Renan Ribeiro - Universidade de São Paulo"

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

data Issue = ParseIssue P.ParseError
  | IntegrityIssue Integrity.ValidationError
  | CompilationIssue Compile.CompilationError
  deriving (Show)



runCompiler :: String -> String -> IO ()
runCompiler inputName outputName = do
  source <- readFile inputName

  putStrLn $ "Compiling " ++ inputName ++ "..."

  let handle t = ExceptT . return . first t

  result <- runExceptT $ do
    ast <- handle ParseIssue $ Ast.parseWithFilename inputName source
    handle IntegrityIssue $ Integrity.validateAst ast
    output <- handle CompilationIssue $ Compile.compile ast
    lift $ writeOutput outputName output
    return ()

  case result of
    (Left (ParseIssue issue)) -> do
      putStrLn "Parse Error:"
      print issue

    (Left (IntegrityIssue (pos, issue))) -> do
      putStrLn "Integrity Error:"
      putStr "At "
      printPos pos
      putStrLn ":"
      putStrLn issue

    (Left (CompilationIssue (Compile.CompilationError (pos, issue)))) -> do
      putStrLn "Compilation Error:"
      putStr "At "
      printPos pos
      putStrLn ":"
      putStrLn issue

    (Right _) -> do
      putStrLn $ "Finished writing to " ++ outputName

  return ()

printPos :: Ast.SourcePos -> IO ()
printPos (Ast.Paste l r)  = printPos l >> putStr ", " >> printPos r
printPos Ast.NoPos = putStr "Unknown Position"
printPos (Ast.Pos pos) = putStr $ show pos

writeOutput :: String -> Seq Word8 -> IO ()
writeOutput name result = B.writeFile name $ B.pack $ toList result
