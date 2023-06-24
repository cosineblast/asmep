module Main (main) where


import Asm.Ast (parse)
import Asm.Compile (compile)

main :: IO ()
main = do
  content <- readFile "sample.txt"

  case parse content of
    Left uh -> putStrLn "Parsing error" >> print uh
    Right ast -> do
      print ast
      case compile ast of
        Left issue -> putStrLn "Compilation error" >> print issue
        Right result -> print result

