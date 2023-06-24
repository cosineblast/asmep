module Asm.Ast
  (
    Name,
    Value(..),
    Command(..),
    Operation(..),
    Instruction(..),
    Value(..),
    Label(..),
    Source,
    parse
  )
where

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.Parsec (SourcePos)

import Data.List (singleton)

import Control.Monad (void)

import Control.Applicative (Alternative(..))

type ParserState = ()

type Parser = P.Parsec String ParserState

type Name = String

data Value = Identifier Name | Constant Int
  deriving Show

data Command = Command Name [Value]
  deriving Show

data Operation = CommandOp Command | InstructionOp Instruction | LabelOp Label
  deriving Show

data Instruction = Instruction Name (Maybe Value)
  deriving Show

data Label = Label Name
  deriving Show

type Source = [Operation]

parseWithFilename :: String -> String -> Either P.ParseError Source
parseWithFilename name code = P.runParser source () name code

parse :: String -> Either P.ParseError Source
parse = parseWithFilename "input"

source :: Parser [Operation]
source = do
  result <- operations
  P.skipMany C.newline
  return result


operations :: Parser [Operation]
operations = P.try (do x <- operation; P.skipMany C.newline; (x:)  <$> operations) <|> (singleton <$> operation)

operation :: Parser Operation
operation = (CommandOp <$> command) <|>
            (P.try (LabelOp <$> label)) <|>
            (InstructionOp <$> instruction)

command :: Parser Command
command = do
  name <- commandName
  forcedWhitespace
  args <- value `P.sepBy` forcedWhitespace
  return $ Command name args

commandName :: Parser Name
commandName = P.char '.' >> identifier

instruction :: Parser Instruction
instruction = do
  name <- identifier
  let withArgument = do
        forcedWhitespace
        v <- value
        return $ Instruction name (Just v)
  let withoutArgument = return $ Instruction name Nothing
  withArgument <|> withoutArgument

label :: Parser Label
label = do
  name <- identifier
  void $ P.char ':'
  return $ Label name

identifier :: Parser Name
identifier = do
  f <- (C.letter <|> P.char '_')
  xs <- many (C.letter <|> P.char '_' <|> P.digit)
  return $ f : xs

constant  :: Parser Int
constant = P.try hexConstant <|> decimalConstant

decimalConstant :: Parser Int
decimalConstant = read <$> P.many1 C.digit

hexConstant :: Parser Int
hexConstant = do
  prefix <- C.string "0x"
  content <- P.many1 C.hexDigit
  return $ read (prefix ++ content)


value :: Parser Value
value = (Identifier <$> identifier) <|> (Constant <$> constant)

forcedWhitespace :: Parser ()
forcedWhitespace = do
  void $ P.char ' '
  return ()
