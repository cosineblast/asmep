module Asm.Ast
  (
    Name,
    Value(..),
    Command(..),
    Operation(..),
    Instruction(..),
    Label(..),
    Source,
    Sourced,
    SourcePos(..),
    startPos,
    (<!>),
    parse,
  )
where

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos
import qualified Text.Parsec.Char as C
import qualified Text.Parsec (SourcePos)

import Data.List (singleton)

import Control.Monad (void)

import Control.Applicative (Alternative(..))

type ParserState = ()

type Parser = P.Parsec String ParserState

data SourcePos = Pos Text.Parsec.SourcePos | Paste SourcePos SourcePos | NoPos
  deriving Show

type Sourced a = (SourcePos, a)
type Name = Sourced String
type Literal = Sourced Int

data Value = Identifier Name | Constant Literal
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


posFromParsec :: P.SourcePos -> SourcePos
posFromParsec p = Pos p

infixr 6 <!>
(<!>) :: SourcePos -> SourcePos -> SourcePos
(<!>) = Paste

startPos :: Int -> Int -> SourcePos
startPos row col = posFromParsec $ Text.Parsec.Pos.newPos "input" row col

parseWithFilename :: String -> String -> Either P.ParseError Source
parseWithFilename name code = P.runParser source () name code

parse :: String -> Either P.ParseError Source
parse = parseWithFilename "input"

source :: Parser [Operation]
source = do
  P.skipMany C.newline
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
  p <- P.getPosition
  f <- (C.letter <|> P.char '_')
  xs <- many (C.letter <|> P.char '_' <|> P.digit)
  return $ (posFromParsec p, f : xs)

literal  :: Parser Literal
literal = do
  pos <- P.getPosition
  (posFromParsec pos,) <$> (P.try hexConstant <|> decimalConstant)

decimalConstant :: Parser Int
decimalConstant = read <$> P.many1 C.digit

hexConstant :: Parser Int
hexConstant = do
  prefix <- C.string "0x"
  content <- P.many1 C.hexDigit
  return $ read (prefix ++ content)


value :: Parser Value
value = (Identifier <$> identifier) <|> (Constant <$> literal)

forcedWhitespace :: Parser ()
forcedWhitespace = do
  void $ P.char ' '
  return ()
