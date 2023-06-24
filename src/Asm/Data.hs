
module Asm.Data
  (ValidationError,
   validateAst) where

import qualified Asm.Ast as Ast

import Data.Maybe

import Data.List (find)

import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

type ValidationError = String


validateAst :: Ast.Source -> Maybe ValidationError
validateAst [] = Nothing
validateAst ops = fromJust $ find isJust $ map validateOp ops

validateOp :: Ast.Operation -> Maybe ValidationError
validateOp (Ast.InstructionOp instruction) = validateInstruction instruction
validateOp (Ast.CommandOp command) = validateCommand command
validateOp (Ast.LabelOp _) = Nothing

-- TODO: keep record of SourcePos
-- TODO: validate sizes

validateInstruction :: Ast.Instruction -> Maybe ValidationError
validateInstruction (Ast.Instruction "nop" (Just _))
  = Just "NOP instruction must not have an argument"
validateInstruction (Ast.Instruction "nop" Nothing) = Nothing
validateInstruction (Ast.Instruction name Nothing) =
  Just $ "Instruction " ++ name  ++ " expected an argument."
validateInstruction (Ast.Instruction name (Just _))
  | (map toLower name) `Set.member` validInstructionNames = Nothing
  | otherwise = Just $ "Unknown instruction name " ++ name

validateCommand :: Ast.Command -> Maybe ValidationError

validateCommand (Ast.Command name _)
  | name `Set.notMember` validCommandNames = Just $ "Unknown command name " ++ name

validateCommand (Ast.Command "define" [(Ast.Identifier _), _]) = Nothing
validateCommand (Ast.Command "at" [_]) = Nothing
validateCommand (Ast.Command "byte" [_]) = Nothing

validateCommand command = Just $ "Illegal combination of commands and arguments: " ++ show command


validCommandNames :: Set String
validCommandNames = Set.fromList ["define", "at", "byte"]

validInstructionNames :: Set String
validInstructionNames = Set.fromList ["nop", "lda", "sta", "add", "sub", "in", "out", "brk", "jmp", "bpl", "bze", "bmi"]
