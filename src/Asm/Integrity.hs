{-# LANGUAGE GHC2021 #-}

module Asm.Integrity
  (ValidationError,
   validateAst) where

import qualified Asm.Ast as Ast

import Data.Maybe

import Data.List (find)

import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (join)

type ValidationError = Ast.Sourced String

toEither :: Maybe ValidationError -> Either ValidationError ()
toEither (Just err) = Left err
toEither Nothing = Right ()


validateAst :: Ast.Source -> Either ValidationError ()
validateAst = toEither . validateAst'

validateAst' :: Ast.Source -> Maybe ValidationError
validateAst' [] = Nothing
validateAst' ops = join $ find isJust $ map validateOp ops

validateOp :: Ast.Operation -> Maybe ValidationError
validateOp (Ast.InstructionOp instruction) = validateInstruction instruction
validateOp (Ast.CommandOp command) = validateCommand command
validateOp (Ast.LabelOp _) = Nothing

-- TODO: consider making a SourcedMonad
-- TODO: validate sizes

validateInstruction :: Ast.Instruction -> Maybe ValidationError
validateInstruction (Ast.Instruction (pos, "nop") (Just _))
  = Just (pos, "NOP instruction must not have an argument")

validateInstruction (Ast.Instruction (pos, "brk") (Just _))
  = Just (pos, "BRK instruction must not have an argument")

validateInstruction (Ast.Instruction (_, "brk") Nothing) = Nothing
validateInstruction (Ast.Instruction (_, "nop") Nothing) = Nothing
validateInstruction (Ast.Instruction (pos, name) Nothing) =
  Just $ (pos, "Instruction " ++ name  ++ " expected an argument.")
validateInstruction (Ast.Instruction (pos, name) (Just _))
  | (map toLower name) `Set.member` validInstructionNames = Nothing
  | otherwise = Just $ (pos, "Unknown instruction name " ++ name)

validateCommand :: Ast.Command -> Maybe ValidationError

validateCommand (Ast.Command (pos, name) _)
  | name `Set.notMember` validCommandNames = Just $ (pos, "Unknown command name " ++ name)

validateCommand (Ast.Command (_, "define" )[(Ast.Identifier _), _]) = Nothing
validateCommand (Ast.Command (_, "at") [(Ast.Identifier (_, "word")), _]) = Nothing
validateCommand (Ast.Command (_, "at") [(Ast.Identifier (_, "byte")), _]) = Nothing
validateCommand (Ast.Command (_, "byte") (_:_)) = Nothing

validateCommand (Ast.Command (pos, _) _) = Just $ (pos, "Illegal combination of commands and arguments")


validCommandNames :: Set String
validCommandNames = Set.fromList ["define", "at", "byte"]

validInstructionNames :: Set String
validInstructionNames = Set.fromList ["nop", "lda", "sta", "add", "sub", "in", "out", "brk", "jmp", "bpl", "bze", "bmi"]
