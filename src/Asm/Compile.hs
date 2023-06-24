module Asm.Compile
  (compile,
   CompilationError) where

import qualified Asm.Ast as Ast
import Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
import Control.Monad.Trans.Except (Except, throwE, runExcept)
import Control.Monad.Trans.Class (MonadTrans(..))

import Control.Monad (when)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Word (Word8)

import Data.Maybe (fromJust)

import Data.List (sortBy)

import Data.Function (on)

type Constant = Word8
type Address = Int
type Name = String

type Block = Seq Word8

data Context = Context { ctxVariables :: Map Name Constant,
                         ctxCurrentBlockAddress :: Address,
                         ctxCurrentBlock :: Block,
                         ctxNextAddress :: Address,
                         ctxBlocks :: [(Address, Block)]
                       }
  deriving (Show)

data CompilationError = CompilationError (Ast.Sourced String)
  deriving (Show)

newtype Compile a = Compile (StateT Context (Except CompilationError) a)
  deriving (Functor, Applicative, Monad)

runCompile :: Context -> Compile a -> Either CompilationError a
runCompile ctx (Compile state) = runExcept $ fst <$> runStateT state ctx

liftState :: (StateT Context (Except CompilationError) a) -> Compile a
liftState = Compile

getCtx :: Compile Context
getCtx = liftState get

putCtx :: Context -> Compile ()
putCtx ctx = liftState $ put ctx

liftExcept :: Except CompilationError a -> Compile a
liftExcept = liftState . lift

compile :: Ast.Source -> Either CompilationError (Seq Word8)
compile source =
  let context = Context {
        ctxVariables = Map.empty,
        ctxCurrentBlockAddress = 0,
        ctxCurrentBlock = Seq.empty,
        ctxNextAddress = 0,
        ctxBlocks = []
        }
      in runCompile context $ run source

run :: Ast.Source -> Compile (Seq Word8)
run source = do
  mapM_ compileOperation source
  pushBlock 0
  ctx <- getCtx
  tryFuseBlocks $ ctxBlocks ctx

pushBlock :: Address ->  Compile ()
pushBlock address = do
  ctx <- getCtx

  let (Context { ctxBlocks = blocks,
                 ctxCurrentBlock = block,
                 ctxCurrentBlockAddress = currentAddress
               }) = ctx

  putCtx $ ctx { ctxCurrentBlock = Seq.empty,
                              ctxCurrentBlockAddress = address,
                              ctxBlocks = (currentAddress, block) : blocks,
                              ctxNextAddress = address
                            }
  return ()


compileOperation :: Ast.Operation -> Compile ()
compileOperation (Ast.CommandOp command) = compileCommand command
compileOperation (Ast.InstructionOp instruction) = compileInstruction instruction
compileOperation (Ast.LabelOp label) = compileLabel label

incrementNextAddress :: Ast.SourcePos -> Int -> Compile ()
incrementNextAddress pos n = do
  ctx <- getCtx

  let address = ctxNextAddress ctx

  when (address + n > 256) $ do
    liftExcept $ throwE $ CompilationError (pos, "This operation goes past the end of RAM")

  putCtx $ ctx {ctxNextAddress = address + n}



compileCommand :: Ast.Command -> Compile ()
compileCommand (Ast.Command (pos, "byte") values) = do
  constants <- mapM resolveValue values
  let constants' = Seq.fromList constants

  ctx <- getCtx
  let block = ctxCurrentBlock ctx

  putCtx $ ctx { ctxCurrentBlock = block Seq.>< constants' }

  incrementNextAddress pos $ length constants'


compileCommand (Ast.Command (_, "at")[value]) = do
  address <- resolveValue value
  pushBlock $ fromIntegral address
  return ()

compileCommand (Ast.Command (_, "define") [(Ast.Identifier (pos, name)), value]) = do
  context <- getCtx
  value' <- resolveValue value

  let vars = ctxVariables context

  when (name `Map.member` vars) $ do
    liftExcept $ throwE $ CompilationError $ (pos, "The variable '" ++ name ++ "' already exists.")

  let vars' = Map.insert name value' vars

  putCtx $ context { ctxVariables = vars' }

  return ()

compileCommand _ = undefined

compileInstruction :: Ast.Instruction -> Compile ()
compileInstruction instruction = do
  let (Ast.Instruction (pos, _) _) = instruction
  (opcode, target) <- renderInstruction instruction
  ctx <- getCtx
  let block = ctxCurrentBlock ctx

  putCtx $ ctx { ctxCurrentBlock = block Seq.>< (Seq.fromList [opcode, target]) }

  incrementNextAddress pos 2

compileLabel :: Ast.Label -> Compile ()
compileLabel (Ast.Label (pos, name)) = do
  context <- getCtx

  let vars = ctxVariables context
  let address = ctxNextAddress context

  when (name `Map.member` vars) $ do
    liftExcept $ throwE $ CompilationError $ (pos, "The name " ++ name ++ " is already being utilized.")

  let vars' = Map.insert name (fromIntegral address) vars

  putCtx $ context { ctxVariables = vars' }
  return ()

renderInstruction :: Ast.Instruction -> Compile (Constant, Constant)
renderInstruction (Ast.Instruction (_, name) argument) = do
  let opcode = fromJust $ Map.lookup name opcodeTable
  target <- case argument of
    (Just value) -> resolveValue value
    Nothing -> return 0

  return (opcode, target)

opcodeTable :: Map String Constant
opcodeTable = Map.fromList [("nop", 0x00),
                            ("lda", 0x01),
                            ("sta", 0x02),
                            ("add", 0x03),
                            ("sub", 0x04),
                            ("in" , 0x07),
                            ("out", 0x08),
                            ("brk", 0x09),
                            ("jmp", 0x0A),
                            ("bpl", 0x0B),
                            ("bze", 0x0D),
                            ("bmi", 0x0F)]

resolveValue :: Ast.Value -> Compile Constant
resolveValue (Ast.Identifier name) = resolveVariable name
resolveValue (Ast.Constant (_, x)) = return (fromIntegral x)

resolveVariable :: Ast.Sourced Name -> Compile Constant
resolveVariable (pos, name) = do
  vars <- ctxVariables <$> getCtx
  case Map.lookup name vars of
    Nothing -> liftExcept $ throwE $ CompilationError $ (pos, "The variable '" ++ name ++ "' does not exist.")
    (Just x) -> return x

tryFuseBlocks :: [(Address, Block)] -> Compile (Seq Word8)
tryFuseBlocks blocks =
  let sorted = sortBy (compare `on` fst) blocks
      problematic ((x,b1), (y,b2)) = fromIntegral x + length b1 > fromIntegral y && length b1 > 0 && length b2 > 0
      hasIssues = not (null blocks) && any problematic (zip sorted (tail sorted))
      in if hasIssues
         then liftExcept $ throwE $ CompilationError $ (Ast.NoPos, "There are overlapping .at blocks")
         else return $ fuseBlocks sorted

-- todo: add SourcePos to block

fuseBlocks :: [(Address, Block)] -> Seq Word8
fuseBlocks sorted =
  let (lastAddr, result) = foldl step (0, Seq.empty) sorted
      in result Seq.>< Seq.fromList (replicate (256 - lastAddr) 0)

step :: (Int, Seq Word8) -> (Address, Block) -> (Int, Seq Word8)
step (currentAddr, result) (addr, block) =
  let currentAddr' = currentAddr
      nextAddr = addr + length block
      nextResult = result Seq.>< Seq.fromList (replicate (addr - currentAddr') 0) Seq.>< block
      in (nextAddr, nextResult)
