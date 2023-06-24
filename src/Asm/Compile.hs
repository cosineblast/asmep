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
type Address = Word8
type Name = String

type Block = Seq Word8

data Context = Context { ctxVariables :: Map Name Constant,
                         ctxCurrentBlockAddress :: Address,
                         ctxCurrentBlock :: Block,
                         ctxNextAddress :: Address,
                         ctxBlocks :: [(Address, Block)]
                       }
  deriving (Show)

data CompilationError = CompilationError
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

compileCommand :: Ast.Command -> Compile ()
compileCommand (Ast.Command "byte" values) = do
  constants <- mapM resolveValue values
  let constants' = Seq.fromList constants

  ctx <- getCtx
  let block = ctxCurrentBlock ctx
  let address = ctxNextAddress ctx

  when (fromIntegral address + length constants' > 255) $ do
    liftExcept $ throwE CompilationError

  putCtx $ ctx { ctxCurrentBlock = block Seq.>< constants',
                 ctxNextAddress = address + (fromIntegral $ length constants')
               }


compileCommand (Ast.Command "at" [value]) = do
  address <- resolveValue value
  pushBlock address
  return ()

compileCommand (Ast.Command "define" [(Ast.Identifier name), value]) = do
  context <- getCtx
  value' <- resolveValue value

  let vars = ctxVariables context

  when (name `Map.member` vars) $ do
    liftExcept $ throwE CompilationError

  let vars' = Map.insert name value' vars

  putCtx $ context { ctxVariables = vars' }

  return ()

compileCommand _ = undefined

compileInstruction :: Ast.Instruction -> Compile ()
compileInstruction instruction = do
  (opcode, target) <- renderInstruction instruction
  ctx <- getCtx
  let block = ctxCurrentBlock ctx
  let address = ctxNextAddress ctx
  putCtx $ ctx { ctxCurrentBlock = block Seq.>< (Seq.fromList [opcode, target]),
                 ctxNextAddress = address + 2 }

compileLabel :: Ast.Label -> Compile ()
compileLabel (Ast.Label name) = do
  context <- getCtx

  let vars = ctxVariables context
  let address = ctxNextAddress context

  when (name `Map.member` vars) $ do
    liftExcept $ throwE CompilationError

  let vars' = Map.insert name address vars

  putCtx $ context { ctxVariables = vars' }
  return ()

renderInstruction :: Ast.Instruction -> Compile (Constant, Constant)
renderInstruction (Ast.Instruction name argument) = do
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
resolveValue (Ast.Constant x) = return (fromIntegral x)

resolveVariable :: Name -> Compile Constant
resolveVariable name = do
  vars <- ctxVariables <$> getCtx
  case Map.lookup name vars of
    Nothing -> liftExcept $ throwE CompilationError
    (Just x) -> return x

tryFuseBlocks :: [(Address, Block)] -> Compile (Seq Word8)
tryFuseBlocks blocks =
  let sorted = sortBy (compare `on` fst) blocks
      problematic ((x,b1), (y,_)) = fromIntegral x + length b1 > fromIntegral y
      hasIssues = not (null blocks) && any problematic (zip sorted (tail sorted))
      in if hasIssues
         then liftExcept $ throwE CompilationError
         else return $ fuseBlocks sorted

fuseBlocks :: [(Address, Block)] -> Seq Word8
fuseBlocks sorted =
  let (lastAddr, result) = foldl step (0, Seq.empty) sorted
      in result Seq.>< Seq.fromList (replicate (255 - lastAddr) 0)

step :: (Int, Seq Word8) -> (Address, Block) -> (Int, Seq Word8)
step (currentAddr, result) (addr, block) =
  let addr' = fromIntegral addr
      currentAddr' = currentAddr
      nextAddr = addr' + length block
      nextResult = result Seq.>< Seq.fromList (replicate (addr' - currentAddr') 0) Seq.>< block
      in (nextAddr, nextResult)
