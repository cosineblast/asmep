
{-# LANGUAGE GHC2021 #-}

module Asm.Compile
  (compile,
   CompilationError(..)) where

import qualified Asm.Ast as Ast
import Asm.Ast ((<!>))

import Control.Monad.State.Strict (StateT, get, put, runStateT)
import Control.Monad.Except

import Control.Monad (when, foldM, guard, forM)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Word (Word8)

import Data.Maybe (fromJust)

import Data.List (find, sortBy, groupBy)

import Data.Function (on, (&))

type Constant = Int
type Address = Int
type Name = String

type Block = Seq Word8

data Chunk = Chunk {
  chunkAddress :: Address,
  chunkBlock :: Block,
  chunkPos :: Ast.SourcePos }
  deriving (Show)

data LabelDependency = LabelDependency Name Ast.SourcePos
  deriving Show

data Context = Context { ctxVariables :: Map Name Constant,
                         ctxCurrentBlockAddress :: Address,
                         ctxCurrentBlock :: Block,
                         ctxNextAddress :: Address,
                         ctxChunks :: [Chunk],
                         ctxDependencies :: Map Address LabelDependency
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
        ctxChunks = [],
        ctxDependencies = Map.empty
        }
      in runCompile context $ run source

run :: Ast.Source -> Compile (Seq Word8)
run source = do
  mapM_ compileOperation source
  pushChunk (Ast.startPos 0 0) 0
  ctx <- getCtx
  result <- tryFuseChunks $ ctxChunks ctx
  cleanDependencies result

pushChunk :: Ast.SourcePos -> Address ->  Compile ()
pushChunk pos address = do
  ctx <- getCtx

  let (Context { ctxChunks = chunks,
                 ctxCurrentBlock = block,
                 ctxCurrentBlockAddress = currentAddress
               }) = ctx

  putCtx $ ctx { ctxCurrentBlock = Seq.empty,
                 ctxCurrentBlockAddress = address,
                 ctxChunks = Chunk currentAddress block pos : chunks,
                 ctxNextAddress = address
               }
  return ()


compileOperation :: Ast.Operation -> Compile ()
compileOperation (Ast.CommandOp command) = compileCommand command
compileOperation (Ast.InstructionOp instruction) = compileInstruction instruction
compileOperation (Ast.LabelOp label) = compileLabel label

fileByteCount :: Int
fileByteCount = 512

incrementNextAddress :: Ast.SourcePos -> Int -> Compile ()
incrementNextAddress pos n = do
  ctx <- getCtx

  let address = ctxNextAddress ctx

  when (address + n > fileByteCount) $ do
    liftExcept $ throwError $ CompilationError (pos, "This operation goes past the end of RAM")

  putCtx $ ctx {ctxNextAddress = address + n}



compileCommand :: Ast.Command -> Compile ()
compileCommand (Ast.Command (pos, "byte") values) = do
  constants <- mapM resolveByte values
  let constants' = Seq.fromList constants

  ctx <- getCtx
  let block = ctxCurrentBlock ctx

  putCtx $ ctx { ctxCurrentBlock = block Seq.>< constants' }

  incrementNextAddress pos $ length constants'


compileCommand (Ast.Command (pos, "at")[(Ast.Identifier (pos', precision)), value]) = do
  address <- resolveValue' value
  address' <- case precision of
        "word" -> ensureByte pos' address >> (return $ address * 2)
        "byte" -> return address
        _ -> error "Application Logic Error: unknown at precision"

  pushChunk pos address'
  return ()

compileCommand (Ast.Command (_, "define") [(Ast.Identifier (pos, name)), value]) = do
  context <- getCtx
  value' <- resolveValue' value

  let vars = ctxVariables context

  when (name `Map.member` vars) $ do
    liftExcept $ throwError $ CompilationError $ (pos, "The variable '" ++ name ++ "' already exists.")

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
  let byteAddress = ctxNextAddress context
  let wordAddress = byteAddress `div` 2

  when (name `Map.member` vars) $ do
    liftExcept $ throwError $ CompilationError $ (pos, "The name " ++ name ++ " is already being utilized.")

  let vars' = Map.insert name wordAddress vars

  putCtx $ context { ctxVariables = vars' }
  return ()

renderInstruction :: Ast.Instruction -> Compile (Word8, Word8)
renderInstruction (Ast.Instruction (pos, name) argument) = do

  let opcode = fromJust $ Map.lookup name opcodeTable
  target <- case argument of
    (Just value) -> resolveByteOrAddDependency value
    Nothing -> return 0

  ensureAligned pos

  return (fromIntegral opcode, target)

ensureAligned :: Ast.SourcePos -> Compile ()
ensureAligned pos = do
  Context { ctxNextAddress = addr } <- getCtx
  when (addr `mod` 2 /= 0) $ do
    liftExcept $ throwError $ misalignedInstruction pos addr

misalignedInstruction :: Ast.SourcePos -> Address -> CompilationError
misalignedInstruction pos addr = CompilationError $
  (pos, "The current address (" ++  show addr ++ ") is misaligned (odd).\nCan't generate instruction.")


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


-- Resolves the given value, or adds a depdendency for it,
-- returning placeHolderConstant
resolveByteOrAddDependency :: Ast.Value -> Compile Word8
resolveByteOrAddDependency (Ast.Identifier (pos, name)) = do
  ctx <- getCtx
  let Context { ctxNextAddress = next,
                ctxVariables = vars,
                ctxDependencies = deps } = ctx

  case Map.lookup name vars of
    Nothing -> do
      putCtx $ ctx { ctxDependencies = Map.insert (next + 1) (LabelDependency name pos)  deps }
      return placeHolderConstant
    (Just x) -> ensureByte pos x

resolveByteOrAddDependency (Ast.Constant (pos, x)) = ensureByte pos x

ensureByte :: Ast.SourcePos -> Constant -> Compile Word8
ensureByte pos x
  | x > 255 = liftExcept $ throwError $ byteLiteralOverflow (pos, x)
  | otherwise = return (fromIntegral x)

placeHolderConstant :: Word8
placeHolderConstant = 128

resolveByte :: Ast.Value -> Compile Word8
resolveByte (Ast.Identifier name@(pos, _)) = do
  v <- resolveVariable name
  ensureByte pos v

resolveByte (Ast.Constant (pos, x)) = ensureByte pos x

byteLiteralOverflow :: Ast.Sourced Constant -> CompilationError
byteLiteralOverflow (pos, value) = CompilationError $ (pos, "The literal " ++ show value ++ " does not fit in a byte.")



resolveValue' :: Ast.Value -> Compile Constant
resolveValue' (Ast.Identifier name) = resolveVariable name
resolveValue' (Ast.Constant (_, x)) = return x

resolveVariable :: Ast.Sourced Name -> Compile Constant
resolveVariable (pos, name) = do
  vars <- ctxVariables <$> getCtx
  case Map.lookup name vars of
    Nothing -> liftExcept $ throwError $ CompilationError $ (pos, "The variable '" ++ name ++ "' does not exist.")
    (Just x) -> return x

tryFuseChunks :: [Chunk] -> Compile (Seq Word8)
tryFuseChunks chunks = do
  let sorted = sortBy (compare `on` chunkAddress) chunks

  filtered <- removeDuplicateAddressChunks sorted

  let problematic ((Chunk x b1 _), (Chunk y b2 _)) =
        length b1 > 0 && length b2 > 0 && x + length b1 > y

  let firstProblematic = guard (not (null chunks)) >>
        find problematic (zip filtered (tail filtered))

  case firstProblematic of
    Just (b1, b2) -> liftExcept $ throwError $ overlappingChunks b1 b2
    Nothing -> return $ fuseChunks filtered

overlappingChunks :: Chunk -> Chunk -> CompilationError
overlappingChunks x y =
  CompilationError (chunkPos x <!> chunkPos y,
                    "There are overlapping chunks at address " ++ (show . chunkAddress) x
                    ++ " and " ++
                    (show . chunkAddress) y)

removeDuplicateAddressChunks :: [Chunk] -> Compile [Chunk]
removeDuplicateAddressChunks chunks =
  let groups = chunks &
        filter ((> 0) . length . chunkBlock) &
        groupBy ((==) `on` chunkAddress)

  in forM groups $ \group ->
    case group of
      [] -> error "Application Logic Error: Illegal State"
      [x] -> return x
      (x:y:_) -> liftExcept $ throwError $ overlappingChunks x y


  


  



fuseChunks :: [Chunk] -> Seq Word8
fuseChunks sorted =
  let (lastAddr, result) = foldl step (0, Seq.empty) sorted
      in result Seq.>< Seq.fromList (replicate (fileByteCount - lastAddr) 0)



step :: (Int, Seq Word8) -> Chunk -> (Int, Seq Word8)
step (currentAddr, result) (Chunk addr block _) =
  let currentAddr' = currentAddr
      nextAddr = addr + length block
      nextResult = result Seq.>< Seq.fromList (replicate (addr - currentAddr') 0) Seq.>< block
      in (nextAddr, nextResult)

cleanDependencies :: Seq Word8 -> Compile (Seq Word8)
cleanDependencies result = do
  Context { ctxDependencies = deps,
            ctxVariables = vars } <- getCtx

  let go arr (addr, (LabelDependency name pos))
        = case Map.lookup name vars of
            Nothing -> liftExcept $ throwError $ CompilationError $ (pos, "Unknown variable or label: '" ++ name ++  "'.")
            (Just value) -> do
              value' <- ensureByte pos value
              return (Seq.update addr value' arr)

  foldM go result (Map.toList deps)
