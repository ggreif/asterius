{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Asterius.Types
  ( BinaryenIndex,
    AsteriusCodeGenError (..),
    AsteriusStatic (..),
    AsteriusStaticsType (..),
    AsteriusStatics (..),
    AsteriusModule (..),
    ModuleMetadata(..),
    AsteriusRepModule(..),
    AsteriusCachedModule(..),
    toCachedModule,
    EntitySymbol,
    entityName,
    mkEntitySymbol,
    UnresolvedLocalReg (..),
    UnresolvedGlobalReg (..),
    ValueType (..),
    FunctionType (..),
    UnaryOp (..),
    BinaryOp (..),
    Expression (..),
    Function (..),
    FunctionImport (..),
    TableImport (..),
    MemoryImport (..),
    FunctionExport (..),
    FunctionTable (..),
    DataSegment (..),
    Module (..),
    RelooperAddBlock (..),
    RelooperAddBranch (..),
    RelooperBlock (..),
    unreachableRelooperBlock,
    RelooperRun (..),
    FFIValueTypeRep (..),
    FFIValueType (..),
    FFIFunctionType (..),
    FFISafety (..),
    FFIImportDecl (..),
    FFIExportDecl (..),
    FFIMarshalState (..),
  )
where

import Asterius.Binary.Orphans ()
import Asterius.Binary.TH
import Asterius.NFData.TH
import Asterius.Types.EntitySymbol
import Asterius.Types.SymbolMap (SymbolMap)
import qualified Asterius.Types.SymbolMap as SM
import Asterius.Types.SymbolSet (SymbolSet)
import qualified Asterius.Types.SymbolSet as SS
import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString as BS
import Data.Data
import qualified Data.Map.Lazy as LM
import Foreign
import qualified Type.Reflection as TR

type BinaryenIndex = Word32

data AsteriusCodeGenError
  = UnsupportedCmmLit BS.ByteString
  | UnsupportedCmmInstr BS.ByteString
  | UnsupportedCmmBranch BS.ByteString
  | UnsupportedCmmType BS.ByteString
  | UnsupportedCmmWidth BS.ByteString
  | UnsupportedCmmGlobalReg BS.ByteString
  | UnsupportedCmmExpr BS.ByteString
  | UnsupportedCmmSectionType BS.ByteString
  | UnsupportedImplicitCasting Expression ValueType ValueType
  | AssignToImmutableGlobalReg UnresolvedGlobalReg
  deriving (Show, Data)

instance Exception AsteriusCodeGenError

data AsteriusStatic
  = SymbolStatic EntitySymbol Int
  | Uninitialized Int
  | Serialized BS.ByteString
  deriving (Show, Data)

data AsteriusStaticsType
  = ConstBytes
  | Bytes
  | InfoTable
  | Closure
  deriving (Eq, Show, Data)

data AsteriusStatics
  = AsteriusStatics
      { staticsType :: AsteriusStaticsType,
        asteriusStatics :: [AsteriusStatic]
      }
  deriving (Show, Data)

----------------------------------------------------------------------------

data AsteriusModule
  = AsteriusModule
      { staticsMap :: SymbolMap AsteriusStatics,
        staticsErrorMap :: SymbolMap AsteriusCodeGenError,
        functionMap :: SymbolMap Function,
        sptMap :: SymbolMap (Word64, Word64),
        ffiMarshalState :: FFIMarshalState
      }
  deriving (Show, Data)

instance Semigroup AsteriusModule where
  AsteriusModule sm0 se0 fm0 spt0 mod_ffi_state0 <> AsteriusModule sm1 se1 fm1 spt1 mod_ffi_state1 =
    AsteriusModule
      (sm0 <> sm1)
      (se0 <> se1)
      (fm0 <> fm1)
      (spt0 <> spt1)
      (mod_ffi_state0 <> mod_ffi_state1)

instance Monoid AsteriusModule where
  mempty = AsteriusModule mempty mempty mempty mempty mempty

----------------------------------------------------------------------------

-- | Location of an 'EntitySymbol' on drive. To be able to retrieve the entity
-- itself, depending on its origin, we have two cases:
-- * For entities originating in object files it suffices to know (a) the name
--   of the object file and (b) the absolute offset in the object file.
-- * For entities originating in archive files, we need (a) the name of the
--   archive file, (b) the name of the object file within the archive file, and
--   (c) the relative offset in the object file.
data EntityLocation
  = -- | object file origin
    InObjectFile
      { srcObj :: FilePath,
        srcOffset :: Integer
      }
    -- | archive file origin
  | InArchiveFile
      { srcAr :: FilePath,
        srcObj :: FilePath,
        srcOffset :: Integer
      }
  deriving (Show, Data)

----------------------------------------------------------------------------

-- TODO: This should replace all uses of AsteriusCachedModule basically.
data ModuleMetadata
  = ModuleMetadata
      { staticsDependencyMap :: SymbolMap SymbolSet,
        functionDependencyMap :: SymbolMap SymbolSet,
        errorsDependencyMap :: SymbolMap SymbolSet,
        entitySymbolIndex :: SM.SymbolMap EntityLocation,
        -- TODO: for now we duplicate the whole FFIMarshalState but we will probably
        -- prefer to have an index here, if that is possible.
        metaFFIMarshalState :: FFIMarshalState
      }
  deriving (Show, Data)

instance Semigroup ModuleMetadata where
  ModuleMetadata sdm0 fdm0 edm0 idx0 mod_ffi_state0 <> ModuleMetadata sdm1 fdm1 edm1 idx1 mod_ffi_state1 =
    ModuleMetadata
      (sdm0 <> sdm1)
      (fdm0 <> fdm1)
      (edm0 <> edm1)
      (idx0 <> idx1)
      (mod_ffi_state0 <> mod_ffi_state1)

instance Monoid ModuleMetadata where
  mempty = ModuleMetadata mempty mempty mempty mempty mempty

----------------------------------------------------------------------------

-- | An 'AsteriusRepModule' is the representation of an 'AsteriusModule' before
-- @gcSections@ has processed it. Note that this representation is supposed to
-- capture ALL data, whether it comes from object files (in @repMetadata@),
-- archive files (in @repMetadata@), or entities created using our EDSL (in
-- @inMemoryModule@). So, the picture should look as follows:
--
-- > On disk                  : AsteriusCachedModule
-- > In memory before GC pass : AsteriusRepModule
-- > In memory after GC pass  : AsteriusModule
data AsteriusRepModule
  = AsteriusRepModule
      { -- | All metadata for the module, as obtained from object and archive files.
        repMetadata :: ModuleMetadata,
        -- | In-memory parts of the module that are not yet stored anywhere on disk yet.
        inMemoryModule :: AsteriusModule
      }
  deriving (Show, Data)

instance Semigroup AsteriusRepModule where
  AsteriusRepModule meta0 inmem0 <> AsteriusRepModule meta1 inmem1 =
    AsteriusRepModule (meta0 <> meta1) (inmem0 <> inmem1)

instance Monoid AsteriusRepModule where
  mempty = AsteriusRepModule mempty mempty

----------------------------------------------------------------------------

-- | An 'AsteriusCachedModule' in an 'AsteriusModule' along with  with all of
-- its 'EntitySymbol' dependencies, as they are appear in the modules data
-- segments and function definitions (see function 'toCachedModule').
data AsteriusCachedModule
  = AsteriusCachedModule
      { cachedMetadata :: ModuleMetadata,
        fromCachedModule :: AsteriusModule
      }
  deriving (Show, Data)

instance Semigroup AsteriusCachedModule where
  AsteriusCachedModule meta0 m0 <> AsteriusCachedModule meta1 m1 =
    AsteriusCachedModule (meta0 <> meta1) (m0 <> m1)

instance Monoid AsteriusCachedModule where
  mempty = AsteriusCachedModule mempty mempty

-- | Convert an 'AsteriusModule' to an 'AsteriusCachedModule' by laboriously
-- computing the dependency graph for each 'EntitySymbol'. Historical note: we
-- used to compute the dependency graph during link time but that were quite
-- inefficient (see isssue #568). Instead, we now do the same work at
-- compile-time, thus creating object files containing 'AsteriusCachedModule's
-- instead of 'AsteriusModule's.
toCachedModule :: AsteriusModule -> AsteriusCachedModule
toCachedModule m =
  AsteriusCachedModule
    { cachedMetadata = createMetadata m,
      fromCachedModule = m
    }

createMetadata :: AsteriusModule -> ModuleMetadata
createMetadata m =
  ModuleMetadata
    { staticsDependencyMap = staticsMap m `add` SM.empty,
      functionDependencyMap = functionMap m `add` SM.empty,
      errorsDependencyMap = staticsErrorMap m `add` SM.empty,
      -- TODO empty for now but this is wrong. The @entitySymbolIndex@ can only
      -- be created once we have actually put the thing on disk. Fix this.
      entitySymbolIndex = mempty,
      metaFFIMarshalState = ffiMarshalState m -- TODO: hate to duplicate this.
    }
  where
    add :: Data a => SymbolMap a -> SymbolMap SymbolSet -> SymbolMap SymbolSet
    add = flip $ SM.foldrWithKey' (\k e -> SM.insert k (collectEntitySymbols e))

    -- Collect all entity symbols from an entity.
    collectEntitySymbols :: Data a => a -> SymbolSet
    collectEntitySymbols t
      | Just TR.HRefl <- TR.eqTypeRep (TR.typeOf t) (TR.typeRep @EntitySymbol) =
        SS.singleton t
      | otherwise =
        gmapQl (<>) SS.empty collectEntitySymbols t

----------------------------------------------------------------------------

data UnresolvedLocalReg
  = UniqueLocalReg Int ValueType
  | QuotRemI32X
  | QuotRemI32Y
  | QuotRemI64X
  | QuotRemI64Y
  deriving (Eq, Ord, Show, Data)

data UnresolvedGlobalReg
  = VanillaReg Int
  | FloatReg Int
  | DoubleReg Int
  | LongReg Int
  | Sp
  | SpLim
  | Hp
  | HpLim
  | CCCS
  | CurrentTSO
  | CurrentNursery
  | HpAlloc
  | EagerBlackholeInfo
  | GCEnter1
  | GCFun
  | BaseReg
  deriving (Show, Data)

data ValueType
  = I32
  | I64
  | F32
  | F64
  deriving (Eq, Ord, Enum, Show, Data)

data FunctionType
  = FunctionType
      { paramTypes, returnTypes :: [ValueType]
      }
  deriving (Eq, Ord, Show, Data)

data UnaryOp
  = ClzInt32
  | CtzInt32
  | PopcntInt32
  | NegFloat32
  | AbsFloat32
  | CeilFloat32
  | FloorFloat32
  | TruncFloat32
  | NearestFloat32
  | SqrtFloat32
  | EqZInt32
  | ClzInt64
  | CtzInt64
  | PopcntInt64
  | NegFloat64
  | AbsFloat64
  | CeilFloat64
  | FloorFloat64
  | TruncFloat64
  | NearestFloat64
  | SqrtFloat64
  | EqZInt64
  | ExtendSInt32
  | ExtendUInt32
  | WrapInt64
  | TruncSFloat32ToInt32
  | TruncSFloat32ToInt64
  | TruncUFloat32ToInt32
  | TruncUFloat32ToInt64
  | TruncSFloat64ToInt32
  | TruncSFloat64ToInt64
  | TruncUFloat64ToInt32
  | TruncUFloat64ToInt64
  | ReinterpretFloat32
  | ReinterpretFloat64
  | ConvertSInt32ToFloat32
  | ConvertSInt32ToFloat64
  | ConvertUInt32ToFloat32
  | ConvertUInt32ToFloat64
  | ConvertSInt64ToFloat32
  | ConvertSInt64ToFloat64
  | ConvertUInt64ToFloat32
  | ConvertUInt64ToFloat64
  | PromoteFloat32
  | DemoteFloat64
  | ReinterpretInt32
  | ReinterpretInt64
  deriving (Show, Data)

data BinaryOp
  = AddInt32
  | SubInt32
  | MulInt32
  | DivSInt32
  | DivUInt32
  | RemSInt32
  | RemUInt32
  | AndInt32
  | OrInt32
  | XorInt32
  | ShlInt32
  | ShrUInt32
  | ShrSInt32
  | RotLInt32
  | RotRInt32
  | EqInt32
  | NeInt32
  | LtSInt32
  | LtUInt32
  | LeSInt32
  | LeUInt32
  | GtSInt32
  | GtUInt32
  | GeSInt32
  | GeUInt32
  | AddInt64
  | SubInt64
  | MulInt64
  | DivSInt64
  | DivUInt64
  | RemSInt64
  | RemUInt64
  | AndInt64
  | OrInt64
  | XorInt64
  | ShlInt64
  | ShrUInt64
  | ShrSInt64
  | RotLInt64
  | RotRInt64
  | EqInt64
  | NeInt64
  | LtSInt64
  | LtUInt64
  | LeSInt64
  | LeUInt64
  | GtSInt64
  | GtUInt64
  | GeSInt64
  | GeUInt64
  | AddFloat32
  | SubFloat32
  | MulFloat32
  | DivFloat32
  | CopySignFloat32
  | MinFloat32
  | MaxFloat32
  | EqFloat32
  | NeFloat32
  | LtFloat32
  | LeFloat32
  | GtFloat32
  | GeFloat32
  | AddFloat64
  | SubFloat64
  | MulFloat64
  | DivFloat64
  | CopySignFloat64
  | MinFloat64
  | MaxFloat64
  | EqFloat64
  | NeFloat64
  | LtFloat64
  | LeFloat64
  | GtFloat64
  | GeFloat64
  deriving (Show, Data)

data Expression
  = Block
      { name :: BS.ByteString,
        bodys :: [Expression],
        blockReturnTypes :: [ValueType]
      }
  | If
      { condition, ifTrue :: Expression,
        ifFalse :: Maybe Expression
      }
  | Loop
      { name :: BS.ByteString,
        body :: Expression
      }
  | Break
      { name :: BS.ByteString,
        breakCondition :: Maybe Expression
      }
  | Switch
      { names :: [BS.ByteString],
        defaultName :: BS.ByteString,
        condition :: Expression
      }
  | Call
      { target :: EntitySymbol,
        operands :: [Expression],
        callReturnTypes :: [ValueType]
      }
  | CallImport
      { target' :: BS.ByteString,
        operands :: [Expression],
        callImportReturnTypes :: [ValueType]
      }
  | CallIndirect
      { indirectTarget :: Expression,
        operands :: [Expression],
        functionType :: FunctionType
      }
  | GetLocal
      { index :: BinaryenIndex,
        valueType :: ValueType
      }
  | SetLocal
      { index :: BinaryenIndex,
        value :: Expression
      }
  | TeeLocal
      { index :: BinaryenIndex,
        value :: Expression,
        valueType :: ValueType
      }
  | Load
      { signed :: Bool,
        bytes, offset :: BinaryenIndex,
        valueType :: ValueType,
        ptr :: Expression
      }
  | Store
      { bytes, offset :: BinaryenIndex,
        ptr, value :: Expression,
        valueType :: ValueType
      }
  | ConstI32 Int32
  | ConstI64 Int64
  | ConstF32 Float
  | ConstF64 Double
  | Unary
      { unaryOp :: UnaryOp,
        operand0 :: Expression
      }
  | Binary
      { binaryOp :: BinaryOp,
        operand0, operand1 :: Expression
      }
  | Drop
      { dropValue :: Expression
      }
  | ReturnCall
      { returnCallTarget64 :: EntitySymbol
      }
  | ReturnCallIndirect
      { returnCallIndirectTarget64 :: Expression
      }
  | Nop
  | Unreachable
  | CFG
      { graph :: RelooperRun
      }
  | Symbol
      { unresolvedSymbol :: EntitySymbol,
        symbolOffset :: Int
      }
  | UnresolvedGetLocal
      { unresolvedLocalReg :: UnresolvedLocalReg
      }
  | UnresolvedSetLocal
      { unresolvedLocalReg :: UnresolvedLocalReg,
        value :: Expression
      }
  | Barf
      { barfMessage :: BS.ByteString,
        barfReturnTypes :: [ValueType]
      }
  deriving (Show, Data)

data Function
  = Function
      { functionType :: FunctionType,
        varTypes :: [ValueType],
        body :: Expression
      }
  deriving (Show, Data)

data FunctionImport
  = FunctionImport
      { internalName, externalModuleName, externalBaseName :: BS.ByteString,
        functionType :: FunctionType
      }
  deriving (Show, Data)

data TableImport
  = TableImport
      { externalModuleName, externalBaseName :: BS.ByteString
      }
  deriving (Show, Data)

data MemoryImport
  = MemoryImport
      { externalModuleName, externalBaseName :: BS.ByteString
      }
  deriving (Show, Data)

data FunctionExport
  = FunctionExport
      { internalName, externalName :: BS.ByteString
      }
  deriving (Show, Data)

data FunctionTable
  = FunctionTable
      { tableFunctionNames :: [BS.ByteString],
        tableOffset :: BinaryenIndex
      }
  deriving (Show, Data)

data DataSegment
  = DataSegment
      { content :: BS.ByteString,
        offset :: Int32
      }
  deriving (Show, Data)

data Module
  = Module
      { functionMap' :: LM.Map BS.ByteString Function,
        functionImports :: [FunctionImport],
        functionExports :: [FunctionExport],
        functionTable :: FunctionTable,
        tableImport :: TableImport,
        tableSlots :: Int,
        memorySegments :: [DataSegment],
        memoryImport :: MemoryImport,
        memoryMBlocks :: Int
      }
  deriving (Show, Data)

data RelooperAddBlock
  = AddBlock
      { code :: Expression
      }
  | AddBlockWithSwitch
      { code, condition :: Expression
      }
  deriving (Show, Data)

data RelooperAddBranch
  = AddBranch
      { to :: BS.ByteString,
        addBranchCondition :: Maybe Expression
      }
  | AddBranchForSwitch
      { to :: BS.ByteString,
        indexes :: [BinaryenIndex]
      }
  deriving (Show, Data)

data RelooperBlock
  = RelooperBlock
      { addBlock :: RelooperAddBlock,
        addBranches :: [RelooperAddBranch]
      }
  deriving (Show, Data)

-- | A 'RelooperBlock' containing a single 'Unreachable' instruction.
unreachableRelooperBlock :: RelooperBlock
unreachableRelooperBlock =
  RelooperBlock -- See Note [unreachableRelooperBlock]
    { addBlock =
        AddBlock
          { code = Unreachable
          },
      addBranches = []
    }

data RelooperRun
  = RelooperRun
      { entry :: BS.ByteString,
        blockMap :: LM.Map BS.ByteString RelooperBlock,
        labelHelper :: BinaryenIndex
      }
  deriving (Show, Data)

data FFIValueTypeRep
  = FFILiftedRep
  | FFIUnliftedRep
  | FFIJSValRep
  | FFIIntRep
  | FFIWordRep
  | FFIAddrRep
  | FFIFloatRep
  | FFIDoubleRep
  deriving (Show, Data)

data FFIValueType
  = FFIValueType
      { ffiValueTypeRep :: FFIValueTypeRep,
        hsTyCon :: BS.ByteString
      }
  deriving (Show, Data)

data FFIFunctionType
  = FFIFunctionType
      { ffiParamTypes, ffiResultTypes :: [FFIValueType],
        ffiInIO :: Bool
      }
  deriving (Show, Data)

data FFISafety
  = FFIUnsafe
  | FFISafe
  | FFIInterruptible
  deriving (Eq, Show, Data)

data FFIImportDecl
  = FFIImportDecl
      { ffiFunctionType :: FFIFunctionType,
        ffiSafety :: FFISafety,
        ffiSourceText :: BS.ByteString
      }
  deriving (Show, Data)

data FFIExportDecl
  = FFIExportDecl
      { ffiFunctionType :: FFIFunctionType,
        ffiExportClosure :: EntitySymbol
      }
  deriving (Show, Data)

data FFIMarshalState
  = FFIMarshalState
      { ffiImportDecls :: SymbolMap FFIImportDecl,
        ffiExportDecls :: SymbolMap FFIExportDecl
      }
  deriving (Show, Data)

instance Semigroup FFIMarshalState where
  s0 <> s1 =
    FFIMarshalState
      { ffiImportDecls = ffiImportDecls s0 <> ffiImportDecls s1,
        ffiExportDecls = ffiExportDecls s0 <> ffiExportDecls s1
      }

instance Monoid FFIMarshalState where
  mempty = FFIMarshalState {ffiImportDecls = mempty, ffiExportDecls = mempty}

-- NFData instances

$(genNFData ''AsteriusCodeGenError)

$(genNFData ''AsteriusStatic)

$(genNFData ''AsteriusStaticsType)

$(genNFData ''AsteriusStatics)

$(genNFData ''AsteriusModule)

$(genNFData ''EntityLocation)

$(genNFData ''ModuleMetadata)

$(genNFData ''AsteriusRepModule)

$(genNFData ''AsteriusCachedModule)

$(genNFData ''UnresolvedLocalReg)

$(genNFData ''UnresolvedGlobalReg)

$(genNFData ''ValueType)

$(genNFData ''FunctionType)

$(genNFData ''UnaryOp)

$(genNFData ''BinaryOp)

$(genNFData ''Expression)

$(genNFData ''Function)

$(genNFData ''FunctionImport)

$(genNFData ''TableImport)

$(genNFData ''MemoryImport)

$(genNFData ''FunctionExport)

$(genNFData ''FunctionTable)

$(genNFData ''DataSegment)

$(genNFData ''Module)

$(genNFData ''RelooperAddBlock)

$(genNFData ''RelooperAddBranch)

$(genNFData ''RelooperBlock)

$(genNFData ''RelooperRun)

$(genNFData ''FFIValueTypeRep)

$(genNFData ''FFIValueType)

$(genNFData ''FFIFunctionType)

$(genNFData ''FFISafety)

$(genNFData ''FFIImportDecl)

$(genNFData ''FFIExportDecl)

$(genNFData ''FFIMarshalState)

-- Binary instances

$(genBinary ''AsteriusCodeGenError)

$(genBinary ''AsteriusStatic)

$(genBinary ''AsteriusStaticsType)

$(genBinary ''AsteriusStatics)

$(genBinary ''AsteriusModule)

$(genBinary ''EntityLocation)

$(genBinary ''ModuleMetadata)

$(genBinary ''AsteriusRepModule)

$(genBinary ''AsteriusCachedModule)

$(genBinary ''UnresolvedLocalReg)

$(genBinary ''UnresolvedGlobalReg)

$(genBinary ''ValueType)

$(genBinary ''FunctionType)

$(genBinary ''UnaryOp)

$(genBinary ''BinaryOp)

$(genBinary ''Expression)

$(genBinary ''Function)

$(genBinary ''FunctionImport)

$(genBinary ''TableImport)

$(genBinary ''MemoryImport)

$(genBinary ''FunctionExport)

$(genBinary ''FunctionTable)

$(genBinary ''DataSegment)

$(genBinary ''Module)

$(genBinary ''RelooperAddBlock)

$(genBinary ''RelooperAddBranch)

$(genBinary ''RelooperBlock)

$(genBinary ''RelooperRun)

$(genBinary ''FFIValueTypeRep)

$(genBinary ''FFIValueType)

$(genBinary ''FFIFunctionType)

$(genBinary ''FFISafety)

$(genBinary ''FFIImportDecl)

$(genBinary ''FFIExportDecl)

$(genBinary ''FFIMarshalState)
