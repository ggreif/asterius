{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Asterius.Passes.GCSections
  ( gcSections,
  )
where

import Asterius.Types
import Asterius.Types.EntitySymbol
import qualified Asterius.Types.SymbolMap as SM
import qualified Asterius.Types.SymbolSet as SS
import Data.Foldable
import Data.Maybe
import Data.String

-- TODO: Add a couple of comments about why we create the final
-- @ffiImportDecls@ the way we do:
--
-- As it can be seen in Asterius.Builtins, for each JSFFI import we have two
-- different things:

-- * The first is a FunctionImport, which is a wasm function import

-- * The second is a function wrapper which takes care of the i64/f64
--   conversion (for both arguments and results) and internally calls the real
--   function import.
--
-- Now, during GC, after we have the full set of used functions, some of the
-- functions may be JSFFI import function wrappers. So, given the wrapper
-- function symbols, we filter the JSFFI import declarations which actually
-- correspond to those symbols.

-- | Build an 'AsteriusModule' from an 'AsteriusRepModule', by keeping only the
-- parts of the program that are reachable from the given root symbols and
-- exported functions. Notice that this operation needs to be in 'IO', since
-- most parts of the generated 'AsteriusModule' need to be read from disk.
gcSections ::
  Bool ->
  AsteriusRepModule ->
  SS.SymbolSet ->
  [EntitySymbol] ->
  IO AsteriusModule
gcSections verbose_err module_rep root_syms export_funcs = do
  let (mod_syms, err_syms) = resolveSyms verbose_err all_root_syms module_rep
  buildGCModule mod_syms err_syms module_rep ffi_exports
  where
    ffi_exports =
      getCompleteFFIExportDecls module_rep `SM.restrictKeys` SS.fromList export_funcs
    -- Real root symbols include the given root symbols and the exported functions.
    all_root_syms =
      SS.fromList [ffiExportClosure | FFIExportDecl {..} <- SM.elems ffi_exports]
        <> root_syms

-- | Resolve all symbols that are reachable from the given root symnols. This
-- includes 2 categories: symbols that refer to statics and functions, and
-- symbols that refer to statics originating from barf messages (when
-- @verbose_err@ is set to @True@).
resolveSyms :: Bool -> SS.SymbolSet -> AsteriusRepModule -> (SS.SymbolSet, SS.SymbolSet)
resolveSyms verbose_err root_syms module_rep = go (root_syms, SS.empty, mempty, mempty)
  where
    go (i_staging_syms, i_acc_syms, i_m_syms, i_err_syms)
      | SS.null i_staging_syms = (i_m_syms, i_err_syms)
      | otherwise =
        let o_acc_syms = i_staging_syms <> i_acc_syms
            (i_child_syms, o_m_syms, o_err_syms) = SS.foldr' step (SS.empty, i_m_syms, i_err_syms) i_staging_syms
            o_staging_syms = i_child_syms `SS.difference` o_acc_syms
         in go (o_staging_syms, o_acc_syms, o_m_syms, o_err_syms)
      where
        step i_staging_sym (i_child_syms_acc, o_m_acc_syms, err_syms)
          | Just es <- i_staging_sym `SM.lookup` dependencyMap module_rep =
            (es <> i_child_syms_acc, o_m_acc_syms <> SS.singleton i_staging_sym, err_syms)
          | verbose_err =
            (i_child_syms_acc, o_m_acc_syms, err_syms <> SS.singleton i_staging_sym)
          | otherwise =
            (i_child_syms_acc, o_m_acc_syms, err_syms)

buildGCModule :: SS.SymbolSet -> SS.SymbolSet -> AsteriusRepModule -> SM.SymbolMap FFIExportDecl -> IO AsteriusModule
buildGCModule mod_syms err_syms module_rep ffi_exports = do
  -- 1. Build the statics and function maps
  statics_fns_m <- SS.foldrM addEntry mempty mod_syms
  -- 2. Add statics for barf-messages
  statics_fns_errs_m <- SS.foldrM addErrEntry statics_fns_m err_syms
  -- 3. Create the FFI imports
  let wrapper_fn_syms :: [EntitySymbol]
      wrapper_fn_syms = catMaybes . map stripWrapperSuffix . SS.toList . SM.keysSet $ functionMap statics_fns_m
  ffi_imports <- foldrM addFFIImportEntry mempty wrapper_fn_syms
  -- 4. Create the static pointers map
  let spt_map =
        getCompleteSptMap module_rep `SM.restrictKeys` SM.keysSet (staticsMap statics_fns_m)
  -- Combine it all
  pure $
    statics_fns_errs_m
      { sptMap = spt_map,
        ffiMarshalState =
          FFIMarshalState
            { ffiImportDecls = ffi_imports,
              ffiExportDecls = ffi_exports
            }
      }
  where
    addEntry sym m = findEntity module_rep sym >>= \case
      JustStatics statics -> pure $ extendStaticsMap m sym statics
      JustFunction function -> pure $ extendFunctionMap m sym function
      JustCodeGenError {} -> pure m -- Do nothing (couldn't happen with original implementation)
      JustFFIImportDecl {} -> pure m -- TODO: Do nothing (couldn't happen with original implementation)
      JustFFIExportDecl {} -> pure m -- TODO: Do nothing (couldn't happen with original implementation)
      NoEntity -> pure m -- Else, do nothing

    addErrEntry sym m = do
      statics <- mkErrStatics sym module_rep
      pure $ extendStaticsMap m ("__asterius_barf_" <> sym) statics

    addFFIImportEntry sym sm = findEntity module_rep sym >>= \case
      JustFFIImportDecl ffiimport -> pure $ SM.insert sym ffiimport sm
      _other -> pure sm -- TODO: think about this one

-- | Create a new data segment, containing the barf message as a plain,
-- NUL-terminated bytestring.
mkErrStatics :: EntitySymbol -> AsteriusRepModule -> IO AsteriusStatics
mkErrStatics sym module_rep = do
  err <- findEntity module_rep sym >>= \case
    JustCodeGenError e -> pure $ fromString (": " <> show e)
    _other -> pure mempty -- TODO: think about this one
  pure
    AsteriusStatics
      { staticsType = ConstBytes,
        asteriusStatics = [Serialized $ entityName sym <> err <> "\0"]
      }

-- TODO: remove eventually.
extendStaticsMap :: AsteriusModule -> EntitySymbol -> AsteriusStatics -> AsteriusModule
extendStaticsMap m sym entry = m {staticsMap = SM.insert sym entry (staticsMap m)}

-- TODO: remove eventually.
extendFunctionMap :: AsteriusModule -> EntitySymbol -> Function -> AsteriusModule
extendFunctionMap m sym fun = m {functionMap = SM.insert sym fun (functionMap m)}
