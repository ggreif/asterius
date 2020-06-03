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
import qualified Asterius.Types.SymbolMap as SM
import qualified Asterius.Types.SymbolSet as SS
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

gcSections ::
  Bool ->
  AsteriusRepModule ->
  SS.SymbolSet ->
  [EntitySymbol] ->
  IO AsteriusModule
gcSections verbose_err module_rep root_syms export_funcs = do
  let -- inputs
      ffi_all = getCompleteFFIMarshalState module_rep
      ffi_exports =
        ffiExportDecls ffi_all `SM.restrictKeys` SS.fromList export_funcs
      -- Real root symbols include the given root symbols and the exported functions.
      all_root_syms :: SS.SymbolSet
      all_root_syms =
        SS.fromList [ffiExportClosure | FFIExportDecl {..} <- SM.elems ffi_exports]
          <> root_syms
  final_m <- buildGCModule verbose_err all_root_syms module_rep
  let spt_map =
        getCompleteSptMap module_rep `SM.restrictKeys` SM.keysSet (staticsMap final_m)
  -- NOTE: it seems that the static pointers map either (a) needs
  -- to be in the metadata (b) needs an index in the metadata. The
  -- metadata gets quite populated :/
  let ffi_this =
        ffi_all
          { ffiImportDecls = flip SM.filterWithKey (ffiImportDecls ffi_all) $ \k _ ->
              (k <> "_wrapper") `SM.member` functionMap final_m,
            ffiExportDecls = ffi_exports
          }
  pure $
    final_m
      { sptMap = spt_map,
        ffiMarshalState = ffi_this
      }

buildGCModule ::
  Bool ->
  SS.SymbolSet ->
  AsteriusRepModule ->
  IO AsteriusModule
buildGCModule verbose_err root_syms module_rep = go (root_syms, SS.empty, mempty)
  where
    go (i_staging_syms, i_acc_syms, i_m)
      | SS.null i_staging_syms = pure i_m
      | otherwise = do
        let o_acc_syms = i_staging_syms <> i_acc_syms
        (i_child_syms, o_m) <- SS.foldrM step (SS.empty, i_m) i_staging_syms
        let o_staging_syms = i_child_syms `SS.difference` o_acc_syms
        go (o_staging_syms, o_acc_syms, o_m)
      where
        step i_staging_sym (i_child_syms_acc, o_m_acc)
          | Just es <- i_staging_sym `SM.lookup` staticsDependencyMap (repMetadata module_rep) = do
            ss <- findStatics module_rep i_staging_sym
            pure (es <> i_child_syms_acc, extendStaticsMap o_m_acc i_staging_sym ss)
          | Just es <- i_staging_sym `SM.lookup` functionDependencyMap (repMetadata module_rep) = do
            func <- findFunction module_rep i_staging_sym
            pure (es <> i_child_syms_acc, extendFunctionMap o_m_acc i_staging_sym func)
          | verbose_err = do
            statics <- mkErrStatics i_staging_sym module_rep
            pure
              ( i_child_syms_acc,
                extendStaticsMap o_m_acc ("__asterius_barf_" <> i_staging_sym) statics
              )
          | otherwise =
            pure (i_child_syms_acc, o_m_acc)

-- | Create a new data segment, containing the barf message as a plain,
-- NUL-terminated bytestring.
mkErrStatics :: EntitySymbol -> AsteriusRepModule -> IO AsteriusStatics
mkErrStatics sym module_rep = do
  err <- findCodeGenError module_rep sym >>= \case
    Just e -> pure $ fromString (": " <> show e)
    Nothing -> pure mempty
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
