{-# LANGUAGE DuplicateRecordFields #-} -- TODO: eventually remove.
{-# LANGUAGE GADTs #-}
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
  AsteriusModule
gcSections verbose_err module_rep root_syms export_funcs =
  final_m
    { sptMap = spt_map,
      ffiMarshalState = ffi_this
    }
  where
    -- inputs
    meta = repMetadata module_rep
    store_mod = inMemoryModule module_rep

    ffi_all = metaFFIMarshalState meta
    ffi_exports =
      ffiExportDecls ffi_all `SM.restrictKeys` SS.fromList export_funcs
    -- Real root symbols include the given root symbols and the exported functions.
    all_root_syms :: SS.SymbolSet
    all_root_syms =
      SS.fromList [ffiExportClosure | FFIExportDecl {..} <- SM.elems ffi_exports]
        <> root_syms
    -- outputs
    final_m = buildGCModule  -- TODO: eventually: buildGCModule verbose_err all_root_syms meta
                verbose_err
                all_root_syms
                meta
                (staticsMap store_mod)
                (functionMap store_mod)
                (staticsErrorMap store_mod)

    -- TODO: it seems that the static pointers map either (a) needs
    -- to be in the metadata (b) needs an index in the metadata. The
    -- metadata gets quite populated :/
    spt_map =
      sptMap store_mod `SM.restrictKeys` SM.keysSet (staticsMap final_m)
    ffi_this =
      ffi_all
        { ffiImportDecls = flip SM.filterWithKey (ffiImportDecls ffi_all) $ \k _ ->
            (k <> "_wrapper") `SM.member` functionMap final_m,
          ffiExportDecls = ffi_exports
        }

buildGCModule ::
  Bool ->
  SS.SymbolSet ->
  ModuleMetadata ->
  -- TODO: Eventually get rid of (pass offset index instead)
  SM.SymbolMap AsteriusStatics ->
  -- TODO: Eventually get rid of (pass offset index instead)
  SM.SymbolMap Function ->
  -- TODO: Eventually get rid of (pass offset index instead)
  SM.SymbolMap AsteriusCodeGenError ->
  AsteriusModule
buildGCModule verbose_err root_syms meta statics_map function_map error_map = go (root_syms, SS.empty, mempty)
  where
    go (i_staging_syms, i_acc_syms, i_m)
      | SS.null i_staging_syms = i_m
      | otherwise = go (o_staging_syms, o_acc_syms, o_m)
      where
        o_acc_syms = i_staging_syms <> i_acc_syms
        (i_child_syms, o_m) = SS.foldr' step (SS.empty, i_m) i_staging_syms
        o_staging_syms = i_child_syms `SS.difference` o_acc_syms
        step i_staging_sym (i_child_syms_acc, o_m_acc)
          | Just es <- i_staging_sym `SM.lookup` staticsDependencyMap meta,
            ss <- statics_map SM.! i_staging_sym = -- should always succeed
            (es <> i_child_syms_acc, extendStaticsMap o_m_acc i_staging_sym ss)
          | Just es <- i_staging_sym `SM.lookup` functionDependencyMap meta,
            func <- function_map SM.! i_staging_sym = -- should always succeed
            (es <> i_child_syms_acc, extendFunctionMap o_m_acc i_staging_sym func)
          | verbose_err =
            ( i_child_syms_acc,
              extendStaticsMap
                o_m_acc
                ("__asterius_barf_" <> i_staging_sym)
                (mkErrStatics i_staging_sym error_map)
            )
          | otherwise =
            (i_child_syms_acc, o_m_acc)

-- | Create a new data segment, containing the barf message as a plain,
-- NUL-terminated bytestring.
mkErrStatics :: EntitySymbol -> SM.SymbolMap AsteriusCodeGenError -> AsteriusStatics
mkErrStatics sym error_map =
  AsteriusStatics
    { staticsType = ConstBytes,
      asteriusStatics = [Serialized $ entityName sym <> err <> "\0"]
    }
  where
    err = case SM.lookup sym error_map of
      Just e -> fromString (": " <> show e)
      _ -> mempty

-- TODO: remove eventually.
extendStaticsMap :: AsteriusModule -> EntitySymbol -> AsteriusStatics -> AsteriusModule
extendStaticsMap m sym entry = m {staticsMap = SM.insert sym entry (staticsMap m)}

-- TODO: remove eventually.
extendFunctionMap :: AsteriusModule -> EntitySymbol -> Function -> AsteriusModule
extendFunctionMap m sym fun = m {functionMap = SM.insert sym fun (functionMap m)}
