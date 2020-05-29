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
import qualified Asterius.Types.DependencyMap as DM
import qualified Asterius.Types.SymbolMap as SM
import qualified Asterius.Types.SymbolSet as SS
import Data.String

gcSections ::
  Bool ->
  AsteriusCachedModule ->
  SS.SymbolSet ->
  [EntitySymbol] ->
  AsteriusModule
gcSections verbose_err c_store_mod root_syms export_funcs =
  final_m
    { sptMap = spt_map,
      ffiMarshalState = ffi_this
    }
  where
    -- inputs
    store_mod = fromCachedModule c_store_mod
    deps = dependencyMap c_store_mod
    ffi_all = ffiMarshalState store_mod
    ffi_exports =
      ffiExportDecls ffi_all `SM.restrictKeys` SS.fromList export_funcs
    -- Real root symbols include the given root symbols and the exported functions.
    all_root_syms :: SS.SymbolSet
    all_root_syms =
      SS.fromList [ffiExportClosure | FFIExportDecl {..} <- SM.elems ffi_exports]
        <> root_syms
    -- outputs
    final_m = buildGCModule verbose_err all_root_syms deps (staticsMap store_mod) (functionMap store_mod)
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
  DM.DependencyMap ->
  SM.SymbolMap AsteriusStatics ->
  SM.SymbolMap Function ->
  AsteriusModule
buildGCModule verbose_err root_syms deps statics_map function_map = go (root_syms, SS.empty, mempty)
  where
    go (i_staging_syms, i_acc_syms, i_m)
      | SS.null i_staging_syms = i_m
      | otherwise = go (o_staging_syms, o_acc_syms, o_m)
      where
        o_acc_syms = i_staging_syms <> i_acc_syms
        (i_child_syms, o_m) = SS.foldr' step (SS.empty, i_m) i_staging_syms
        o_staging_syms = i_child_syms `SS.difference` o_acc_syms
        step i_staging_sym (i_child_syms_acc, o_m_acc)
          | Just ss <- SM.lookup i_staging_sym statics_map,
            es <- deps DM.! i_staging_sym = -- should always succeed
            (es <> i_child_syms_acc, extendStaticsMap o_m_acc i_staging_sym ss)
          | Just func <- SM.lookup i_staging_sym function_map,
            es <- deps DM.! i_staging_sym = -- should always succeed
            (es <> i_child_syms_acc, extendFunctionMap o_m_acc i_staging_sym func)
          | verbose_err =
            ( i_child_syms_acc,
              extendStaticsMap
                o_m_acc
                ("__asterius_barf_" <> i_staging_sym)
                (mkErrStatics i_staging_sym o_m_acc)
            )
          | otherwise =
            (i_child_syms_acc, o_m_acc)

-- | Create a new data segment, containing the barf message as a plain,
-- NUL-terminated bytestring.
mkErrStatics :: EntitySymbol -> AsteriusModule -> AsteriusStatics
mkErrStatics sym m =
  AsteriusStatics
    { staticsType = ConstBytes,
      asteriusStatics = [Serialized $ entityName sym <> err <> "\0"]
    }
  where
    err = case SM.lookup sym (staticsErrorMap m) of
      Just e -> fromString (": " <> show e)
      _ -> mempty

-- TODO: remove eventually.
extendStaticsMap :: AsteriusModule -> EntitySymbol -> AsteriusStatics -> AsteriusModule
extendStaticsMap m sym entry = m {staticsMap = SM.insert sym entry (staticsMap m)}

-- TODO: remove eventually.
extendFunctionMap :: AsteriusModule -> EntitySymbol -> Function -> AsteriusModule
extendFunctionMap m sym fun = m {functionMap = SM.insert sym fun (functionMap m)}
