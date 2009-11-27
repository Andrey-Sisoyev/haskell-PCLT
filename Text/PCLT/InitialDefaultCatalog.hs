{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

module Text.PCLT.InitialDefaultCatalog where

import Text.PCLT.SDL__               (PCLTRawCatalog__Text_PCLT_SDL)
import Text.PCLT.ShowAsPCSI__        (PCLTRawCatalog__Text_PCLT_ShowAsPCSI)
import Text.PCLT.Template__          (PCLTRawCatalog__Text_PCLT_Template)
import Text.PCLT.CatalogFromHSRT__   (PCLTRawCatalog__Text_PCLT_CatalogFromHSRT)
import Text.PCLT.CatalogMaths__      (PCLTRawCatalog__Text_PCLT_CatalogMaths)
import Text.PCLT.Catalog__           (PCLTRawCatalog__Text_PCLT_Catalog)
import Text.PCLT.MakeCatalog__       (PCLTRawCatalog__Text_PCLT_MakeCatalog)
import Text.PCLT.MakeMessage__       (PCLTRawCatalog__Text_PCLT_MakeMessage)

import Text.ConstraintedLBS
import Text.PCLT.Catalog           (PCLT_CatalogID, PCLT_Catalog)
import Text.PCLT.CatalogFromHSRT
import Text.PCLT.CommonTypes       (LanguageName)
import Text.PCLT.Config
import Text.PCLT.HasStaticRawPCLTs
import Text.PCLT.SDL               (ShowDetalizationLevel)

-- | This type is a special instance of 'HasStaticRawPCLTs' - it accumulates
-- all other instances of 'HasStaticRawPCLTs' from the whole PCLT package
data PCLTRawCatalog__Text_PCLT_InitialDefaultCatalog = PCLTRawCatalog__Text_PCLT_InitialDefaultCatalog

instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_InitialDefaultCatalog where
     widenessOfStaticRawPCLTsSet _ = Package_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_SDL)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_ShowAsPCSI)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_Template)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_CatalogFromHSRT)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_CatalogMaths)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_Catalog)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_MakeCatalog)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_MakeMessage)
                        ]

initDefaultCatalog   :: PCLT_InnerConfig -> PCLT_CatalogID                                                       -> (PCLT_Catalog, CatalogFromHSRTInitErrors)
initDefaultCatalog_2 :: PCLT_InnerConfig -> PCLT_CatalogID -> (StdErr_CLBS, ShowDetalizationLevel, LanguageName) -> (PCLT_Catalog, StdErr_CLBS)
initDefaultCatalog_3 ::                     PCLT_CatalogID -> (StdErr_CLBS, ShowDetalizationLevel, LanguageName) -> (PCLT_Catalog, StdErr_CLBS)

initDefaultCatalog   = initCatalogFromHSRT   PCLTRawCatalog__Text_PCLT_InitialDefaultCatalog
initDefaultCatalog_2 = initCatalogFromHSRT_2 PCLTRawCatalog__Text_PCLT_InitialDefaultCatalog
initDefaultCatalog_3 = initCatalogFromHSRT_2 PCLTRawCatalog__Text_PCLT_InitialDefaultCatalog defaultPCLTInnerConfig
