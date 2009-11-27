{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}

module Text.PCLT.CatalogMaths__ where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.MyHelpers
import Text.PCLT.SH__
import Text.PCLT.CatalogMaths
import Text.PCLT.Catalog__
import Text.PCLT.SDL__
import Text.PCLT.Template__

----------------------------------------

instance ShowAsPCSI AddLngTpl_toPCLT_Error where
        showAsPCSI apstpte = thePCSI "E_PCLT_APSTPTE" [("pclt_apstpte_err_details", PCSI_PV err_pcsi)]
          where
            err_pcsi =
                case apstpte of
                    TplUniquenessViol_APSTPTE tuv ->
                            addToPCSI
                                [showAsPCSI tuv]
                                (   empPCSI "E_PCLT_APSTPTE_TPLUNQVIOL")
                    DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_APSTPTE dltcpsdfoon ->
                            addToPCSI
                                [showAsPCSI dltcpsdfoon]
                                (   empPCSI "E_PCLT_APSTPTE_DLTCPSDFOON")

instance HasStaticRawPCLTs AddLngTpl_toPCLT_Error where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLT_APSTPTE", (M.fromList [("rus", B.pack "##|E_PCLT_APSTPTE_PREFIX##|@@|pclt_apstpte_err_details@@|."), ("eng", B.pack "##|E_PCLT_APSTPTE_PREFIX##|@@|pclt_apstpte_err_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_APSTPTE_PREFIX", (M.fromList [("rus", B.pack "Произошла ошибка при прибавлен локализованой версии шаблона к локализуемому шаблону: "), ("eng", B.pack "An error occured when adding a localized template version to localizable template:")], str2PCLT_SDL Required_SDLM "##|E_PCLT_APSTPTE##|" inner_cfg))
                        , ("E_PCLT_APSTPTE_TPLUNQVIOL", (M.fromList [("rus", B.pack "##|E_PCLT_COMMONS_UNIQUEVIOL##|"), ("eng", B.pack "##|E_PCLT_COMMONS_UNIQUEVIOL##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_APSTPTE##|" inner_cfg))
                        , ("E_PCLT_APSTPTE_DLTCPSDFOON", (M.fromList [("rus", B.pack "##|E_PCLT_COMMONS_DLTCPSDFOON##|"), ("eng", B.pack "##|E_PCLT_COMMONS_DLTCPSDFOON##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_APSTPTE##|" inner_cfg))
                        ]

------------------------------------------------------
instance ShowAsPCSI AddPCLT_toPCLT_Error where
        showAsPCSI apttpte = thePCSI "E_PCLT_APTTPTE" [("pclt_apttpte_err_details", PCSI_PV err_pcsi)]
           where
              err_pcsi =
                case apttpte of
                    AddLngTpl_toPCLT_Error_APTTPTE apstpte ->
                            addToPCSI
                                [showAsPCSI apstpte]
                                (   empPCSI "E_PCLT_APTTPTE_APSTPTE")
                    DifferentSDLs_APTTPTE dsdls ->
                            addToPCSI
                                [showAsPCSI dsdls]
                                (   empPCSI "E_PCLT_APTTPTE_DIFFSDLS")

instance HasStaticRawPCLTs AddPCLT_toPCLT_Error where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLT_APTTPTE", (M.fromList [("rus", B.pack "##|E_PCLT_APTTPTE_PREFIX##|@@|pclt_apttpte_err_details@@|."), ("eng", B.pack "##|E_PCLT_APTTPTE_PREFIX##|@@|pclt_apttpte_err_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_APTTPTE_PREFIX", (M.fromList [("rus", B.pack "Произошла ошибка при слиянии 2-ух наборов локалицаций в один шаблон: "), ("eng", B.pack "An error occured when when merging 2 localizations sets into one templatestructure:")], str2PCLT_SDL Required_SDLM "##|E_PCLT_APTTPTE##|" inner_cfg))
                        , ("E_PCLT_APTTPTE_APSTPTE", (M.fromList [("rus", B.pack "##|E_PCLT_APSTPTE##|"), ("eng", B.pack "##|E_PCLT_APSTPTE##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_APTTPTE##|" inner_cfg))
                        , ("E_PCLT_APTTPTE_DIFFSDLS", (M.fromList [("rus", B.pack "##|E_PCLT_COMMONS_DIFFSDLS##|"), ("eng", B.pack "##|E_PCLT_COMMONS_DIFFSDLS##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_APTTPTE##|" inner_cfg))
                        ]

------------------------------------------------------

data PCLTRawCatalog__Text_PCLT_CatalogMaths = PCLTRawCatalog__Text_PCLT_CatalogMaths
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_CatalogMaths where
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: AddLngTpl_toPCLT_Error)
                        , getStaticRawPCLTs inner_cfg (undefined :: AddPCLT_toPCLT_Error)
                        ] -- i wish i knew an easy way how to achieve a smaller code of this by using a "map (getStaticRawPCLTs inner_cfg) [...]" function