{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | Tools to use 'HasStaticRawPCLTs' class instances in order to build up
-- a catalog. (\"FromHSRT\" in module name means "from a given instance of
-- HasStaticRawPCLTs class")
module Text.PCLT.CatalogFromHSRT where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.MyHelpers
import Data.Either
import Data.Typeable
import Text.ConstraintedLBS
import Text.PCLT.Catalog
import Text.PCLT.CatalogMaths
import Text.PCLT.CatalogMaths__
import Text.PCLT.CommonTypes
import Text.PCLT.Config
import Text.PCLT.MakeCatalog
import Text.PCLT.MakeMessage
import Text.PCLT.MakeMessage2
import Text.PCLT.SDL
import Text.PCLT.PCSI
import Text.PCLT.Template
import Text.PCLT.HasStaticRawPCLTs
import Text.PCLT.ShowAsPCSI

------------------------------
-- |
data CatalogFromHSRTInitErrors = CatalogFromHSRTInitErrors {
                  cfhieCatalogID      :: PCLT_CatalogID
                , cfhieCatReadErrs    :: [ErrorWithPCSCatalog ReadPCSCatalogError]
                , cfhieCollectionErrs :: [AddPCLT_toPCLT_Error]
                }
               deriving (Show, Typeable)

defaultCatalogFromHSRTInitErrors :: PCLT_CatalogID -> CatalogFromHSRTInitErrors
defaultCatalogFromHSRTInitErrors cat_id = CatalogFromHSRTInitErrors {
                  cfhieCatalogID      = cat_id
                , cfhieCatReadErrs    = []
                , cfhieCollectionErrs = []
                }

initCatalogFromHSRT :: HasStaticRawPCLTs a => a -> PCLT_InnerConfig -> PCLT_CatalogID -> (PCLT_Catalog, CatalogFromHSRTInitErrors)
initCatalogFromHSRT a inner_cfg cat_id =
        let (raw_cat_input_data, pclt_sum_errs) = getStaticRawPCLTs inner_cfg a
            (catalog, cat_make_errs) = readPCLTCatalog
                                inner_cfg
                                cat_id
                                raw_cat_input_data
         in ( catalog
            , (defaultCatalogFromHSRTInitErrors cat_id) {
                          cfhieCollectionErrs = pclt_sum_errs
                        , cfhieCatReadErrs    = cat_make_errs
              }
            )

-- | A wrapper around 'initCatalogFromHSRT' function.
-- For case, when all errors are to be represented at once.
initCatalogFromHSRT_2 :: HasStaticRawPCLTs a => a -> PCLT_InnerConfig -> PCLT_CatalogID -> (StdErr_CLBS, ShowDetalizationLevel, LanguageName) -> (PCLT_Catalog, StdErr_CLBS)
initCatalogFromHSRT_2 a inner_config cat_id (stderr_clbs, sdl, lng) =
        let (cat, cfhie)  = initCatalogFromHSRT a inner_config cat_id
            errs_arethere = (not $ null $ cfhieCatReadErrs cfhie) || (not $ null $ cfhieCollectionErrs cfhie)
         in (cat, case errs_arethere of
                      True  -> pcsi2text_plus_errs_1 stderr_clbs (showAsPCSI cfhie) (sdl, lng) cat
                      False -> stderr_clbs
            )

------------------------------------------------------------
-- it is defined here, not in "CatalogFromHSRT__", because of recursive module imports restriction... an exclusive case
instance ShowAsPCSI CatalogFromHSRTInitErrors where
        showAsPCSI cfhie =
                thePCSI
                        "E_PCLTC_CFHIE"
                        [ ("pclt_cfhie_err_details", PCSIList_PV err_pcsi_list usualSeparatorInPCSIList)
                        , ("cat_id"               , PlainText_PV $ show $ cfhieCatalogID cfhie)
                        ]
              where
                err_pcsi_rpcsce_l = case cfhieCatReadErrs cfhie of
                                        []            -> []
                                        l             -> [ thePCSI "E_PCLTC_CFHIE_RPCSCE_L"  [("cre_list" , Indented_PV 4 $ PCSIList_PV (map showAsPCSI l) usualSeparatorInPCSIList)] ]
                err_pcsi_cole_l   = case cfhieCollectionErrs cfhie of
                                        []            -> []
                                        l             -> [ thePCSI "E_PCLTC_CFHIE_APTTPTE_L" [("cole_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI l) usualSeparatorInPCSIList)] ]
                err_pcsi_list     = err_pcsi_cole_l ++ err_pcsi_rpcsce_l
-------------------------------------------------------------

addFromHSRTToCatalog :: HasStaticRawPCLTs a => a -> PCLT_Catalog -> (PCLT_Catalog, CatalogFromHSRTInitErrors)
addFromHSRTToCatalog a catalog =
        let inner_cfg = pcltcInnerConfig catalog
            (raw_input, pclt_sum_errs) = getStaticRawPCLTs inner_cfg a
            (catalog2 , cat_read_errs) = _readPCLTCatalog (catalog, []) raw_input
         in ( catalog2
            , CatalogFromHSRTInitErrors {
                  cfhieCatalogID      = pcltcCatalogID catalog
                , cfhieCatReadErrs    = cat_read_errs
                , cfhieCollectionErrs = pclt_sum_errs
                }
            )

-- | A wrapper around 'addFromHSRTToCatalog' function.
-- For case, when all errors are to be represented at once.
addFromHSRTToCatalog_2 :: HasStaticRawPCLTs a => a -> PCLT_Catalog -> (StdErr_CLBS, ShowDetalizationLevel, LanguageName) -> (PCLT_Catalog, StdErr_CLBS)
addFromHSRTToCatalog_2 a catalog (stderr_clbs, sdl, lng) =
        let (catalog2, cfhie) = addFromHSRTToCatalog a catalog
            errs_arethere = (not $ null $ cfhieCatReadErrs cfhie) || (not $ null $ cfhieCollectionErrs cfhie)
         in (catalog2, case errs_arethere of
                      True  -> pcsi2text_plus_errs_1 stderr_clbs (showAsPCSI cfhie) (sdl, lng) catalog2
                      False -> stderr_clbs
            )
