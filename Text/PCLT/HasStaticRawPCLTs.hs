{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | A way to store templates is by assigning them to types.
module Text.PCLT.HasStaticRawPCLTs where

import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.MyHelpers
import Data.Typeable
import Text.PCLT.Config
import Text.PCLT.Catalog
import Text.PCLT.CatalogMaths
import Text.PCLT.MakeCatalog
import Text.PCLT.Template

data RawPCLTsSetWideness = AlgebraicDataType_RPSW | Module_RPSW | Package_RPSW | System_RPSW deriving (Show, Typeable)
class HasStaticRawPCLTs t where
     getStaticRawPCLTs :: PCLT_InnerConfig -> t -> (PCLT_RawCatalogData, [AddPCLT_toPCLT_Error])
     -- assumed usage: getStaticRawPCLTs inner_cfg (undefined :: MyType)
     widenessOfStaticRawPCLTsSet :: t -> RawPCLTsSetWideness
     widenessOfStaticRawPCLTsSet _ = AlgebraicDataType_RPSW -- default
------------------------------

mergeRawCatalogDataSets :: Bool -> [PCLT_RawCatalogData] -> (PCLT_RawCatalogData, [AddPCLT_toPCLT_Error])
mergeRawCatalogDataSets allow_overlaps wrapped_rcd_l =
        let rcd_l = map (\ (PCLT_RawCatalogData rcd) -> rcd) wrapped_rcd_l
            (rcd_r, errs) = foldr f (M.empty, []) rcd_l
         in (PCLT_RawCatalogData rcd_r, errs)
    where
        f rcd (rcd_accum, errs_list) =
           unionWithKey_2
                (\ tpl_id ((bylng_m1, sdl1), (bylng_m2, sdl2)) ->
                        let mb_sdl_err =
                                case sdl1 == sdl2 of
                                    True  -> []
                                    False -> [DifferentSDLs_APTTPTE $ DifferentSDLs_PCLTE tpl_id (sdl1, sdl2)]
                         in case allow_overlaps of
                                True  -> ((M.union bylng_m1 bylng_m2, sdl1), mb_sdl_err)
                                False ->
                                    let (bylng_mr, errs) =
                                                unionWithKey_2
                                                    (\ lng (str1, _) ->
                                                                ( str1
                                                                , [AddLngTpl_toPCLT_Error_APTTPTE $ TplUniquenessViol_APSTPTE $ TplUniquenessViol_PCLTE tpl_id [lng]]
                                                                )
                                                    )
                                                    (++)
                                                    mb_sdl_err
                                                    bylng_m1
                                                    bylng_m2
                                     in ((bylng_mr, sdl1), errs ++ mb_sdl_err)
                )
                (++)
                errs_list
                rcd_accum
                rcd

mergeRawCatalogDataSets2 :: Bool -> [(PCLT_RawCatalogData, [AddPCLT_toPCLT_Error])] -> (PCLT_RawCatalogData, [AddPCLT_toPCLT_Error])
mergeRawCatalogDataSets2 allow_overlaps wrapped_rcd_l_w_errs =
        let (wrapped_rcd_l, _errs) = unzip wrapped_rcd_l_w_errs
            errs0 = concat _errs
            (wrapped_rcd_r, errs1) = mergeRawCatalogDataSets allow_overlaps wrapped_rcd_l
         in (wrapped_rcd_r, errs0 ++ errs1)
