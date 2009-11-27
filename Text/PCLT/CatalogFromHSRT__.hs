{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}

module Text.PCLT.CatalogFromHSRT__ where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Text.PCLT.SH__
import Text.PCLT.CatalogFromHSRT
-------------------------------------------

-- instance ShowAsPCSI CatalogFromHSRTInitErrors  is defined in primary module (Text.PCLT.CatalogFromHSRT)

instance HasStaticRawPCLTs CatalogFromHSRTInitErrors where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLTC_CFHIE", (M.fromList [("rus", B.pack "##|E_PCLTC_CFHIE_PREFIX##| @@|pclt_cfhie_err_details@@|\n##|E_PCLTC_CFHIE_POSTFIX##|"), ("eng", B.pack "##|E_PCLTC_CFHIE_PREFIX##| @@|pclt_cfhie_err_details@@|\n##|E_PCLTC_CFHIE_POSTFIX##|")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLTC_CFHIE_PREFIX", (M.fromList [("rus", B.pack "Произошла одна или несколько ошибок при попытке инициализировать каталог [ИД: @@|cat_id@@|] из данных определённых через воплощение класса \"HasStaticRawPCLTs\": "), ("eng", B.pack "One on more errors occurred when trying to initialize a catalog [ID: @@|cat_id@@|] from an instance of class \"HasStaticRawPCLTs\": ")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CFHIE##|" inner_cfg))
                        , ("E_PCLTC_CFHIE_APTTPTE_L", (M.fromList [("rus", B.pack "\n ** Ошибки сборки множества шаблонов, из которых необходимо сформировать каталог: @@|cole_list@@|"), ("eng", B.pack "\n ** Errors of collecting templates for catalog formation input: @@|cole_list@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CFHIE##|" inner_cfg))
                        , ("E_PCLTC_CFHIE_RPCSCE_L", (M.fromList [("rus", B.pack "\n ** Ошибки чтения и формирования образа каталога: @@|cre_list@@|"), ("eng", B.pack "\n ** Catalog parsing errors: @@|cre_list@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CFHIE##|" inner_cfg))
                        , ("E_PCLTC_CFHIE_POSTFIX", (M.fromList [("rus", B.pack "С упущениями, но всё же каталог (частично) считан."), ("eng", B.pack "Despite of losses, the catalog is (partly) read.")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CFHIE##|" inner_cfg))
                        ]

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

data PCLTRawCatalog__Text_PCLT_CatalogFromHSRT = PCLTRawCatalog__Text_PCLT_CatalogFromHSRT
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_CatalogFromHSRT where
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                                [ getStaticRawPCLTs inner_cfg (undefined :: CatalogFromHSRTInitErrors) ]