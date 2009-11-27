{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}

module Text.PCLT.MakeCatalog__ where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Text.PCLT.SH__
import Text.PCLT.MakeCatalog
import Text.PCLT.Catalog__
import Text.PCLT.Template__

-------------------------------------------

instance ShowAsPCSI DRL_NormalizationError where
        showAsPCSI drlne = thePCSI "E_PCLT_DRLNE" [("pclt_drlne_err_details", PCSI_PV err_pcsi)]
           where
             err_pcsi =
                case drlne of
                    SDL_ToCompositeLinksCycle_DRLNE                              buf ->
                                thePCSI "E_PCLT_DRLNE_CYCL"        [("buf", PlainText_PV $ show buf)]
                    SDL_DetFail_ToCompositeLinkRefsToNonexistent_DRLNE rer_id red_id ->
                                thePCSI "E_PCLT_DRLNE_BADREF"      [("rer_id", PlainText_PV rer_id), ("red_id", PlainText_PV red_id)]

instance HasStaticRawPCLTs DRL_NormalizationError where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLT_DRLNE", (M.fromList [("rus", B.pack "##|E_PCLT_DRLNE_PREFIX##|@@|pclt_drlne_err_details@@|."), ("eng", B.pack "##|E_PCLT_DRLNE_PREFIX##|@@|pclt_drlne_err_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_DRLNE_PREFIX", (M.fromList [("rus", B.pack "Произошла ошибка во время окончательной нормализации [требований к уровням детализации для отображения]: "), ("eng", B.pack "An error occured during normalization of detalization level requirements (DRL) of message templetes:")], str2PCLT_SDL Required_SDLM "##|E_PCLT_DRLNE##|" inner_cfg))
                        , ("E_PCLT_DRLNE_CYCL", (M.fromList [("rus", B.pack "обнаружен ссылочный композиционный цикл. Буфер композитов: @@|buf@@|"), ("eng", B.pack "compositional referential cycle detected. Composites buffer: @@|buf@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_DRLNE##|" inner_cfg))
                        , ("E_PCLT_DRLNE_BADREF", (M.fromList [("rus", B.pack "не получается определить требование к уровню детализации шаблона (ИД: @@|red_id@@|), на который ссылается шаблон, чей ИД - @@|rer_id@@|. Шаблон на который ссылка — отсутствует."), ("eng", B.pack "couldn't determine the DRL of referenced template (ID: @@|red_id@@|), referenced by a composite (ID: @@|rer_id@@|) DRL. Referenced template is missing.")], str2PCLT_SDL Required_SDLM "##|E_PCLT_DRLNE##|" inner_cfg))
                        ]

-------------------------------------------------

instance ShowAsPCSI ReadPCSCatalogError where
        showAsPCSI rpcsce = thePCSI "E_PCLT_RPCSCE" [("pclt_rpcsce_err_details", PCSI_PV err_pcsi)]
           where
             err_pcsi =
                case rpcsce of
                    CompositionCycle_RPCSCE cc ->
                            addToPCSI
                                [ showAsPCSI cc]
                                (    empPCSI "E_PCLT_RPCSCE_CCYCL")
                    RequiredCompositeIsMissing_RPCSCE rrcim ->
                            addToPCSI
                                [ showAsPCSI rrcim]
                                (    empPCSI "E_PCLT_RPCSCE_MISSINGCOMPST")
                    RequiredCompositeIsUnparsable_RPCSCE rer_id red_id ->
                                thePCSI "E_PCLT_RPCSCE_UNPARSABLECOMPST"
                                        [ ("rer_id", PlainText_PV rer_id)
                                        , ("red_id", PlainText_PV red_id)
                                        ]
                    ParseFailedForDefaultLng_RPCSCE tpl_id pdw ll_errs_list ->
                                thePCSI "E_PCLT_RPCSCE_DFLTLNGPARSEFAIL"
                                        [ ("tpl_id", PlainText_PV tpl_id)
                                        , ("if_not_parsed", case pdw of {True -> Nothing_PV; False -> PCSI_PV $ empPCSI "E_PCLT_RPCSCE_DFLTLNGPARSEFAIL_C"} )
                                        , ("ll_errs_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI ll_errs_list) usualSeparatorInPCSIList)
                                        ]
                    TplDefaultLngIsMissing_RPCSCE tdlim ->
                            addToPCSI
                                [showAsPCSI tdlim]
                                (   empPCSI "E_PCLT_RPCSCE_TDLIM")
                    ParseFailedForNondefaultLng_RPCSCE tpl_id pdw lng ll_errs_list ->
                                thePCSI "E_PCLT_RPCSCE_NDFLTLNGPARSEFAIL"
                                        [ ("tpl_id", PlainText_PV tpl_id)
                                        , ("if_not_parsed", case pdw of {True -> Nothing_PV; False -> PCSI_PV $ empPCSI "E_PCLT_RPCSCE_NDFLTLNGPARSEFAIL_C"} )
                                        , ("ll_errs_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI ll_errs_list) usualSeparatorInPCSIList)
                                        , ("lng", PlainText_PV lng)
                                        ]
                    DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_RPCSCE dltcpsdfoon ->
                            addToPCSI
                                [showAsPCSI dltcpsdfoon]
                                (   empPCSI "E_PCLT_RPCSCE_DLTCPSDFOON")
                    DifferentSDLs_RPCSCE dsdls ->
                            addToPCSI
                                [showAsPCSI dsdls]
                                (   empPCSI "E_PCLT_RPCSCE_DIFFSDLS")
                    TplUniquenessViol_RPCSCE tuv ->
                            addToPCSI
                                [showAsPCSI tuv]
                                (   empPCSI "E_PCLT_RPCSCE_UNIQUEVIOL")
                    SDL_ToCompositeLinkRefsToNonexistent_RPCSCE rer_id red_id ->
                                thePCSI "E_PCLT_RPCSCE_SDLPTRTONONEXIST" [("rer_id", PlainText_PV rer_id), ("red_id", PlainText_PV red_id)]
                    DRL_NormalizationError_RPCSCE drlne ->
                                thePCSI "E_PCLT_RPCSCE_DRLNE" [("drlne", PCSI_PV $ showAsPCSI drlne)]

instance HasStaticRawPCLTs ReadPCSCatalogError where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLT_RPCSCE", (M.fromList [("rus",  B.pack "##|E_PCLT_RPCSCE_PREFIX##|@@|pclt_rpcsce_err_details@@|."), ("eng",  B.pack "##|E_PCLT_RPCSCE_PREFIX##|@@|pclt_rpcsce_err_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_PREFIX", (M.fromList [("rus",  B.pack "Произошла ошибка при чтении/формировании каталога шаблонов стандартных локализуемых детализуемых сообщений: "), ("eng",  B.pack "An error occured during formation of standard messages templates catalog:")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_CCYCL", (M.fromList [("rus",  B.pack "##|E_PCLT_COMMONS_CCYCL##|"), ("eng",  B.pack "##|E_PCLT_COMMONS_CCYCL##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_MISSINGCOMPST", (M.fromList [("rus",  B.pack "##|E_PCLT_COMMONS_MISSINGCOMPSTBYRER##|"), ("eng",  B.pack "##|E_PCLT_COMMONS_MISSINGCOMPSTBYRER##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_UNPARSABLECOMPST", (M.fromList [("rus",  B.pack "композит (ИД: @@|red_id@@|), на который ссылается шаблон (ИД: @@|rer_id@@|), наличествует во входе на формирование каталога, но (этот требуемый шаблон) не парсится"), ("eng",  B.pack "a composite (ID: @@|red_id@@|) required by the message template (ID: @@|rer_id@@|) is found in raw data for catalog, but parsing it failed")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_DFLTLNGPARSEFAIL", (M.fromList [("rus",  B.pack "шаблон (ИД: @@|tpl_id@@|) на основном языке не парсится без ошибок. @@|if_not_parsed@@|Список ошибок выданый парсером: @@|ll_errs_list@@|"), ("eng",  B.pack "a message template (ID: @@|tpl_id@@|) in default language is parsed (separation on composites, parameters and plain text) with errors. @@|if_not_parsed@@|Parse error(s): @@|ll_errs_list@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_DFLTLNGPARSEFAIL_C", (M.fromList [("rus",  B.pack "ДЛЯ ВСЕХ ЯЗЫКОВ данный шаблон будет отвергнут. "), ("eng",  B.pack "FOR ALL LANGUAGES the message template will be discarded. ")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE_DFLTLNGPARSEFAIL##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_TDLIM", (M.fromList [("rus",  B.pack "##|E_PCLT_COMMONS_TDLIM##|"), ("eng",  B.pack "##|E_PCLT_COMMONS_TDLIM##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_NDFLTLNGPARSEFAIL", (M.fromList [("rus", B.pack "шаблон (ИД: @@|tpl_id@@|) на НЕосновном языке ('@@|lng@@|') не парсится без ошибок. @@|if_not_parsed@@|Список ошибок выданый парсером: @@|ll_errs_list@@|"), ("eng", B.pack "a message template (ID: @@|tpl_id@@|) in nondefault language ('@@|lng@@|') is parsed (separation on composites, parameters and plain text) with errors. @@|if_not_parsed@@|Parse error(s): @@|ll_errs_list@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_NDFLTLNGPARSEFAIL_C", (M.fromList [("rus",  B.pack "Для данного языка этот шаблон будет отвергнут. "), ("eng",  B.pack "The message template for this language will be discarded. ")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE_NDFLTLNGPARSEFAIL##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_DLTCPSDFOON", (M.fromList [("rus",  B.pack "##|E_PCLT_COMMONS_DLTCPSDFOON##|"), ("eng",  B.pack "##|E_PCLT_COMMONS_DLTCPSDFOON##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_DIFFSDLS", (M.fromList [("rus", B.pack "##|E_PCLT_COMMONS_DIFFSDLS##|"), ("eng", B.pack "##|E_PCLT_COMMONS_DIFFSDLS##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_UNIQUEVIOL", (M.fromList [("rus", B.pack "##|E_PCLT_COMMONS_UNIQUEVIOL##|"), ("eng", B.pack "##|E_PCLT_COMMONS_UNIQUEVIOL##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_SDLPTRTONONEXIST", (M.fromList [("rus",  B.pack "необходимый для отображения (сообщения по шаблону ИД: @@|rer_id@@|)) уровень детализации определён быть таким же, как у композита [ИД: @@|red_id@@|], но последнего в каталоге нет"), ("eng",  B.pack "for message template (ID: @@|rer_id@@|) representation required detailization level is referenced to be the same as of another composite, but referenced composite (ID: @@|red_id@@|) is missing")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        , ("E_PCLT_RPCSCE_DRLNE", (M.fromList [("rus",  B.pack "@@|drlne@@|"), ("eng",  B.pack "@@|drlne@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_RPCSCE##|" inner_cfg))
                        ]

-------------------------------------------------------------------------------

instance ShowAsPCSI AHSTR2LngTpl_Error where
        showAsPCSI ahs2pe = thePCSI "E_PCLT_AHS2PE" [("pclt_ahs2pe_err_details", PCSI_PV err_pcsi)]
           where
             err_pcsi =
                case ahs2pe of
                    RequiredCompositeIsMissing_AHS2PE rcim ->
                            addToPCSI
                                [ showAsPCSI rcim]
                                (empPCSI "E_PCLT_AHS2PE_MISSINGCOMPST")
                    ParseFailure_AHS2PE ll_errs_list ->
                                 thePCSI "E_PCLT_AHS2PE_PARSEFAIL" [("ll_errs_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI ll_errs_list) usualSeparatorInPCSIList)]

instance HasStaticRawPCLTs AHSTR2LngTpl_Error where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLT_AHS2PE", (M.fromList [("rus", B.pack "##|E_PCLT_AHS2PE_PREFIX##|@@|pclt_ahs2pe_err_details@@|."), ("eng", B.pack "##|E_PCLT_AHS2PE_PREFIX##|@@|pclt_ahs2pe_err_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_AHS2PE_PREFIX", (M.fromList [("rus", B.pack "Произошла ошибка при чтении шаблона (параметризуемого сообщения) в тексте. Ошибка: "), ("eng", B.pack "An error occured in ad-hoc parsing of text, that's supposed to be a template for message:")], str2PCLT_SDL Required_SDLM "##|E_PCLT_AHS2PE##|" inner_cfg))
                        , ("E_PCLT_AHS2PE_MISSINGCOMPST", (M.fromList [("rus", B.pack "##|E_PCLT_COMMONS_MISSINGCOMPST##|"), ("eng", B.pack "##|E_PCLT_COMMONS_MISSINGCOMPST##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_AHS2PE##|" inner_cfg))
                        , ("E_PCLT_AHS2PE_PARSEFAIL", (M.fromList [("rus", B.pack "данный текст не парсится в шаблон без ошибок. Список ошибок выданый парсером: @@|ll_errs_list@@|"), ("eng", B.pack "the given text won't parse into a template. Parse error(s): @@|ll_errs_list@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_AHS2PE##|" inner_cfg))
                        ]

---------------------------------------
data PCLTRawCatalog__Text_PCLT_MakeCatalog = PCLTRawCatalog__Text_PCLT_MakeCatalog
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_MakeCatalog where
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: DRL_NormalizationError)
                        , getStaticRawPCLTs inner_cfg (undefined :: ReadPCSCatalogError)
                        , getStaticRawPCLTs inner_cfg (undefined :: AHSTR2LngTpl_Error)
                        ] -- i wish i knew an easy way how to achieve a smaller code of this by using a "map (getStaticRawPCLTs inner_cfg) [...]" function