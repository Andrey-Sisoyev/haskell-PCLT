{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}

module Text.PCLT.Catalog__ where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Text.PCLT.SH__
import Text.PCLT.Catalog
import Text.PCLT.Template__

--------------------------------------

instance ShowAsPCSI DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_PCLTE where
        showAsPCSI (DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_PCLTE tpl_id lng) = thePCSI "E_PCLT_COMMONS_DLTCPSDFOON" [("tpl_id", PlainText_PV tpl_id), ("lng", PlainText_PV lng)]
instance ShowAsPCSI TplDefaultLngIsMissing_PCLTE where
        showAsPCSI (TplDefaultLngIsMissing_PCLTE tpl_id) = thePCSI "E_PCLT_COMMONS_TDLIM" [("tpl_id", PlainText_PV tpl_id)]
instance ShowAsPCSI RequiredCompositeIsMissing_PCLTE where
        showAsPCSI (RequiredCompositeIsMissing_PCLTE red_id) = thePCSI "E_PCLT_COMMONS_MISSINGCOMPST" [("red_id", PlainText_PV red_id)]
instance ShowAsPCSI RequiredByRequirerCompositeIsMissing_PCLTE where
        showAsPCSI (RequiredByRequirerCompositeIsMissing_PCLTE rer_id (RequiredCompositeIsMissing_PCLTE red_id)) =
                        thePCSI "E_PCLT_COMMONS_MISSINGCOMPSTBYRER" [("red_id", PlainText_PV red_id), ("rer_id", PlainText_PV rer_id)]
instance ShowAsPCSI CompositionCycle_PCLTE where
        showAsPCSI (CompositionCycle_PCLTE tpl_id buf) = thePCSI "E_PCLT_COMMONS_CCYCL" [("tpl_id", PlainText_PV tpl_id), ("buf", PlainText_PV $ show buf)]
instance ShowAsPCSI TplUniquenessViol_PCLTE where
        showAsPCSI (TplUniquenessViol_PCLTE tpl_id lng_list) = thePCSI "E_PCLT_COMMONS_UNIQUEVIOL" [("tpl_id", PlainText_PV tpl_id), ("lng_list", PlainText_PV $ show lng_list)]
instance ShowAsPCSI DifferentSDLs_PCLTE where
        showAsPCSI (DifferentSDLs_PCLTE tpl_id (main_sdl, add_sdl)) =
                                thePCSI "E_PCLT_COMMONS_DIFFSDLS"
                                            [ ("tpl_id"  , PlainText_PV tpl_id)
                                            , ("main_sdl", PCSI_PV $ showAsPCSI main_sdl)
                                            , ("add_sdl" , PCSI_PV $ showAsPCSI add_sdl)
                                            ]

data PCLTRawCatalog__Text_PCLT_PCLTCommons = PCLTRawCatalog__Text_PCLT_PCLTCommons
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_PCLTCommons where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLT_COMMONS_CCYCL", (M.fromList [("rus",  B.pack "обнаружен композиционный цикл в шаблоне сообщения (ИД: @@|tpl_id@@|), буфер композиций: @@|buf@@|"), ("eng",  B.pack "a composition cycle detected in a message template (ID: @@|tpl_id@@|), compositions path buffer: @@|buf@@|")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_COMMONS_MISSINGCOMPST", (M.fromList [("rus",  B.pack "композит (ИД: @@|red_id@@|) в каталоге отсутствует"), ("eng",  B.pack "a composite (ID: @@|red_id@@|) is missing in catalog")], str2PCLT_SDL Required_SDLM "##|E_PCLT_COMMONS_CCYCL##|" inner_cfg))
                        , ("E_PCLT_COMMONS_MISSINGCOMPSTBYRER", (M.fromList [("rus",  B.pack "##|E_PCLT_COMMONS_MISSINGCOMPST##|. На недостающий композит ссылается шаблон (ИД: @@|rer_id@@|)"), ("eng",  B.pack "##|E_PCLT_COMMONS_MISSINGCOMPST##|. The missing composite is required by the message template (ID: @@|rer_id@@|)")], str2PCLT_SDL Required_SDLM "##|E_PCLT_COMMONS_CCYCL##|" inner_cfg))
                        , ("E_PCLT_COMMONS_TDLIM", (M.fromList [("rus",  B.pack "шаблон (ИД: @@|tpl_id@@|) на основном языке не дан, но дан для др. языков. ДЛЯ ВСЕХ ЯЗЫКОВ данный шаблон будет отвергнут"), ("eng",  B.pack "A message template (ID: @@|tpl_id@@|) in default language is missing (while persisting for other languages). FOR ALL LANGUAGES the message template will be discarded")], str2PCLT_SDL Required_SDLM "##|E_PCLT_COMMONS_CCYCL##|" inner_cfg))
                        , ("E_PCLT_COMMONS_DLTCPSDFOON", (M.fromList [("rus", B.pack "шаблон (ИД: @@|tpl_id@@|) - его множества параметров и композитов отличаются для основного языка и для НЕосновного языка «@@|lng@@|». Версия шаблона на НЕосновном языке отвергнута (если такая строгость вас неустраивает, подрегулируйте параметр <StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets> в конфигурации каталога)"), ("eng", B.pack "A message template's (ID: @@|tpl_id@@|) sets of subcomposites and parameters differs for default and nondefault languages ('@@|lng@@|'). The template version for this nondefault language WILL BE DISCARDED (if such strictness doesn't fit your needs, change catalog configuration parameter <StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets>)")], str2PCLT_SDL Required_SDLM "##|E_PCLT_COMMONS_CCYCL##|" inner_cfg))
                        , ("E_PCLT_COMMONS_UNIQUEVIOL", (M.fromList [("rus", B.pack "в локализуемом шаблоне (ИД: @@|tpl_id@@|), уже присутствует версия на языках @@|lng_list@@|, - языковые версии шаблона на этих языках добавить нельзя (не выкинув старые версии)"), ("eng", B.pack "the localizeble template (ID: @@|tpl_id@@|) is already localized in languages @@|lng_list@@|, - new localizations' versions can't be added (without throwing away old ones)")], str2PCLT_SDL Required_SDLM "##|E_PCLT_COMMONS_CCYCL##|" inner_cfg))
                        , ("E_PCLT_COMMONS_DIFFSDLS", (M.fromList [("rus", B.pack "при сложении локализуемых шаблонов (ИД: @@|tpl_id@@|) обнаружено расхождение в их требованиях к уровню детализации необходимому для отображения. Требуемые уровни (прибавляемы шаблоном / шаблоном аккумулятором): @@|add_sdl@@|/@@|main_sdl@@|"), ("eng", B.pack "when adding up two localizables of one ID (\"@@|tpl_id@@|\") an inconsistency shows up - required show-detalization-levels differs for them. Required levels (of added tpl / of accumulator tpl): @@|add_sdl@@|/@@|main_sdl@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_COMMONS_CCYCL##|" inner_cfg))
                        ]

instance ShowAsPCSI a => ShowAsPCSI (ErrorWithPCSCatalog a) where
        showAsPCSI (ErrorWithPCSCatalog cat_id pclt_wpcsc_err_details) =
                thePCSI "E_PCSC"
                            [ ("cat_id", PlainText_PV $ show cat_id)
                            , ("pclt_wpcsc_err_details", PCSI_PV $ showAsPCSI pclt_wpcsc_err_details)
                            ]

data PCLTRawCatalog__Text_PCLT_ErrorWithPCSCatalog = PCLTRawCatalog__Text_PCLT_ErrorWithPCSCatalog
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_ErrorWithPCSCatalog where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCSC", (M.fromList [("rus", B.pack "##|E_PCLTC_PREFIX##| @@|pclt_wpcsc_err_details@@|"), ("eng", B.pack "##|E_PCLTC_PREFIX##| @@|pclt_wpcsc_err_details@@|")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLTC_PREFIX", (M.fromList [("rus", B.pack "Произошла ошибка в связи с каталогом [ИД: @@|cat_id@@|] шаблонов стандартных локализуемых детализуемых сообщений. "), ("eng", B.pack "An error occured in context of messages templates catalog [ID: @@|cat_id@@|].")], str2PCLT_SDL Required_SDLM "##|E_PCSC##|" inner_cfg))
                        ]

------------------------------------------------------------------------------

data PCLTRawCatalog__Text_PCLT_Catalog = PCLTRawCatalog__Text_PCLT_Catalog
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_Catalog where
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_PCLTCommons)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_ErrorWithPCSCatalog)
                        ] -- i wish i knew an easy way how to achieve a smaller code of this by using a "map (getStaticRawPCLTs inner_cfg) [...]" function