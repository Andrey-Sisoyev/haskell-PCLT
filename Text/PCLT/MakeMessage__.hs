{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}

module Text.PCLT.MakeMessage__ where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.MyHelpers
import Text.PCLT.SH__
import Text.PCLT.MakeMessage
import Text.PCLT.Catalog__
import Text.PCLT.MakeCatalog__
import Text.PCLT.Template__
import Text.PCLT.ShowAsPCSI__

-------------------------------------------

instance ShowAsPCSI PCSI2Text_Error where
        showAsPCSI p2te = thePCSI "E_PCLT_P2TE"
                                      [ ("short_err_mark", PlainText_PV $ B.unpack $ shortOf_PCSI2Text_Error p2te)
                                      , ("pclt_p2te_err_details", PCSI_PV err_pcsi)
                                      ]
          where
             err_pcsi =
                case p2te of
                    RequiredCompositeIsMissing_P2TE rrcim ->
                            addToPCSI
                               [showAsPCSI rrcim]
                               (   empPCSI "E_PCLT_P2TE_MISSINGCOMPST")
                    RequiredCompositeLoclizationIsMissing_P2TE rrcim lng ->
                            addToPCSI
                               [showAsPCSI rrcim]
                               (   thePCSI "E_PCLT_P2TE_MISSINGLNGCOMPST" [("lng", PlainText_PV lng)])
                    CompositionCycle_P2TE cc ->
                            addToPCSI
                               [showAsPCSI cc]
                               (   empPCSI "E_PCLT_P2TE_COMPCYCL")
                    SDL_DeterminationFailure_P2TE tpl_id sdldf ->
                            addToPCSI
                               [showAsPCSI sdldf]
                               (   thePCSI "E_PCLT_P2TE_SDLDF" [("tpl_id" , PlainText_PV tpl_id)])
                    UnsupportedMarker_P2TE                     ssm contents tpl_id lng ->
                                thePCSI "E_PCLT_P2TE_USUPMARK"   [ ("tpl_id"  , PlainText_PV tpl_id)
                                                                 , ("lng"     , PlainText_PV lng)
                                                                 , ("contents", PlainTextLBS_PV $ truncLiteraryLBS contents 15)
                                                                 , ("ssm"     , PCSI_PV $ showAsPCSI $ SeparatedSectorMarker_PCSIWrapped ssm)
                                                                 ]
                    NoValueForParameter_P2TE                   tpl_id lng param_id ->
                                thePCSI "E_PCLT_P2TE_NOPARAMVAL" [ ("tpl_id"  , PlainText_PV tpl_id)
                                                                     , ("lng"     , PlainText_PV lng)
                                                                     , ("param_id", PlainText_PV param_id)
                                                                     ]
                    UnderAccordingParamReparsingFailure_P2TE        tpl_id lng param_id rpcsce ->
                                thePCSI "E_PCLT_P2TE_REPRSFAIL"  [ ("tpl_id"  , PlainText_PV tpl_id)
                                                                 , ("lng"     , PlainText_PV lng)
                                                                 , ("param_id", PlainText_PV param_id)
                                                                 , ("rpcsce"  , PCSI_PV $ showAsPCSI rpcsce)
                                                                 ]
                    ReparsingDepthMaxReached_P2TE                   tpl_id lng param_id reparsing_depth ->
                                thePCSI "E_PCLT_P2TE_REPRSDEPTH" [ ("tpl_id"  , PlainText_PV tpl_id)
                                                                 , ("lng"     , PlainText_PV lng)
                                                                 , ("param_id", PlainText_PV param_id)
                                                                 , ("reparsing_depth", PlainText_PV $ show reparsing_depth)
                                                                 ]
                    ReparsingLengthMaxReached_P2TE                  tpl_id lng param_id max_len ->
                                thePCSI "E_PCLT_P2TE_REPRSLEN"   [ ("tpl_id"  , PlainText_PV tpl_id)
                                                                 , ("lng"     , PlainText_PV lng)
                                                                 , ("param_id", PlainText_PV param_id)
                                                                 , ("max_len", PlainText_PV $ show max_len)
                                                                 ]
                    InstaniationLengthMaxReached_P2TE               tpl_id lng max_len ->
                                thePCSI "E_PCLT_P2TE_INSTLEN"    [ ("tpl_id"  , PlainText_PV tpl_id)
                                                                 , ("lng"     , PlainText_PV lng)
                                                                 , ("max_len", PlainText_PV $ show max_len)
                                                                 ]

instance HasStaticRawPCLTs PCSI2Text_Error where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLT_P2TE", (M.fromList [("rus",  B.pack "##|E_PCLT_P2TE_PREFIX##|@@|pclt_p2te_err_details@@|."), ("eng",  B.pack "##|E_PCLT_P2TE_PREFIX##|@@|pclt_p2te_err_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_P2TE_PREFIX", (M.fromList [("rus",  B.pack "Произошла ошибка (@@|short_err_mark@@|) при реализации сообщения из шаблона: "), ("eng",  B.pack "An error (@@|short_err_mark@@|) occurred, when trying to realize a message from a template: ")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_MISSINGCOMPST", (M.fromList [("rus",  B.pack "##|E_PCLT_COMMONS_MISSINGCOMPSTBYRER##|"), ("eng",  B.pack "##|E_PCLT_COMMONS_MISSINGCOMPSTBYRER##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_MISSINGLNGCOMPST", (M.fromList [("rus",  B.pack "(для языка \"@@|lng@@|\") ##|E_PCLT_COMMONS_MISSINGCOMPSTBYRER##|"), ("eng",  B.pack "(for language \"@@|lng@@|\") ##|E_PCLT_COMMONS_MISSINGCOMPSTBYRER##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_COMPCYCL", (M.fromList [("rus",  B.pack "##|E_PCLT_COMMONS_CCYCL##|"), ("eng",  B.pack "##|E_PCLT_COMMONS_CCYCL##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_SDLDF", (M.fromList [("rus",  B.pack "##|E_PCLT_SDLDF##|"), ("eng",  B.pack "##|E_PCLT_SDLDF##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_USUPMARK", (M.fromList [("rus", B.pack "неизвестный маркер обнаружен в шаблоне [ИД: @@|tpl_id@@|; язык: \"@@|lng@@|\"]. @@|ssm@@| Содержимое под маркером: @@|contents@@|"), ("eng", B.pack "unsupported marker appeara in the template [ID: @@|tpl_id@@|; language: \"@@|lng@@|\"]. @@|ssm@@| Marked text chunk: @@|contents@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_NOPARAMVAL", (M.fromList [("rus",  B.pack "для параметра \"@@|param_id@@|\" к реализации шаблона [ИД: @@|tpl_id@@|; язык: \"@@|lng@@|\"] не указано значение"), ("eng",  B.pack "no value specified for parameter \"@@|param_id@@|\" of template [ID: @@|tpl_id@@|; lng: \"@@|lng@@|\"]")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_REPRSFAIL", (M.fromList [("rus",  B.pack "произошла ошибка при репарсинге значения параметра \"@@|param_id@@|\" при реализации шаблона [ИД: @@|tpl_id@@|; язык: \"@@|lng@@|\"]. Ошибка: @@|rpcsce@@|"), ("eng",  B.pack "an error occurred, when reparsing a value specified for parameter \"@@|param_id@@|\" of template [ID: @@|tpl_id@@|; lng: \"@@|lng@@|\"]: @@|rpcsce@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_REPRSDEPTH", (M.fromList [("rus",  B.pack "достигнута максимальная глубина репарсинга (@@|reparsing_depth@@|). Дальнейшее углубление остановлено на репарсинге значения параметра \"@@|param_id@@|\" при реализации шаблона [ИД: @@|tpl_id@@|; язык: \"@@|lng@@|\"]"), ("eng",  B.pack "reparsing depth maximum (@@|reparsing_depth@@|) reached, - halted a reparsing of a value specified for parameter \"@@|param_id@@|\" of template [ID: @@|tpl_id@@|; lng: \"@@|lng@@|\"]")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_REPRSLEN", (M.fromList [("rus",  B.pack "достигнута максимальная длина (@@|max_len@@|) сторки подготавливаемой к репарсингу, при репарсинге значения параметра \"@@|param_id@@|\" при реализации шаблона [ИД: @@|tpl_id@@|; язык: \"@@|lng@@|\"]"), ("eng",  B.pack "the text, that is to be reparsed, length maximum (@@|max_len@@|) reached, when reparsing a value specified for parameter \"@@|param_id@@|\" of template [ID: @@|tpl_id@@|; lng: \"@@|lng@@|\"]")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        , ("E_PCLT_P2TE_INSTLEN", (M.fromList [("rus",  B.pack "достигнута максимальная длина (@@|max_len@@|) реализации сообщения из шаблона [ИД: @@|tpl_id@@|; язык: \"@@|lng@@|\"]"), ("eng",  B.pack "the realization length maximum (@@|max_len@@|) is reached, when realizing template [ID: @@|tpl_id@@|; lng: \"@@|lng@@|\"]")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE##|" inner_cfg))
                        -- ------------!!!
                        , ("E_PCLT_P2TE_LIST", (M.fromList [("rus", B.pack "##|E_PCLT_P2TE_LIST_PREFIX##|\n@@|errors_list@@|\n##|E_PCLT_P2TE_LIST_POSTFIX##|"), ("eng", B.pack "##|E_PCLT_P2TE_LIST_PREFIX##|\n@@|errors_list@@|\n##|E_PCLT_P2TE_LIST_POSTFIX##|")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_P2TE_LIST_PREFIX", (M.fromList [("rus", B.pack "----СЛЕЛУЮЩИЕ-ОШИБКИ-ВОЗНИКЛИ-ПРИ-ГЕНЕРАЦИИ-СООБЩЕНИЙ ИЗ-ШАБЛОНОВ----\n"), ("eng", B.pack "----FOLLOWING-ERRORS-OCCURED-WHEN-GENERATING-MESSAGES-FROM-TEMPLATES----------\n")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE_LIST##|" inner_cfg))
                        , ("E_PCLT_P2TE_LIST_POSTFIX", (M.fromList [("rus", B.pack "\n----------------\n----ОШИБКИ-ГЕНЕРАТОРА-СООБЩЕНИЙ-[END]----------\n"), ("eng", B.pack "\n----------------\n----MESSAGES-GENERATOR-ERRORS-[END]----------\n")], str2PCLT_SDL Required_SDLM "##|E_PCLT_P2TE_LIST##|" inner_cfg))
                        ]

---------------------------------------------

instance ShowAsPCSI SDL_DeterminationFailure where
        showAsPCSI sdldf = thePCSI "E_PCLT_SDLDF" [("pclt_sdldf_err_details", PCSI_PV err_pcsi)]
           where
             err_pcsi =
                case sdldf of
                    RequiredCompositeIsMissing_SDLDF rrcim ->
                            addToPCSI
                               [showAsPCSI rrcim]
                               (   empPCSI "E_PCLT_SDLDF_MISSCMPST")
                    MissingParam_SDLByParamCompositeLink_SDLDF     tpl_id param_id  ->
                                thePCSI "E_PCLT_SDLDF_MISSPARAM"    [ ("tpl_id"     , PlainText_PV tpl_id)
                                                                    , ("param_id"   , PlainText_PV param_id)
                                                                    ]
                    WrongParamType_SDLByParamCompositeLink_SDLDF   tpl_id param_id  ->
                                thePCSI "E_PCLT_SDLDF_WRNGPRMTYP"   [ ("tpl_id"     , PlainText_PV tpl_id)
                                                                    , ("param_id"   , PlainText_PV param_id)
                                                                    ]
                    UnknownComposite_SDLByParamCompositeLink_SDLDF tpl_id param_id wrng_tpl_id ->
                                thePCSI "E_PCLT_SDLDF_WRNGPRMCMPST" [ ("tpl_id"     , PlainText_PV tpl_id)
                                                                    , ("param_id"   , PlainText_PV param_id)
                                                                    , ("wrng_tpl_id", PlainText_PV wrng_tpl_id)
                                                                    ]
                    SDLReferentialCycle_SDLDF                      tpl_id super_buf ->
                                thePCSI "E_PCLT_SDLDF_REFCYCL"      [ ("tpl_id"     , PlainText_PV tpl_id)
                                                                    , ("super_buf"  , PlainText_PV $ show super_buf)
                                                                    ]
                    ErrornousSDL_SDLDF                             tpl_id esdl ->
                                thePCSI "E_PCLT_SDLDF_ESDL"         [ ("tpl_id"     , PlainText_PV tpl_id)
                                                                    , ("super_buf"  , PCSI_PV $ showAsPCSI esdl)
                                                                    ]

instance HasStaticRawPCLTs SDL_DeterminationFailure where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLT_SDLDF", (M.fromList [("rus",  B.pack "##|E_PCLT_SDLDF_PREFIX##|@@|pclt_sdldf_err_details@@|."), ("eng",  B.pack "##|E_PCLT_SDLDF_PREFIX##|@@|pclt_sdldf_err_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_SDLDF_PREFIX", (M.fromList [("rus", B.pack "Произошла ошибка при определении шаблоном {ИД: \"@@|tpl_id@@|\"} требуемого для отображения уровня детализации: "), ("eng", B.pack "An error occured when trying to determine required by the template {ID: \"@@|tpl_id@@|\"} representation detalization level: ")], str2PCLT_SDL Required_SDLM "##|E_PCLT_SDLDF##|" inner_cfg))
                        , ("E_PCLT_SDLDF_MISSCMPST", (M.fromList [("rus",  B.pack "##|E_PCLT_COMMONS_MISSINGCOMPSTBYRER##|"), ("eng",  B.pack "##|E_PCLT_COMMONS_MISSINGCOMPSTBYRER##|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_SDLDF##|" inner_cfg))
                        , ("E_PCLT_SDLDF_CMPZPARAMREF", (M.fromList [("rus",  B.pack "необходимый уровень детализации для отображения шаблона [ИД: @@|tpl_id@@|], указан быть таким же, как у шаблона, который подставляется под параметр [ИД: @@|param_id@@|]"), ("eng",  B.pack "of template (ID: @@|tpl_id@@|) for message representation required detailization level is referenced to be the same as is one of composite put behind parameter (ID: @@|param_id@@|)")], str2PCLT_SDL Required_SDLM "##|E_PCLT_SDLDF##|" inner_cfg))
                        , ("E_PCLT_SDLDF_MISSPARAM", (M.fromList [("rus",  B.pack "##|E_PCLT_SDLDF_CMPZPARAMREF##|, однако такой параметр вообще отсутствует на входе в реализацию шаблона"), ("eng",  B.pack "##|E_PCLT_SDLDF_CMPZPARAMREF##|, but referenced parameter is missing in realization input")], str2PCLT_SDL Required_SDLM "##|E_PCLT_SDLDF##|" inner_cfg))
                        , ("E_PCLT_SDLDF_WRNGPRMTYP", (M.fromList [("rus",  B.pack "##|E_PCLT_SDLDF_CMPZPARAMREF##|, однако под параметр подставлен не шаблон"), ("eng",  B.pack "##|E_PCLT_SDLDF_CMPZPARAMREF##|, but referenced parameter value is not a template")], str2PCLT_SDL Required_SDLM "##|E_PCLT_SDLDF##|" inner_cfg))
                        , ("E_PCLT_SDLDF_WRNGPRMCMPST", (M.fromList [("rus",  B.pack "##|E_PCLT_SDLDF_CMPZPARAMREF##|, однако под параметр подставлен шаблон [ИД: @@|wrng_tpl_id@@|] - каталогу неизвестный"), ("eng",  B.pack "##|E_PCLT_SDLDF_CMPZPARAMREF##|, but referenced parameter value is set to a template [ID: @@|wrng_tpl_id@@|], which is not in the catalog")], str2PCLT_SDL Required_SDLM "##|E_PCLT_SDLDF##|" inner_cfg))
                        , ("E_PCLT_SDLDF_REFCYCL", (M.fromList [("rus",  B.pack "у данного шаблона для отображения требуемый уровнь детализации такой же, как у указанного - при определении требуемого уровня детализации (остановлен на шаблоне [ИД: @@|tpl_id@@|]) обнаружен цикл таких ссылок. Буфер зацикленных шаблонов: @@|super_buf@@|"), ("eng",  B.pack "for message (of given template) representation required detailization level is referenced to be the same as is defined for the referenced template - in a group of such definitions a referential cycle is detected. Procedure of level requirements determination halted on template [ID: @@|tpl_id@@|]. Cycled tpls buffer: @@|super_buf@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_SDLDF##|" inner_cfg))
                        , ("E_PCLT_SDLDF_ESDL", (M.fromList [("rus",  B.pack "у шаблона [ИД: @@|tpl_id@@|] для отображения требуемый уровнь детализации ошибочен. @@|esdl_reason@@| Вместо определённого используется требование бесконечного уровня"), ("eng",  B.pack "for message (of template [ИД: @@|tpl_id@@|]) representation required detailization level is errornous. @@|esdl_reason@@| The requirement of infinite level is used instead")], str2PCLT_SDL Required_SDLM "##|E_PCLT_SDLDF##|" inner_cfg))
                        ]

--------------------------------------------------------------------

data PCLTRawCatalog__Text_PCLT_MakeMessage = PCLTRawCatalog__Text_PCLT_MakeMessage
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_MakeMessage where
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: PCSI2Text_Error)
                        , getStaticRawPCLTs inner_cfg (undefined :: SDL_DeterminationFailure)
                        ]-- i wish i knew an easy way how to achieve a smaller code of this by using a "map (getStaticRawPCLTs inner_cfg) [...]" function
