{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}

module Text.PCLT.ShowAsPCSI__ where

import Control.Exception
import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Text.PCLT.SH__
import Text.PCLT.Parser.AdvancedSepBy

-----------------------------------------------------------------------
-------STANDARD-INSTANCES----------------------------------------------
-----------------------------------------------------------------------

-- WARNING: Creating an instance of ShowAsPCSI for String, LBS and/or other text types is not recommended.
--          Use of such instaniations would dread strictness of templates catalog structure - make it's use workaroundish, less systematic and less strict, which is a way to badmade applications.

instance ShowAsPCSI SomeException where
        showAsPCSI e = thePCSI "LLEXCPT" [("excpt_msg", PlainText_PV $ show e)]

instance ShowAsPCSI Bool where
        showAsPCSI b =
                case b of
                    True  -> thePCSI "TRUE"  []
                    False -> thePCSI "FALSE" []

instance ShowAsPCSI a => ShowAsPCSI (Maybe a) where
        showAsPCSI mb_a = thePCSI "MAYBE_A" [("maybe_cnstr", PCSI_PV maybe_cnstr_pcsi)]
             where
                maybe_cnstr_pcsi = case mb_a of
                                       Nothing -> empPCSI "MAYBE_NOTHING"
                                       Just  a -> thePCSI "MAYBE_JUST" [("a", PCSI_PV $ showAsPCSI a)]

-----------------------------------------------------------------------
-------STANDARD-INSTANCES-[END]----------------------------------------
-----------------------------------------------------------------------

instance ShowAsPCSI SeparatedSectorMarker where
        showAsPCSI sms = case sms of
                             Beginning_SSM          -> empPCSI "PARSER_SSM_BEGIN"
                             EOF_SSM                -> empPCSI "PARSER_SSM_EOF"
                             InnerMarker_SSM sm_idx -> thePCSI "PARSER_SSM_INNER" [("sm_idx", PlainText_PV $ show sm_idx)]
                             Error_SSM         smse -> thePCSI "PARSER_SSM_ERR"   [("smse"  , PCSI_PV $ showAsPCSI smse)]

data SeparatedSectorMarker_PCSIWrapped = SeparatedSectorMarker_PCSIWrapped SeparatedSectorMarker
instance ShowAsPCSI SeparatedSectorMarker_PCSIWrapped where
        showAsPCSI (SeparatedSectorMarker_PCSIWrapped sms) = thePCSI "PARSER_SSM" [("parser_ssm_details", PCSI_PV $ showAsPCSI sms)]

instance HasStaticRawPCLTs SeparatedSectorMarker where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("PARSER_SSM", (M.fromList [("rus", B.pack "##|PARSER_SSM_PREFIX##|@@|parser_ssm_details@@|."), ("eng", B.pack "##|PARSER_SSM_PREFIX##|@@|parser_ssm_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("PARSER_SSM_PREFIX", (M.fromList [("rus", B.pack "Маркер разделения секторов:"), ("eng", B.pack "Sector separation marker: ")], str2PCLT_SDL Required_SDLM "##|PARSER_SSM##|" inner_cfg))
                        , ("PARSER_SSM_BEGIN", (M.fromList [("rus", B.pack "начало ввода"), ("eng", B.pack "input beginning")], str2PCLT_SDL Required_SDLM "##|PARSER_SSM##|" inner_cfg))
                        , ("PARSER_SSM_EOF", (M.fromList [("rus", B.pack "конец ввода"), ("eng", B.pack "input ending")], str2PCLT_SDL Required_SDLM "##|PARSER_SSM##|" inner_cfg))
                        , ("PARSER_SSM_INNER", (M.fromList [("rus", B.pack "маркер #@@|sm_idx@@|"), ("eng", B.pack "marker #@@|sm_idx@@|")], str2PCLT_SDL Required_SDLM "##|PARSER_SSM##|" inner_cfg))
                        , ("PARSER_SSM_ERR", (M.fromList [("rus", B.pack "ошибка \"@@|smse@@|\""), ("eng", B.pack "error \"@@|smse@@|\"")], str2PCLT_SDL Required_SDLM "##|PARSER_SSM##|" inner_cfg))
                        ]

------------------------------------------------------------------------------

instance ShowAsPCSI StandartMarkingStrategyError where
        showAsPCSI smse = case smse of
                           InputAfterEOF_SMSE               -> empPCSI "E_PARSER_SMSE_INPUTAFTEREOF"
                           BeginningMNotInTheBeginning_SMSE -> empPCSI "E_PARSER_SMSE_BEGINNIGINMID"
                           OverlappingMarkedChunks_SMSE     -> empPCSI "E_PARSER_SMSE_OVERLAPMARK"
                           UnsupportedMarkersSequence_SMSE prev_active_sm cur_sm
                                                            -> thePCSI "E_PARSER_SMSE_UNSUPMARKSEQ"
                                                                           [ ("prev_active_sm", PCSI_PV $ showAsPCSI prev_active_sm)
                                                                           , ("cur_sm"        , PCSI_PV $ showAsPCSI cur_sm)
                                                                           ]
                           OpenMarkerAtEOF_SMSE      sm_idx -> thePCSI "E_PARSER_SMSE_UNFINWHENEOF"
                                                                           [("sm_idx"         , PlainText_PV $ show sm_idx)]
                           UnallowedCharacter_SMSE        c -> thePCSI "E_PARSER_SMSE_BADCHAR"
                                                                           [("c"              , PlainText_PV $ show c)]
data StandartMarkingStrategyError_PCSIWrapped = StandartMarkingStrategyError_PCSIWrapped StandartMarkingStrategyError
instance ShowAsPCSI StandartMarkingStrategyError_PCSIWrapped where
        showAsPCSI (StandartMarkingStrategyError_PCSIWrapped smse) = thePCSI "E_PARSER_SMSE" [("parser_smse_err_details", PCSI_PV $ showAsPCSI smse)]

instance HasStaticRawPCLTs StandartMarkingStrategyError where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PARSER_SMSE", (M.fromList [("rus", B.pack "##|E_PARSER_SMSE_PREFIX##|@@|parser_smse_err_details@@|."), ("eng", B.pack "##|E_PARSER_SMSE_PREFIX##|@@|parser_smse_err_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PARSER_SMSE_PREFIX", (M.fromList [("rus", B.pack "Произошла ошибка при грамматическом разборе маркированого текста. Ошибка маркирования:"), ("eng", B.pack "An error occured when parsing a marked text. Marking failed: ")], str2PCLT_SDL Required_SDLM "##|E_PARSER_SMSE##|" inner_cfg))
                        , ("E_PARSER_SMSE_INPUTAFTEREOF", (M.fromList [("rus", B.pack "ввод после конца ввода"), ("eng", B.pack "input is not allowed after EOF")], str2PCLT_SDL Required_SDLM "##|E_PARSER_SMSE##|" inner_cfg))
                        , ("E_PARSER_SMSE_BEGINNIGINMID", (M.fromList [("rus", B.pack "начало вводе не в начале"), ("eng", B.pack "beginning is allowed to occur only as the first input marker")], str2PCLT_SDL Required_SDLM "##|E_PARSER_SMSE##|" inner_cfg))
                        , ("E_PARSER_SMSE_OVERLAPMARK", (M.fromList [("rus", B.pack "пересекающиеся маркированые регионы не допускаются"), ("eng", B.pack "the marking strategy doesn't allow overlapping marked text chunks")], str2PCLT_SDL Required_SDLM "##|E_PARSER_SMSE##|" inner_cfg))
                        , ("E_PARSER_SMSE_UNSUPMARKSEQ", (M.fromList [("rus", B.pack "неподдерживаемая последовательность маркеров (активный предыдущий маркер -> текущий маркер): @@|prev_active_sm@@| -> @@|cur_sm@@|"), ("eng", B.pack "the marking strategy doesn't support markers sequence (active previous separator marker -> current separator marker): @@|prev_active_sm@@| -> @@|cur_sm@@|")], str2PCLT_SDL Required_SDLM "##|E_PARSER_SMSE##|" inner_cfg))
                        , ("E_PARSER_SMSE_UNFINWHENEOF", (M.fromList [("rus", B.pack "ввод оканчивается незакрытым маркированым регионом (индекс маркера: @@|sm_idx@@|)"), ("eng", B.pack "input ends with unclosed chunk of text (marker index: @@|sm_idx@@|)")], str2PCLT_SDL Required_SDLM "##|E_PARSER_SMSE##|" inner_cfg))
                        , ("E_PARSER_SMSE_BADCHAR", (M.fromList [("rus", B.pack "поток символов застопорился из-за недопустимого символа"), ("eng", B.pack "input flow stopped due to occurrence of unallowed character")], str2PCLT_SDL Required_SDLM "##|E_PARSER_SMSE##|" inner_cfg))
                        ]

------------------------------------------------------------------------------

data PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons = PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
             PCLT_RawCatalogData $ M.fromList
                                        [
                                          ("TRUE", (M.fromList [("rus",  B.pack "ДА"), ("eng",  B.pack "TRUE")], str2PCLT_SDL Required_SDLM "one" inner_cfg))
                                        , ("FALSE", (M.fromList [("rus",  B.pack "НЕТ"), ("eng",  B.pack "FALSE")], str2PCLT_SDL Required_SDLM "##|TRUE##|" inner_cfg))

                                        , ("MAYBE_A", (M.fromList [("rus",  B.pack "@@|maybe_cnstr@@|"), ("eng",  B.pack "@@|maybe_cnstr@@|")], str2PCLT_SDL Required_SDLM "##|TRUE##|" inner_cfg))
                                        , ("MAYBE_JUST", (M.fromList [("rus",  B.pack "@@|a@@|"), ("eng",  B.pack "@@|a@@|")], str2PCLT_SDL Required_SDLM "##|MAYBE_A##|" inner_cfg))
                                        , ("MAYBE_NOTHING", (M.fromList [("rus", B.pack "ничего"), ("eng", B.pack "nothing")], str2PCLT_SDL Required_SDLM "##|MAYBE_A##|" inner_cfg))

                                        , ("LLEXCPT", (M.fromList [("rus", B.pack "Программное исключение, детали: \"@@|excpt_msg@@|\""), ("eng", B.pack "Program exception, details: \"@@|excpt_msg@@|\"")], str2PCLT_SDL Required_SDLM "1000" inner_cfg))
                                        ]

------------------------------------------------------------------------------


data PCLTRawCatalog__Text_PCLT_ShowAsPCSI = PCLTRawCatalog__Text_PCLT_ShowAsPCSI
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_ShowAsPCSI where
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons)
                        , getStaticRawPCLTs inner_cfg (undefined :: SeparatedSectorMarker)
                        , getStaticRawPCLTs inner_cfg (undefined :: StandartMarkingStrategyError)
                        ] -- i wish i knew an easy way how to achieve a smaller code of this by using a "map (getStaticRawPCLTs inner_cfg) [...]" function