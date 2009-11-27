{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}

module Text.PCLT.Template__ where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Text.PCLT.SH__
import Text.PCLT.ShowAsPCSI__
import Text.PCLT.SDL
import Text.PCLT.SDL__
import Text.PCLT.Template

------------------------------------------------------------------------------

data PCLT_ShowDetalizationLevel_PCSIWrapper = PCLT_ShowDetalizationLevel_PCSIWrapper PCLT_ShowDetalizationLevel
instance ShowAsPCSI PCLT_ShowDetalizationLevel_PCSIWrapper where
        showAsPCSI (PCLT_ShowDetalizationLevel_PCSIWrapper psdl) =
                thePCSI "PCLT_PSDL" [("psdl", PCSI_PV $ showAsPCSI psdl)]

instance ShowAsPCSI PCLT_ShowDetalizationLevel where
        showAsPCSI psdl = case psdl of
                             PCLT_SDL sdl ->
                                        thePCSI "PCLT_PSDL_SDL" [("sdl", PCSI_PV $ showAsPCSI sdl)]
                             PCLT_SDL_ToTemplateLink tpl_id ->
                                        thePCSI "PCLT_PSDL_CMPST" [("tpl_id", PlainText_PV tpl_id)]
                             PCLT_SDL_ToParamCompositeLink p_name ->
                                        thePCSI "PCLT_PSDL_PARAMCMPST" [("param_name", PlainText_PV p_name)]
                             PCLT_SDL_Errornous err ->
                                        thePCSI "PCLT_PSDL_ERR" [("err_details", PCSI_PV $ showAsPCSI err)]


instance HasStaticRawPCLTs PCLT_ShowDetalizationLevel where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("PCLT_PSDL", (M.fromList [("rus", B.pack "##|PCLT_PSDL_PREFIX##| @@|psdl@@|"), ("eng", B.pack "##|PCLT_PSDL_PREFIX##| @@|psdl@@|")], str2PCLT_SDL Required_SDLM "##|PCLT_SDL##|" inner_cfg))
                        , ("PCLT_PSDL_PREFIX", (M.fromList [("rus", B.pack "Уровень детализации отображения сообщения из шаблона (определённый при шаблоне, как минимальный необходимый для отображения сообщения): "), ("eng", B.pack "Show detalization level from a message template (specified by the template, as a required minimum for message representation):")], str2PCLT_SDL Required_SDLM "##|PCLT_PSDL##|" inner_cfg))
                        , ("PCLT_PSDL_SDL", (M.fromList [("rus", B.pack "@@|sdl@@|"), ("eng", B.pack "@@|sdl@@|")], str2PCLT_SDL Required_SDLM "##|PCLT_PSDL##|" inner_cfg))
                        , ("PCLT_PSDL_CMPST", (M.fromList [("rus", B.pack "такой же, как для шаблона '@@|tpl_id@@|'"), ("eng", B.pack "the same, as specified for template '@@|tpl_id@@|'")], str2PCLT_SDL Required_SDLM "##|PCLT_PSDL##|" inner_cfg))
                        , ("PCLT_PSDL_PARAMCMPST", (M.fromList [("rus", B.pack "такой же, как для шаблона, который должен быть под параметром '@@|param_name@@|'"), ("eng", B.pack "the same, as specified for template, that is to be put under parameter '@@|param_name@@|'")], str2PCLT_SDL Required_SDLM "##|PCLT_PSDL##|" inner_cfg))
                        , ("PCLT_PSDL_ERR", (M.fromList [("rus", B.pack "ошибка определения уровня ('@@|err_details@@|')"), ("eng", B.pack "SDL specification error ('@@|err_details@@|')")], str2PCLT_SDL Required_SDLM "##|PCLT_PSDL##|" inner_cfg))
                        ]

------------------------------------------------------------------------------

instance ShowAsPCSI PCLT_ErrornousSDL where
        showAsPCSI (UnreadableSDL_ESDL sdlm raw_input) =
            addToPCSI
                [showAsPCSI sdlm]
                (thePCSI "E_PCLT_ESDL" [("sdle_raw_inp", PlainText_PV raw_input)])

instance HasStaticRawPCLTs PCLT_ErrornousSDL where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("PCLT_ESDL", (M.fromList [("rus", B.pack "##|PCLT_ESDL_PREFIX##|@@|pclt_esdl_details@@|."), ("eng", B.pack "##|PCLT_ESDL_PREFIX##|@@|pclt_esdl_details@@|.")], str2PCLT_SDL Required_SDLM "##|PCLT_PSDL##|" inner_cfg))
                        , ("PCLT_ESDL_PREFIX", (M.fromList [("rus", B.pack "Причина ошибочности данного уровня детализации отображения сообщения: "), ("eng", B.pack "Error in definition of level of detalization of representation:")], str2PCLT_SDL Required_SDLM "##|PCLT_ESDL##|" inner_cfg))
                        , ("PCLT_ESDL_UNREAD", (M.fromList [("rus", B.pack "задан нечитаемый уровень. ##|PCLT_SDLM##| Текст заданого: \"@@|sdle_raw_inp@@|\" "), ("eng", B.pack "unreadable level specified. ##|PCLT_SDLM##| Input: \"@@|sdle_raw_inp@@|\"")], str2PCLT_SDL Required_SDLM "##|PCLT_ESDL##|" inner_cfg))
                        ]

------------------------------------------------------------------------------

instance ShowAsPCSI PCS_SpecificMarkings where
        showAsPCSI pcs_sm =
                case pcs_sm of
                    PlainText_LngTplM       -> empPCSI "PCLT_MARKER_PLAINTXT"
                    Parameter_LngTplM       -> empPCSI "PCLT_MARKER_PARAM"
                    Composite_LngTplM       -> empPCSI "PCLT_MARKER_COMPOSITE"
                    Unsupported_LngTplM ssm -> thePCSI "PCLT_MARKER_UNSUP" [("ssm", PCSI_PV $ showAsPCSI ssm)]

data PCS_SpecificMarkings_PCSIWrapped = PCS_SpecificMarkings_PCSIWrapped PCS_SpecificMarkings
instance ShowAsPCSI PCS_SpecificMarkings_PCSIWrapped where
        showAsPCSI (PCS_SpecificMarkings_PCSIWrapped pcs_sm) = thePCSI "PCLT_MARKER" [("pclt_marker_details", PCSI_PV $ showAsPCSI pcs_sm)]

instance HasStaticRawPCLTs PCS_SpecificMarkings where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("PCLT_MARKER", (M.fromList [("rus", B.pack "##|PCLT_MARKER_PREFIX##|@@|pclt_marker_details@@|."), ("eng", B.pack "##|PCLT_MARKER_PREFIX##|@@|pclt_marker_details@@|.")], str2PCLT_SDL Required_SDLM "##|PARSER_SSM##|" inner_cfg))
                        , ("PCLT_MARKER_PREFIX", (M.fromList [("rus", B.pack "PCLT маркер:"), ("eng", B.pack "PCLT marker: ")], str2PCLT_SDL Required_SDLM "##|PCLT_MARKER##|" inner_cfg))
                        , ("PCLT_MARKER_PLAINTXT", (M.fromList [("rus", B.pack "обычный текст"), ("eng", B.pack "plain text")], str2PCLT_SDL Required_SDLM "##|PCLT_MARKER##|" inner_cfg))
                        , ("PCLT_MARKER_PARAM", (M.fromList [("rus", B.pack "параметр"), ("eng", B.pack "parameter")], str2PCLT_SDL Required_SDLM "##|PCLT_MARKER##|" inner_cfg))
                        , ("PCLT_MARKER_COMPOSITE", (M.fromList [("rus", B.pack "композит"), ("eng", B.pack "composite")], str2PCLT_SDL Required_SDLM "##|PCLT_MARKER##|" inner_cfg))
                        , ("PCLT_MARKER_UNSUP", (M.fromList [("rus", B.pack "неподдерживаемый маркер \"@@|ssm@@|\""), ("eng", B.pack "unsupported marker \"@@|ssm@@|\"")], str2PCLT_SDL Required_SDLM "##|PCLT_MARKER##|" inner_cfg))
                        ]

------------------------------------------------------------------------------

instance ShowAsPCSI PCLT_ParserLowLevelFailure where
        showAsPCSI pclt_pllf =
                case pclt_pllf of
                    UnexpectedParserResult_PLLF_PCLT parse_result_str -> thePCSI "E_PCLT_PLLF_UNEXP"  [("parse_result_str", PlainText_PV parse_result_str)]
                    BadMarker_PLLF_PCLT               ssm s chunk_idx -> thePCSI "E_PCLT_PLLF_BADSSM" [("ssm", PCSI_PV $ showAsPCSI ssm), ("chunk_idx", PlainText_PV $ show chunk_idx)]
data PCLT_ParserLowLevelFailure_PCSIWrapped = PCLT_ParserLowLevelFailure_PCSIWrapped PCLT_ParserLowLevelFailure
instance ShowAsPCSI PCLT_ParserLowLevelFailure_PCSIWrapped where
        showAsPCSI (PCLT_ParserLowLevelFailure_PCSIWrapped pclt_pllf) = thePCSI "E_PCLT_PLLF" [("pclt_pllf_details", PCSI_PV $ showAsPCSI pclt_pllf)]

instance HasStaticRawPCLTs PCLT_ParserLowLevelFailure where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLT_PLLF", (M.fromList [("rus", B.pack "##|E_PCLT_PLLF_PREFIX##|@@|pclt_pllf_details@@|."), ("eng", B.pack "##|E_PCLT_PLLF_PREFIX##|@@|pclt_pllf_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLT_PLLF_PREFIX", (M.fromList [("rus", B.pack "Произошла ошибка в результате применения парсера, который разделяет обычный текст, параметры и композиты:"), ("eng", B.pack "An error occurred when applying parser, that separates plain text, parameters and composites: ")], str2PCLT_SDL Required_SDLM "##|E_PCLT_PLLF##|" inner_cfg))
                        , ("E_PCLT_PLLF_UNEXP", (M.fromList [("rus", B.pack "парсер вернул ненормальное состояние \"@@|parse_result_str@@|\""), ("eng", B.pack "parser returned an unexpected state \"@@|parse_result_str@@|\"")], str2PCLT_SDL Required_SDLM "##|E_PCLT_PLLF##|" inner_cfg))
                        , ("E_PCLT_PLLF_BADSSM", (M.fromList [("rus", B.pack "плохой маркер у куска текста (позиция: @@|chunk_idx@@|): @@|ssm@@|"), ("eng", B.pack "bad chunk marker (position: @@|chunk_idx@@|): @@|ssm@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLT_PLLF##|" inner_cfg))
                        ]

------------------------------------------------------------------------------

data PCLTRawCatalog__Text_PCLT_Template = PCLTRawCatalog__Text_PCLT_Template
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_Template where
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: PCLT_ErrornousSDL)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCS_SpecificMarkings)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLT_ParserLowLevelFailure)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLT_ShowDetalizationLevel)
                        ]-- i wish i knew an easy way how to achieve a smaller code of this by using a "map (getStaticRawPCLTs inner_cfg) [...]" function