{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | Here are declared 'LocalizableTemplate' (also called PCLT)
-- and 'LocalizedTemplate'.
-- Here by localization is meant localization in languages.
-- First (localizable template) is above languages,
-- while second (localized template) is a template version
-- in a concrete language.
module Text.PCLT.Template where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.MyHelpers
import Data.Typeable
import Text.PCLT.Parser.AdvancedSepBy
import Text.PCLT.Parser.ParserInternals
import Text.PCLT.CommonTypes
import Text.PCLT.Config
import Text.PCLT.SDL

-- * Template pieces

-- | These are types of template pieces. They are made by 'ssm2ldtm'
-- from 'Text.PCLT.Parser.AdvancedSepBy.SeparatedSectorMarker'
data PCS_SpecificMarkings =
         PlainText_LngTplM
       | Parameter_LngTplM
       | Composite_LngTplM
       | Unsupported_LngTplM SeparatedSectorMarker
     deriving (Eq, Show, Typeable)

ssm2ldtm :: SeparatedSectorMarker -> PCS_SpecificMarkings
ssm2ldtm ssm =
           case ssm of
               Error_SSM err_msg -> Unsupported_LngTplM ssm
               InnerMarker_SSM i ->
                             if i == 0 then PlainText_LngTplM
                        else if i == 1 then Parameter_LngTplM
                        else if i == 2 then Composite_LngTplM
                        else                Unsupported_LngTplM ssm
               _ -> Unsupported_LngTplM ssm

-- | Template content.
type LngTpl_AbstractedString  = [(PCS_SpecificMarkings, Lazy.ByteString, MarkedChunkLength)]

-- | Extract a list of parameter names from a template content.
listOfParams :: LngTpl_AbstractedString -> [ParamName_LBS]
listOfParams str_struct = foldl (\ accum (marker, str, _) -> case marker == Parameter_LngTplM of {True -> str : accum; False -> accum }) [] str_struct

------------------------------------------------------------------------

-- * Parsing from a lazy ByteString to a localized template content

type ParserBadResult = String
-- | These errors are possible only if program is wrong.
data PCLT_ParserLowLevelFailure =
          UnexpectedParserResult_PLLF_PCLT ParserBadResult
        | BadMarker_PLLF_PCLT SeparatedSectorMarker Lazy.ByteString ChunkIndexInList_
     deriving (Show, Typeable)

-- | The parsing uses parameters
-- 'Test.PCLT.Config.pcsParameterPlaceholderWrapper' and
-- 'Test.PCLT.Config.pcsCompositePlaceholderWrapper' of
-- 'Test.PCLT.Config.PCLT_InnerConfig'.
-- The list @[PCLT_CompositeKey]@ in the result is a list of composite keys
-- (template IDs, used by template as inclusions)
doTheParse :: PCLT_InnerConfig
           -> Lazy.ByteString
           -> ( [PCLT_ParserLowLevelFailure], Maybe ( LngTpl_AbstractedString, [PCLT_CompositeKey] ))
doTheParse pcsc_config str =
    let parser = sepBySome
                        anyChar
                        standardMarkingStrategy
                        [ stringLBS $ pcsParameterPlaceholderWrapper pcsc_config
                        , stringLBS $ pcsCompositePlaceholderWrapper pcsc_config
                        ]
     in case parse parser str of
            ( IllegalInput               , _ ) -> ([UnexpectedParserResult_PLLF_PCLT "IllegalInput"], Nothing)
            ( ReachedEOF                 , _ ) -> ([UnexpectedParserResult_PLLF_PCLT "ReachedEOF"]  , Nothing)
            ( Success  marked_chunks_list, _ ) ->
                let _fixed_marked_chunks_list = standardMarkingStrategyFix_StripEmptyChunks marked_chunks_list
                    list_of_parser_errors     = map (\ (ssm, s, idx) -> BadMarker_PLLF_PCLT ssm s idx) $ retrieveErrorsMarkingsList _fixed_marked_chunks_list
                    non_plain_markings_map    = retrieveNonPlainMarkingsMap _fixed_marked_chunks_list
                    fixed_marked_chunks_list  = map (\ (ssm, str, len) -> (ssm2ldtm ssm, str, len)) _fixed_marked_chunks_list
                    list_of_composites_keys   = map B.unpack $ fst $ unzip $ getListOfMarkings non_plain_markings_map 2
                 in (list_of_parser_errors, Just (fixed_marked_chunks_list, list_of_composites_keys))

------------------------------------------------------------------------

-- * Localized template

type PCLT_CatalogMap          = Map PCLT_ID LocalizableTemplate
type LngTpl_SubCompositesMap  = PCLT_CatalogMap

data LocalizedTemplate =
        LocalizedTemplate {
                  ldtAbstractedString  :: LngTpl_AbstractedString
                -- | Each composition tree is kept together with each
                -- localization. This is done for speedup and is a source
                -- of complexities, when forming a catalog and sustaining it's
                -- data consistency. So it comes to this:
                -- templates are purely-referenced by
                --
                -- * catalog ('PCLT_CatalogMap') and
                --
                -- * templates, that uses them as composites
                -- ('LngTpl_SubCompositesMap').
                --
                -- By \"purely-reference\" here is meant, that templates are
                -- formed only once, they have one instace in memory, but
                -- are referenced twice - from composeds and from catalog map
                , ldtSubcompositesMap  :: LngTpl_SubCompositesMap
        }
     deriving (Show, Typeable)

-- * Text.PCLT.Config.pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets

type DefaultLngTpl    = LocalizedTemplate
type NondefaultLngTpl = LocalizedTemplate
-- | Carrying strict orientation routines. See description of
-- 'Text.PCLT.Config.StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets'.
compareStrictOrientationOnDefault :: PCLT_ID -> StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets -> NondefaultLngTpl -> DefaultLngTpl -> Bool
compareStrictOrientationOnDefault tpl_id so nondflt_ldt dflt_ldt =
        let (   dflt_subcomps    ,    dflt_params    ) = (fst . unzip . M.toList . ldtSubcompositesMap, listOfParams . ldtAbstractedString) `apFrom2ple` dflt_ldt
            (nondflt_subcomps    , nondflt_params    ) = (fst . unzip . M.toList . ldtSubcompositesMap, listOfParams . ldtAbstractedString) `apFrom2ple` nondflt_ldt
         in _compareStrictOrientationOnDefault tpl_id so (nondflt_subcomps, nondflt_params) (dflt_subcomps, dflt_params)

_compareStrictOrientationOnDefault :: PCLT_ID -> StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets -> ([PCLT_ID], [ParamName_LBS]) -> ([PCLT_ID], [ParamName_LBS]) -> Bool
_compareStrictOrientationOnDefault tpl_id so (nondflt_subcomps, nondflt_params) (dflt_subcomps, dflt_params) =
        let memb  = elem tpl_id $ soExcludingInComposites so
            crit1 =        soStrict_IsIt so  && (not memb)
            crit2 = (not $ soStrict_IsIt so) &&      memb
            crit  = crit1 || crit2
            local_c_exclusions = soExcludingComposites so ++ (snd $ unzip $ filter
                                                                                (\ (_tpl_id, _) -> _tpl_id == tpl_id)
                                                                                (soExcludingCompComposites so)
                                                             )
            local_p_exclusions =
                              map
                                B.pack
                                ( soExcludingParameters so ++ (snd $ unzip $ filter
                                                                                (\ (_tpl_id, _) -> _tpl_id == tpl_id)
                                                                                (soExcludingCompParameters so)
                                )                             )

            op :: Eq a => [a] -> [a] -> [a]
            op = case crit of
                     True  -> (\\)
                     False -> intersect
            (   so_dflt_subcomps    ,    so_dflt_params    ) = (    dflt_subcomps `op`     local_c_exclusions,    dflt_params `op` local_p_exclusions)
            (so_nondflt_subcomps    , so_nondflt_params    ) = ( nondflt_subcomps `op`     local_c_exclusions, nondflt_params `op` local_p_exclusions)
            (  so_union_subcomps    ,   so_union_params    ) = ( so_dflt_subcomps `union` so_nondflt_subcomps, so_dflt_params `union` so_nondflt_params)
            (so_nondflt_subcomps_len, so_nondflt_params_len) = (length so_nondflt_subcomps, length so_nondflt_params)
            (   so_dflt_subcomps_len,    so_dflt_params_len) = (length    so_dflt_subcomps, length    so_dflt_params)
            (  so_union_subcomps_len,   so_union_params_len) = (length   so_union_subcomps, length   so_union_params)

         in    so_union_subcomps_len ==    so_dflt_subcomps_len && so_union_params_len ==    so_dflt_params_len
            && so_union_subcomps_len == so_nondflt_subcomps_len && so_union_params_len == so_nondflt_params_len

------------------------------------------------------------------------
-- * Requirement for making a representation from template - SDL

-- | This is an extending wrapper around SDL. It is used for specification
-- of requirement for making representation from template. This specification
-- is attached to every localizable template in PCLT catalog
data PCLT_ShowDetalizationLevel =
      -- | Plain SDL, nominal. If SDL of representation reciever
      -- is less then that, then template cann't be used in representation
      -- generation.
        PCLT_SDL ShowDetalizationLevel
      -- | \"The requirement is the same as is specified
      -- for referenced template\".
      | PCLT_SDL_ToTemplateLink PCLT_ID
      -- | \"The requirement is the same as is specified a for referenced
      -- template, which is referenced by a @PCSI_PV@ value of referenced
      -- parameter (of current template)\".
      | PCLT_SDL_ToParamCompositeLink PCLT_ParamKey
      -- | In input data for catalog formation the given specification
      -- is errornous.
      -- If config's ("Text.PCLT.Config") parameters
      -- 'Text.PCLT.Config.pcsAllowEmptySDL_parseItByModusMargin' and/or
      -- 'Text.PCLT.Config.pcsAllowUnreadableSDL_parseIdByModusMargin' are
      -- positive, then instead of @PCLT_SDL_Errornous@ the parser
      -- ('str2PCLT_SDL') will use 'Text.PCLT.SDL.marginOfSDLModus' to set
      -- valid specification. When representation generator meets
      -- @PCLT_SDL_Errornous@ it won't use template, and return an error.
      | PCLT_SDL_Errornous PCLT_ErrornousSDL
     deriving (Eq, Show, Typeable)
type PCLT_RequiredShowDetalizationLevel  = PCLT_ShowDetalizationLevel
type PCLT_AllocatedShowDetalizationLevel = PCLT_ShowDetalizationLevel

-- | Raw templates (both localizeds, and localizables).
-- Input data for catalog formation. Used by @HasStaticRawPCLTs@ class
-- (declared in "Text.PCLT.HasStaticRawPCLTs")
data PCLT_RawCatalogData = PCLT_RawCatalogData (Map PCLT_ID (Map LanguageName Lazy.ByteString, PCLT_RequiredShowDetalizationLevel)) deriving (Show, Typeable)

-- | This is a bad result of parsing some 'String'
-- into 'PCLT_ShowDetalizationLevel'. The second argument is this bad input.
data PCLT_ErrornousSDL = UnreadableSDL_ESDL SDLModus String deriving (Eq, Show, Typeable)

-- | A constant currently set to 25. It is used in a parser 'str2PCLT_SDL':
-- if the input is errornous, this much symbols of input are saved
-- in 'UnreadableSDL_ESDL'. If input is bigger, then the saved trunc is
-- tailed with \"...\"
__const_esdl_rawinputshowsize_inShowAsPCSI :: Int
__const_esdl_rawinputshowsize_inShowAsPCSI = 25

-- | Parse 'String' into 'PCLT_ShowDetalizationLevel'. First of all parser
-- tries 'Text.PCLT.SDL.strict_str2sdl'. Then, if failed, parser uses following
-- config entries:
--
-- * 'Text.PCLT.Config.pcsParameterPlaceholderWrapper' -
-- if prefix and postfix of input is this (by default it is \"\@\@\|\"),
-- then it is parsed into 'PCLT_SDL_ToParamCompositeLink'
--
-- * 'Text.PCLT.Config.pcsCompositePlaceholderWrapper' -
-- if prefix and postfix of input is this (by default it is \"\#\#\|\"),
-- then it is parsed into 'PCLT_SDL_ToTemplateLink'
--
-- * 'Text.PCLT.Config.pcsAllowEmptySDL_parseItByModusMargin' -
-- if it is positive and input is empty, then it gets parsed into
-- (@PCLT_SDL $ 'Text.PCLT.SDL.marginOfSDLModus' modus@), where modus
-- is first argument; esle, if parameter is negative and input is empty,
-- it is parsed to 'PCLT_SDL_Errornous'
--
-- * 'Text.PCLT.Config.pcsAllowUnreadableSDL_parseIdByModusMargin' -
-- if it is positive and input is unparsable, then it gets parsed into
-- @'PCLT_SDL' $ 'Text.PCLT.SDL.marginOfSDLModus' modus@, where modus
-- is first argument; esle, if parameter is negative and input is unparsable,
-- it is parsed to 'PCLT_SDL_Errornous'
str2PCLT_SDL :: SDLModus -> String -> PCLT_InnerConfig -> PCLT_ShowDetalizationLevel
str2PCLT_SDL sdlm s cfg =
     let cmpst_phw_str = B.unpack $ pcsCompositePlaceholderWrapper cfg
         param_phw_str = B.unpack $ pcsParameterPlaceholderWrapper cfg
         stripOfWrapper subj0 wrp =
                let wrp_len  = length wrp
                    subj1    = drop wrp_len subj0
                    subj1_len = length subj1
                 in take (subj1_len - wrp_len) subj1
      in case strict_str2sdl s of
             Just  n -> PCLT_SDL n
             Nothing ->
                case isPrefixOf cmpst_phw_str s && isSuffixOf cmpst_phw_str s of
                    True  -> PCLT_SDL_ToTemplateLink (stripOfWrapper s cmpst_phw_str)
                    False -> case isPrefixOf param_phw_str s && isSuffixOf param_phw_str s of
                                 True  -> PCLT_SDL_ToParamCompositeLink (stripOfWrapper s param_phw_str)
                                 False ->
                                     let cond1 = pcsAllowEmptySDL_parseItByModusMargin      cfg && null s
                                         cond2 = pcsAllowUnreadableSDL_parseIdByModusMargin cfg
                                         cond = cond1 || cond2
                                      in case cond of
                                             True  -> PCLT_SDL $ marginOfSDLModus sdlm
                                             False -> PCLT_SDL_Errornous $ UnreadableSDL_ESDL sdlm $ truncLiterary s __const_esdl_rawinputshowsize_inShowAsPCSI

---------------------------------------------------------------------------------
-- * Localizable template

data LocalizableTemplate =
         LocalizableTemplate {
                  pcltLocalizationsMap :: Map LanguageName LocalizedTemplate
                -- | If SDL of representation reciever
                -- is less then that, then template cann't be used in representation
                -- generation.
                , pcltRequiredSDL      :: PCLT_RequiredShowDetalizationLevel
         }
     deriving (Show, Typeable)


