{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | Module around 'pcsi2text' function, which generates a message out of 'PCSI' and 'PCLT_Catalog'.
module Text.PCLT.MakeMessage where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.MyHelpers
import Data.Typeable
import Text.ConstraintedLBS
import Text.PCLT.Parser.AdvancedSepBy (SeparatedSectorMarker, MarkedChunkLength)
import Text.PCLT.Catalog
import Text.PCLT.CatalogMaths
import Text.PCLT.CommonTypes
import Text.PCLT.Config
import Text.PCLT.MakeCatalog
import Text.PCLT.PCSI
import Text.PCLT.SDL
import Text.PCLT.Template

-- | Error type for 'pcsi2text'.
data PCSI2Text_Error =
          RequiredCompositeIsMissing_P2TE            RequiredByRequirerCompositeIsMissing_PCLTE
        | RequiredCompositeLoclizationIsMissing_P2TE RequiredByRequirerCompositeIsMissing_PCLTE LanguageName
        | CompositionCycle_P2TE                      CompositionCycle_PCLTE
        | SDL_DeterminationFailure_P2TE              PCLT_ID SDL_DeterminationFailure
        -- | This error is possible only if program is wrong.
        | UnsupportedMarker_P2TE                     SeparatedSectorMarker Lazy.ByteString PCLT_ID LanguageName
        | NoValueForParameter_P2TE                   PCLT_ID LanguageName PCLT_ParamKey
        -- | Not used, reserved for future versions.
        | UnderAccordingParamReparsingFailure_P2TE   PCLT_ID LanguageName PCLT_ParamKey ReadPCSCatalogError
        -- | Not used, reserved for future versions.
        | ReparsingDepthMaxReached_P2TE              PCLT_ID LanguageName PCLT_ParamKey ReparsingDepth
        -- | Not used, reserved for future versions.
        | ReparsingLengthMaxReached_P2TE             PCLT_ID LanguageName PCLT_ParamKey ForInstaniationUsedChunkLength
        -- | No more free space in resulting 'CLBS'.
        | InstaniationLengthMaxReached_P2TE          PCLT_ID LanguageName ForInstaniationUsedChunkLength
      deriving (Show, Typeable)

-- | Error type for 'givenSDL_statisfies'.
data SDL_DeterminationFailure =
          RequiredCompositeIsMissing_SDLDF               RequiredByRequirerCompositeIsMissing_PCLTE
        -- | The SDL of template is specified
        -- (using 'PCLT_SDL_ToParamCompositeLink') to be the same as is
        -- one of a template, that must have been put
        -- under a parameter (using 'PCSI_PV'), but referenced parameter
        -- is missing in given 'PCSI'
        | MissingParam_SDLByParamCompositeLink_SDLDF     PCLT_CompositeKey PCLT_ParamKey
        -- | The SDL of template is specified
        -- (using 'PCLT_SDL_ToParamCompositeLink') to be the same as is
        -- one of a template, that must have been put
        -- under a parameter (using 'PCSI_PV'), but referenced parameter
        -- value is constructed using not 'PCSI_PV'.
        | WrongParamType_SDLByParamCompositeLink_SDLDF   PCLT_CompositeKey PCLT_ParamKey
        -- | The SDL of template is specified
        -- (using 'PCLT_SDL_ToParamCompositeLink') to be the same as is
        -- one of a template, that must have been put
        -- under a parameter (using 'PCSI_PV'), but the 'PCSI' under this
        -- parameter references some unknown (to catalog) template.
        | UnknownComposite_SDLByParamCompositeLink_SDLDF PCLT_CompositeKey PCLT_ParamKey PCLT_CompositeKey
        | SDLReferentialCycle_SDLDF                      PCLT_CompositeKey [PCLT_ID]
        | ErrornousSDL_SDLDF                             PCLT_CompositeKey PCLT_ErrornousSDL
      deriving (Show, Typeable)

-- | Type of 'PCSI2Text_Error'. Stripping arguments.
data PCSI2Text_Error_Type =
          RequiredCompositeIsMissing_P2TET
        | RequiredCompositeLoclizationIsMissing_P2TET
        | CompositionCycle_P2TET
        | SDL_DeterminationFailure_P2TET SDL_DeterminationFailure_Type
        | UnsupportedMarker_P2TET
        | NoValueForParameter_P2TET
        | UnderAccordingParamReparsingFailure_P2TET
        | ReparsingDepthMaxReached_P2TET
        | ReparsingLengthMaxReached_P2TET
        | InstaniationLengthMaxReached_P2TET

-- | Type of 'SDL_DeterminationFailure'. Stripping arguments.
data SDL_DeterminationFailure_Type =
          RequiredCompositeIsMissing_SDLDFT
        | MissingParam_SDLByParamCompositeLink_SDLDFT
        | WrongParamType_SDLByParamCompositeLink_SDLDFT
        | UnknownComposite_SDLByParamCompositeLink_SDLDFT
        | SDLReferentialCycle_SDLDFT
        | ErrornousSDL_SDLDFT

p2teType :: PCSI2Text_Error -> PCSI2Text_Error_Type
p2teType e =
      case e of
          RequiredCompositeIsMissing_P2TE            _       -> RequiredCompositeIsMissing_P2TET
          RequiredCompositeLoclizationIsMissing_P2TE _ _     -> RequiredCompositeLoclizationIsMissing_P2TET
          CompositionCycle_P2TE                      _       -> CompositionCycle_P2TET
          SDL_DeterminationFailure_P2TE              _ sdldf -> SDL_DeterminationFailure_P2TET (sdldfType sdldf)
          UnsupportedMarker_P2TE                     _ _ _ _ -> UnsupportedMarker_P2TET
          NoValueForParameter_P2TE                   _ _ _   -> NoValueForParameter_P2TET
          UnderAccordingParamReparsingFailure_P2TE   _ _ _ _ -> UnderAccordingParamReparsingFailure_P2TET
          ReparsingDepthMaxReached_P2TE              _ _ _ _ -> ReparsingDepthMaxReached_P2TET
          ReparsingLengthMaxReached_P2TE             _ _ _ _ -> ReparsingLengthMaxReached_P2TET
          InstaniationLengthMaxReached_P2TE          _ _ _   -> InstaniationLengthMaxReached_P2TET

sdldfType :: SDL_DeterminationFailure -> SDL_DeterminationFailure_Type
sdldfType e =
      case e of
          RequiredCompositeIsMissing_SDLDF                   _ -> RequiredCompositeIsMissing_SDLDFT
          MissingParam_SDLByParamCompositeLink_SDLDF       _ _ -> MissingParam_SDLByParamCompositeLink_SDLDFT
          WrongParamType_SDLByParamCompositeLink_SDLDF     _ _ -> WrongParamType_SDLByParamCompositeLink_SDLDFT
          UnknownComposite_SDLByParamCompositeLink_SDLDF _ _ _ -> UnknownComposite_SDLByParamCompositeLink_SDLDFT
          SDLReferentialCycle_SDLDF                        _ _ -> SDLReferentialCycle_SDLDFT
          ErrornousSDL_SDLDF                               _ _ -> ErrornousSDL_SDLDFT

-- | Template representation generation errors types abbreviations:
--
-- >       RequiredCompositeIsMissing_P2TET            -> "CM"
-- >       RequiredCompositeLoclizationIsMissing_P2TET -> "CLM"
-- >       CompositionCycle_P2TET                      -> "CC"
-- >       SDL_DeterminationFailure_P2TET       sdldft -> "SF" ++
-- >           case sdldft of
-- >               RequiredCompositeIsMissing_SDLDFT               -> "(CM)"
-- >               MissingParam_SDLByParamCompositeLink_SDLDFT     -> "(LMP)"
-- >               WrongParamType_SDLByParamCompositeLink_SDLDFT   -> "(LWPT)"
-- >               UnknownComposite_SDLByParamCompositeLink_SDLDFT -> "(LUC)"
-- >               SDLReferentialCycle_SDLDFT                      -> "(CC)"
-- >               ErrornousSDL_SDLDFT                             -> "(ES)"
-- >       UnsupportedMarker_P2TET                     -> "UM"
-- >       NoValueForParameter_P2TET                   -> "NV"
-- >       UnderAccordingParamReparsingFailure_P2TET   -> "RF"
-- >       ReparsingDepthMaxReached_P2TET              -> "RDM"
-- >       ReparsingLengthMaxReached_P2TET             -> "RLM"
-- >       InstaniationLengthMaxReached_P2TET          -> "ILM"
shortOf_PCSI2Text_Error :: PCSI2Text_Error -> Lazy.ByteString
shortOf_PCSI2Text_Error e = B.pack $
        case p2teType e of
            RequiredCompositeIsMissing_P2TET            -> "CM"
            RequiredCompositeLoclizationIsMissing_P2TET -> "CLM"
            CompositionCycle_P2TET                      -> "CC"
            SDL_DeterminationFailure_P2TET       sdldft -> "SF" ++
                case sdldft of
                    RequiredCompositeIsMissing_SDLDFT               -> "(CM)"
                    MissingParam_SDLByParamCompositeLink_SDLDFT     -> "(LMP)"
                    WrongParamType_SDLByParamCompositeLink_SDLDFT   -> "(LWPT)"
                    UnknownComposite_SDLByParamCompositeLink_SDLDFT -> "(LUC)"
                    SDLReferentialCycle_SDLDFT                      -> "(CC)"
                    ErrornousSDL_SDLDFT                             -> "(ES)"
            UnsupportedMarker_P2TET                     -> "UM"
            NoValueForParameter_P2TET                   -> "NV"
            UnderAccordingParamReparsingFailure_P2TET   -> "RF"
            ReparsingDepthMaxReached_P2TET              -> "RDM"
            ReparsingLengthMaxReached_P2TET             -> "RLM"
            InstaniationLengthMaxReached_P2TET          -> "ILM"

-- | Whenever representation generator can't make representation
-- for a template due to some error, it puts there (instead of representation)
-- an error marking ('pcsMarkingErrorPlaceholderWrapper') with an abbreviation (see 'shortOf_PCSI2Text_Error')
-- of error type and template ID.
includeAsAnError :: PCLT_InnerConfig -> PCSI2Text_Error -> String -> (Lazy.ByteString, ForInstaniationUsedChunkLength)
includeAsAnError cfg e s = let _err_marker_str = pcsMarkingErrorPlaceholderWrapper cfg
                               r = B.concat
                                        [ _err_marker_str
                                        , shortOf_PCSI2Text_Error e
                                        , B.pack ("->" ++ s)
                                        , _err_marker_str
                                        ]
                            in (r, B.length r)

-- | A test, if a given reciever's detalization level is enough
-- to represent a given 'PCSI'.
givenSDL_statisfies :: ShowDetalizationLevel -> PCSI -> PCLT_ShowDetalizationLevel -> PCLT_CatalogMap -> Either SDL_DeterminationFailure Bool
givenSDL_statisfies det_lev pcsi_0 req_sdl_0 cat_map = _givenSDL_statisfies pcsi_0 req_sdl_0 []
   where
     pcsi_0_id = pcsiTplID pcsi_0
     err31 rer    red =                     RequiredCompositeIsMissing_SDLDF $ RequiredByRequirerCompositeIsMissing_PCLTE rer $ RequiredCompositeIsMissing_PCLTE red
     err32 i_id   p_k =           MissingParam_SDLByParamCompositeLink_SDLDF i_id p_k
     err33 i_id   p_k =         WrongParamType_SDLByParamCompositeLink_SDLDF i_id p_k
     err34 i_id   p_k sc_id = UnknownComposite_SDLByParamCompositeLink_SDLDF i_id p_k sc_id
     err35 i_id   buf =                            SDLReferentialCycle_SDLDF i_id buf
     err36 i_id  esdl =                                   ErrornousSDL_SDLDF i_id esdl
     --------------------------------------------------------------------------------------
     _givenSDL_statisfies :: PCSI -> PCLT_ShowDetalizationLevel -> [PCLT_ID] -> Either SDL_DeterminationFailure Bool
     _givenSDL_statisfies pcsi req_sdl super_buf =
        let pcsi_id = pcsiTplID pcsi
            new_super_buf = pcsi_id : super_buf
         in case pcsi_id `elem` super_buf of
                True -> Left (err35 pcsi_id (pcsi_id : super_buf))
                False ->
                    case req_sdl of
                        PCLT_SDL sdl -> Right (det_lev >= sdl)
                        PCLT_SDL_ToTemplateLink k ->
                           case M.lookup k cat_map of
                               Nothing -> Left $ err31 pcsi_0_id k
                               Just s_pclt -> _givenSDL_statisfies
                                                        (pcsi {pcsiTplID = k} )
                                                        (pcltRequiredSDL s_pclt)
                                                        new_super_buf
                        PCLT_SDL_ToParamCompositeLink param_key ->
                           case M.lookup param_key $ pcsiParamsValsMap pcsi of
                               Nothing -> Left $ err32 pcsi_id param_key
                               Just p_val ->
                                        case p_val of
                                            PCSI_PV s_pcsi ->
                                                let s_pcsi_id = pcsiTplID s_pcsi
                                                 in case s_pcsi_id `elem` new_super_buf of
                                                        True -> Left (err35 s_pcsi_id (s_pcsi_id : new_super_buf))
                                                        False ->
                                                            case M.lookup (pcsiTplID s_pcsi) cat_map of
                                                                Nothing -> Left $ err34 pcsi_id param_key (pcsiTplID s_pcsi)
                                                                Just s_pclt -> givenSDL_statisfies
                                                                                        det_lev
                                                                                        s_pcsi
                                                                                        (pcltRequiredSDL s_pclt)
                                                                                        cat_map
                                            _ -> Left (err33 pcsi_id param_key)
                        PCLT_SDL_Errornous esdl -> Left (err36 pcsi_id esdl)


type SpaceAvailableForPCSIInstaniation = MarkedChunkLength
type ForInstaniationUsedChunkLength    = MarkedChunkLength

-- | Wrapper around 'pcsi2text' for cases, when new 'CLBS' for output
-- is to be created. It's maximal length is set to be same
-- as configured in parameter 'pcsInstaniationResultMaxSize'
pcsi2new_text :: PCSI -> (ShowDetalizationLevel, LanguageName) -> PCLT_Catalog -> (StdOut_CLBS, [ErrorWithPCSCatalog PCSI2Text_Error])
pcsi2new_text _pcsi (det_lev, lng) pcs_catalog = pcsi2text (newCLBS $ catInstMaxLen pcs_catalog) _pcsi (det_lev, lng) pcs_catalog

-- | Make a representation out of 'PCSI' in specified detailization level,
-- in specified language, using specified catalog. And append result
-- to a specified 'CLBS'.
pcsi2text :: StdOut_CLBS -> PCSI -> (ShowDetalizationLevel, LanguageName) -> PCLT_Catalog -> (StdOut_CLBS, [ErrorWithPCSCatalog PCSI2Text_Error])
pcsi2text init_clbs _pcsi (det_lev, lng) pcs_catalog =
                let (clbs, errs_list) = _pcsi2text (pcsiTplID _pcsi) _pcsi gen_cat_map [] (init_clbs, 0)
                    errs_last_add =
                        case clbsFinalized_isit clbs of
                            True  -> [err12]
                            False -> []
                 in ( clbs
                    , map (ErrorWithPCSCatalog (pcltcCatalogID pcs_catalog)) (errs_last_add ++ errs_list)
                    )
        where
           gen_cat_map = pcltcCatalogMap pcs_catalog
           cfg = pcltcInnerConfig pcs_catalog
           _allow_untmpl_msgs           = pcsAllowUntemplatedMessages             cfg
           _allow_lnguntmpl_msgs        = pcsAllowUntemplatedLocalizationsOfMessages cfg
           _show_adhoc_params_emp       = pcsShowAdhocParamsInResultOfUntemplated cfg
           _insuficient_det_lev_str     = pcsInsuficientDetLevelPlaceholder       cfg
           _insuficient_det_lev_str_len = B.length _insuficient_det_lev_str
           _insuficient_det_lev__incl   = (_insuficient_det_lev_str, _insuficient_det_lev_str_len)
           _inst_max_size               = pcsInstaniationResultMaxSize            cfg
           _reparsing_depth_max         = pcsReparsingDepthMax                    cfg
           _reparsable_text_size_max    = pcsReparseParameterContentMaxSize       cfg
           _newline_lbs                 = pcsNewlineLBS                           cfg
           _newline_lbs_len             = B.length _newline_lbs
           _allow_sdl_det_fail          = pcsAllowFailureToDetermineSDL_parseIdByModusMargin cfg
           errMarkStr           err key = includeAsAnError cfg err key
           err0  msg_id buf = CompositionCycle_P2TE $ CompositionCycle_PCLTE msg_id buf
           err1     rer red = RequiredCompositeIsMissing_P2TE            (RequiredByRequirerCompositeIsMissing_PCLTE rer $ RequiredCompositeIsMissing_PCLTE red)
           err2     rer red = RequiredCompositeLoclizationIsMissing_P2TE (RequiredByRequirerCompositeIsMissing_PCLTE rer $ RequiredCompositeIsMissing_PCLTE red) lng
           err3  rer  sdl_e = SDL_DeterminationFailure_P2TE rer sdl_e
           err6 marker t_chunk tpl_id = UnsupportedMarker_P2TE marker t_chunk tpl_id lng
           err8  tpl_id p_k    = NoValueForParameter_P2TE tpl_id lng p_k
           err9  tpl_id p_k re = UnderAccordingParamReparsingFailure_P2TE tpl_id lng p_k re
           err10 tpl_id p_k    = ReparsingDepthMaxReached_P2TE tpl_id lng p_k _reparsing_depth_max
           err11 tpl_id p_k    = ReparsingLengthMaxReached_P2TE tpl_id lng p_k _reparsable_text_size_max
           err12 = InstaniationLengthMaxReached_P2TE (pcsiTplID _pcsi) lng (clbsMaxLen init_clbs)
           --------------------------------
           --------------------------------
           _pcsi2text :: PCLT_ID
                      -> PCSI
                      -> PCLT_CatalogMap
                      -> [PCLT_ID]
                      -> (StdOut_CLBS, ReparsingDepth)
                      -> (StdOut_CLBS, [PCSI2Text_Error])
           _pcsi2text refere_pcsi_id pcsi composites_map super_buf (clbs, reparsing_depth) =
                let pcsi_id  = pcsiTplID pcsi
                    pcsi_pvm = pcsiParamsValsMap pcsi
                    returnJustErr :: PCSI2Text_Error -> String -> (StdOut_CLBS, [PCSI2Text_Error])
                    returnJustErr _err k = (errMarkStr _err k `addToCLBS_1` clbs, [_err])
                    returnInsuficientSDL errs_list = (_insuficient_det_lev__incl `addToCLBS_1` clbs, errs_list)
                    decontexted_msg __req_det_lev =
                            let _req_det_lev = PCLT_SDL __req_det_lev
                                err_of_statisfies_0 = givenSDL_statisfies det_lev pcsi _req_det_lev gen_cat_map
                                (err_of_statisfies_1, errs0) =
                                        case err_of_statisfies_0 of
                                            Right      _ -> (err_of_statisfies_0, [])
                                            Left sdl_err -> ( case _allow_sdl_det_fail of
                                                                  True -> Right (det_lev == InfinitelyBig_SDL)
                                                                  False -> err_of_statisfies_0
                                                            , [err3 pcsi_id sdl_err]
                                                            )
                             in case err_of_statisfies_1 of
                                    Left     sdl_err -> returnJustErr (err3 pcsi_id sdl_err) pcsi_id
                                    Right statisfies ->
                                          case _show_adhoc_params_emp && statisfies of -- && (null super_buf)
                                              False -> returnInsuficientSDL errs0
                                              True  ->
                                                  let _header = B.concat [B.pack pcsi_id, B.pack " {"]
                                                      _header_len = B.length _header
                                                      clbs2 = (_header, _header_len) `addToCLBS_1` clbs
                                                      -- fold_f :: [(CLBS, [PCSI2Text_Error])]
                                                      fold_f = (\ (p_key, p_val) (clbs_accum, errs_accum) ->
                                                                          let (new_clbs_accum, errs_list_addition) =
                                                                                   instaniateParam
                                                                                        p_key
                                                                                        pcsi { pcsiParamsValsMap = M.singleton
                                                                                                  p_key $
                                                                                                  PVList_PV
                                                                                                        [ NewLine_PV
                                                                                                        , PlainText_PV (" @@@ " ++ p_key ++ ": ")
                                                                                                        , Indented_PV 4 p_val
                                                                                                        ]
                                                                                             }
                                                                                        (clbs_accum, undefined)
                                                                           in case clbsFinalized_isit clbs_accum of
                                                                                  True  -> (clbs_accum, errs_accum)
                                                                                  False ->
                                                                                      ( new_clbs_accum
                                                                                      , errs_accum ++ errs_list_addition
                                                                                      )
                                                               )
                                                      (clbs3, errs1) = foldr fold_f (clbs2, errs0) (M.toList pcsi_pvm)
                                                   in ( addToCLBS_1
                                                                (B.pack "}", 1) $
                                                                case M.null pcsi_pvm of
                                                                    False -> (_newline_lbs, _newline_lbs_len) `addToCLBS_1` clbs3
                                                                    True -> clbs3
                                                      , errs1
                                                      )
                 in case pcsi_id `elem` super_buf of
                        True  -> returnJustErr (err0 pcsi_id super_buf) pcsi_id
                        False ->
                            case clbsFinalized_isit clbs of
                                True  -> (clbs, [])
                                False ->
                                    case M.lookup pcsi_id composites_map of
                                        Nothing   ->
                                                case _allow_untmpl_msgs of
                                                    True  -> decontexted_msg InfinitelyBig_SDL
                                                    False -> returnJustErr (err1 refere_pcsi_id pcsi_id) pcsi_id
                                        Just pclt ->
                                                case M.lookup lng (pcltLocalizationsMap pclt) of
                                                    Nothing -> case _allow_lnguntmpl_msgs of
                                                                   True  -> decontexted_msg InfinitelyBig_SDL
                                                                   False -> returnJustErr (err2 refere_pcsi_id pcsi_id) pcsi_id
                                                    Just ldt ->
                                                            let subcomposites = ldtSubcompositesMap ldt
                                                                err_of_statisfies_0 = givenSDL_statisfies det_lev pcsi (pcltRequiredSDL pclt) gen_cat_map
                                                                (err_of_statisfies_1, errs0) =
                                                                        case err_of_statisfies_0 of
                                                                            Right     _ -> (err_of_statisfies_0, [])
                                                                            Left  sdl_e -> ( case _allow_sdl_det_fail of
                                                                                               True  -> Right (det_lev == InfinitelyBig_SDL)
                                                                                               False -> err_of_statisfies_0
                                                                                         , [err3 pcsi_id sdl_e]
                                                                                         )
                                                             in case err_of_statisfies_1 of
                                                                    Left  sdl_e -> returnJustErr (err3 (pcsiTplID pcsi) sdl_e) pcsi_id
                                                                    Right False -> returnInsuficientSDL errs0
                                                                    Right  True ->
                                                                          let foldr_f =
                                                                                (\ (clbs_accum, errs_accum) (ldtm, str, len)  ->
                                                                                        let (new_clbs_accum, errs_add) =
                                                                                                instaniateTplChunk
                                                                                                        subcomposites
                                                                                                        (ldtm, str, len)
                                                                                                        pcsi
                                                                                                        super_buf
                                                                                                        (clbs_accum, reparsing_depth)
                                                                                         in ( new_clbs_accum
                                                                                            , errs_accum ++ errs_add
                                                                                            )
                                                                                )
                                                                           in foldl foldr_f (clbs, errs0) (ldtAbstractedString ldt)
                where
                   -----------------------------------------------------
                   instaniateTplChunk :: PCLT_CatalogMap
                                      -> (PCS_SpecificMarkings, Lazy.ByteString, MarkedChunkLength)
                                      -> PCSI -> [PCLT_ID]
                                      -> (StdOut_CLBS, ReparsingDepth)
                                      -> (StdOut_CLBS, [PCSI2Text_Error])
                   instaniateTplChunk subcomposites (ldtm, lbs, len) pcsi super_buf (clbs, reparsing_depth) =
                        let str = B.unpack lbs
                        in case clbsFinalized_isit clbs of
                               True  -> (clbs, [])
                               False ->
                                   case ldtm of
                                       PlainText_LngTplM -> ((lbs, len) `addToCLBS_1` clbs, [])
                                       Parameter_LngTplM -> let param_key = str
                                                             in instaniateParam param_key pcsi (clbs, reparsing_depth)
                                       Composite_LngTplM -> let sub_pcsi_id = str
                                                                sub_pcsi = pcsi { pcsiTplID = sub_pcsi_id }
                                                             in _pcsi2text (pcsiTplID pcsi) sub_pcsi subcomposites (pcsiTplID pcsi : super_buf) (clbs, reparsing_depth)
                                       Unsupported_LngTplM ssm ->
                                                            let _err = err6 ssm lbs (pcsiTplID pcsi)
                                                             in (errMarkStr _err str `addToCLBS_1` clbs, [_err])
                   ------------------------------------------------------
                   instaniateParam :: PCLT_ParamKey
                                   -> PCSI
                                   -> (StdOut_CLBS, ReparsingDepth)
                                   -> (StdOut_CLBS, [PCSI2Text_Error])
                   instaniateParam param_key pcsi (clbs, reparsing_depth) =
                               let pcsi_pvm = pcsiParamsValsMap pcsi
                                   pclt_id  = pcsiTplID pcsi
                                   referer_pcsi_id = pclt_id ++ "<p: " ++ param_key ++ ">"
                                in case M.lookup param_key pcsi_pvm of
                                       Nothing        -> let _err = err8 pclt_id param_key
                                                          in (errMarkStr _err param_key `addToCLBS_1` clbs, [_err])
                                       Just _pcsi_ipv ->
                                           let _processParamVal :: PCLT_ParamVal -> CLBS -> (CLBS, [PCSI2Text_Error])
                                               _processParamVal pcsi_ipv sub_clbs =
                                                        case clbsFinalized_isit sub_clbs of
                                                            True  -> (sub_clbs, [])
                                                            False -> _processParamVal_sub
                                                  where
                                                    _processParamVal_sub :: (CLBS, [PCSI2Text_Error])
                                                    _processParamVal_sub = case pcsi_ipv of
                                                       Nothing_PV              -> (sub_clbs, [])
                                                       NewLine_PV              -> ((_newline_lbs, _newline_lbs_len) `addToCLBS_1` sub_clbs, [])
                                                       Indented_PV i pcsi_iipv -> let fr_clbs = freeSpaceCLBS sub_clbs
                                                                                      (notind_clbs, _errs_list) = _processParamVal pcsi_iipv fr_clbs
                                                                                      ind_clbs = insertInsteadOf_inCLBS (_newline_lbs, B.concat [_newline_lbs, B.replicate (fromIntegral i) ' ']) notind_clbs
                                                                                   in (ind_clbs `addToCLBS_2` sub_clbs, _errs_list)
                    {- this is an experimental part    Reparsable_PV rp_pv rp_params ->
                       curently doesn't work               let dest_clbs = newCLBS _reparsable_text_size_max
                                                                (clbs_for_reparse, _errs_list_0) =
                                                                        _processParamVal
                                                                                rp_pv
                                                                                dest_clbs -- in newer version must get rid of this workaround by exploiting laziness at it's 100%... for that will need to get rid of all lengths here.
                                                                _errs_list_1 =
                                                                      (++)
                                                                        _errs_list_0
                                                                        (case clbsFinalized_isit clbs_for_reparse of
                                                                             True  -> [err11 (pcsiTplID pcsi) param_key]
                                                                             False -> []
                                                                        )
                                                                reparse_willwe = reparsing_depth < _reparsing_depth_max
                                                                (add_to_sub_clbs, add_errs_list) =
                                                                        case reparse_willwe of
                                                                            False -> (clbs_for_reparse, _errs_list_1 ++ [err10 pclt_id param_key])
                                                                            True  ->
                                                                                let rp_pclt_id = "<<REPARSE:" ++ pclt_id ++ "(" ++ param_key ++ ")>>"
                                                                                    err_or_pclt = str2pclt
                                                                                                        ( rp_pclt_id
                                                                                                        , PCLT_SDL Zero_SDL
                                                                                                        )
                                                                                                        (lng, clbsLBS clbs_for_reparse)
                                                                                                        pcs_catalog
                                                                                 in case err_or_pclt of
                                                                                        Left   ar_err -> let (ErrorWithPCSCatalog _ re) = ar_err
                                                                                                          in (clbs_for_reparse, _errs_list_1 ++ [err9 pclt_id param_key re])
                                                                                        Right re_pclt ->
                                                                                             let (result_clbs, _errs_list_2) =
                                                                                                        _pcsi2text
                                                                                                                (referer_pcsi_id ++ "<r>")
                                                                                                                (thePCSI rp_pclt_id (M.toList rp_params))
                                                                                                                (M.insert rp_pclt_id re_pclt gen_cat_map)
                                                                                                                []
                                                                                                                (sub_clbs, reparsing_depth + 1)
                                                                                              in (result_clbs, _errs_list_1 ++ _errs_list_2)
                                                              in (add_to_sub_clbs `addToCLBS_2` sub_clbs, add_errs_list) -}
                                                       PlainText_PV     v -> let v_lbs = B.pack v in ((v_lbs, B.length v_lbs) `addToCLBS_1` sub_clbs, [])
                                                       PlainTextLBS_PV  v -> ((v, B.length v) `addToCLBS_1` sub_clbs, [])
                                                       PCSI_PV    pcsi_pv -> _pcsi2text referer_pcsi_id pcsi_pv gen_cat_map [] (sub_clbs, reparsing_depth)
                                                       PCSIList_PV pcsi_l separator_pv ->
                                                                let (separator_clbs, errs_list_0) = _processParamVal separator_pv (freeSpaceCLBS sub_clbs)
                                                                    (result_clbs, errs_list, _) =
                                                                        foldl
                                                                            (\ (clbs_accum0, errs_accum, i) pcsi_fl ->
                                                                                   case clbsFinalized_isit clbs_accum0 of
                                                                                       True  -> (clbs_accum0, errs_accum, i + 1)
                                                                                       False -> let clbs_accum2 =
                                                                                                        case i > 1 of
                                                                                                            True  -> separator_clbs `addToCLBS_2` clbs_accum0
                                                                                                            False -> clbs_accum0
                                                                                                    pcsi_fl2 = pcsi_fl {
                                                                                                                pcsiParamsValsMap =
                                                                                                                    sumPCSI_PVMs
                                                                                                                        (pcsiParamsValsMap pcsi_fl)
                                                                                                                        (M.fromList [("__row_idx", PlainText_PV $ show i)])
                                                                                                                }
                                                                                                    (clbs_accum3, errs_add) = _processParamVal (PCSI_PV pcsi_fl2) clbs_accum2
                                                                                                 in (clbs_accum3, errs_accum ++ errs_add, i + 1)
                                                                            )
                                                                            (sub_clbs, errs_list_0, 1)
                                                                            pcsi_l
                                                                 in (result_clbs, errs_list)
                                                       PVList_PV pv_list ->
                                                                let (result_clbs, errs_list, _) =
                                                                        foldl
                                                                            (\ (clbs_accum, errs_accum, i) pv_fl ->
                                                                                   case clbsFinalized_isit clbs_accum of
                                                                                       True  -> (clbs_accum, errs_accum, i+1)
                                                                                       False -> let (clbs_new_accum, errs_add) = _processParamVal pv_fl clbs_accum
                                                                                                 in (clbs_new_accum, errs_accum ++ errs_add, i + 1)
                                                                            )
                                                                            (sub_clbs, [], 1)
                                                                            pv_list
                                                                 in (result_clbs, errs_list)

                                            ----------------------------
                                         in _processParamVal _pcsi_ipv clbs
           ------------------------------------------------------