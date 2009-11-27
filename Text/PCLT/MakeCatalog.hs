{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

-- | A module around comlex function '_readPCLTCatalog'.
module Text.PCLT.MakeCatalog where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe
import Data.MyHelpers
import Data.Typeable
import Text.PCLT.Catalog
import Text.PCLT.CommonTypes
import Text.PCLT.Config
import Text.PCLT.SDL
import Text.PCLT.Template

type Parsed_DidWe = Bool
-- | Error type for '_readPCLTCatalog'.
--
-- Descriptios for some value constructors:
--
-- * 'DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_RPCSCE':
-- For more information about this error see
-- 'Text.PCLT.Config.StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets'
--
-- * 'DifferentSDLs_RPCSCE':
-- This may occur in case when some 'PCLT_RawCatalogData' is
-- added to a nonempty 'PCLT_Catalog', and same templates in both
-- places has different SDL requirements.
--
-- * 'TplUniquenessViol_RPCSCE':
-- This may occur in case when some 'PCLT_RawCatalogData' is
-- added to a nonempty 'PCLT_Catalog', and there isan intersection
-- between their \{(Template ID, Language)\} sets.
--
-- * 'SDL_ToCompositeLinkRefsToNonexistent_RPCSCE':
-- SDL requirement specification
-- ('Text.PCLT.Template.pcltRequiredSDL') for a template is of type
-- 'Text.PCLT.Template.PCLT_SDL_ToTemplateLink' and references to
-- a template which is not persistant in catalog or raw input data.
data ReadPCSCatalogError =
          CompositionCycle_RPCSCE              CompositionCycle_PCLTE
        | RequiredCompositeIsMissing_RPCSCE    RequiredByRequirerCompositeIsMissing_PCLTE
        | RequiredCompositeIsUnparsable_RPCSCE RequirerCompositeKey RequiredCompositeKey
        | ParseFailedForDefaultLng_RPCSCE      PCLT_CompositeKey Parsed_DidWe [PCLT_ParserLowLevelFailure]
        | TplDefaultLngIsMissing_RPCSCE        TplDefaultLngIsMissing_PCLTE
        | ParseFailedForNondefaultLng_RPCSCE   PCLT_CompositeKey Parsed_DidWe LanguageName [PCLT_ParserLowLevelFailure]
        | DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_RPCSCE
                                               DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_PCLTE
        | DifferentSDLs_RPCSCE                 DifferentSDLs_PCLTE
        | TplUniquenessViol_RPCSCE             TplUniquenessViol_PCLTE
        | SDL_ToCompositeLinkRefsToNonexistent_RPCSCE
                                               RequirerCompositeKey RequiredCompositeKey
        | DRL_NormalizationError_RPCSCE        DRL_NormalizationError
       deriving (Show, Typeable)

-- | Wrapper around '_readPCLTCatalog'. To be used when starting with an empty catalog.
readPCLTCatalog :: PCLT_InnerConfig -> PCLT_CatalogID -> PCLT_RawCatalogData -> (PCLT_Catalog, [ErrorWithPCSCatalog ReadPCSCatalogError])
readPCLTCatalog pcsc_config cat_id raw_input =
        let init_cat = PCLT_Catalog {
                  pcltcCatalogID   = cat_id
                , pcltcCatalogMap  = M.empty
                , pcltcInnerConfig = pcsc_config
                }
         in _readPCLTCatalog (init_cat, []) raw_input

-- | Take a bunch of raw templates, parse them into normal templates and add
-- them to a catalog.
_readPCLTCatalog :: (PCLT_Catalog, [ReadPCSCatalogError]) -> PCLT_RawCatalogData -> (PCLT_Catalog, [ErrorWithPCSCatalog ReadPCSCatalogError])
_readPCLTCatalog (init_cat, init_errs) (PCLT_RawCatalogData messages_raws_map1) = -- traceShowPaged 5 ("**", M.filterWithKey (\ k _ -> k == "HW" || k == "E_PCLT_P2TE" ) messages_raws_map1) $
        let (messages_raws_map2, pclt_catalog_map1, err_msgs1) =
                                              filterPrepareRawInputData (pcltcCatalogMap init_cat, init_errs) messages_raws_map1
            (pclt_catalog_map2 , err_msgs2) = processToPCLT_Catalog     (pclt_catalog_map1       , err_msgs1) messages_raws_map2
            (pclt_catalog_map3 , err_msgs3) = normalizeReferentialSDLs   pclt_catalog_map2
            err_msgs = map (ErrorWithPCSCatalog cat_id)
                           (err_msgs2 ++ map DRL_NormalizationError_RPCSCE err_msgs3)
         in ( init_cat { pcltcCatalogMap = pclt_catalog_map3 }, err_msgs )
         where
            cat_id = pcltcCatalogID init_cat
            pcsc_config = pcltcInnerConfig init_cat
            default_lng = pcsDefaultLanguage pcsc_config
            so = pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets pcsc_config
            err6 msg_id buf            = CompositionCycle_RPCSCE $ CompositionCycle_PCLTE
                                                                              msg_id buf
            err7 msg_id subcomp_id     = RequiredCompositeIsMissing_RPCSCE $ RequiredByRequirerCompositeIsMissing_PCLTE msg_id $ RequiredCompositeIsMissing_PCLTE subcomp_id
            err8 msg_id subcomp_id     = RequiredCompositeIsUnparsable_RPCSCE msg_id subcomp_id
            err12 msg_id pdw errs_list = ParseFailedForDefaultLng_RPCSCE      msg_id pdw errs_list
            err13 msg_id               = TplDefaultLngIsMissing_RPCSCE $ TplDefaultLngIsMissing_PCLTE
                                                                              msg_id
            err14 msg_id pdw lng errs_list
                                       = ParseFailedForNondefaultLng_RPCSCE   msg_id pdw lng errs_list
            err15 msg_id lng           = DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_RPCSCE $ DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_PCLTE
                                                                              msg_id lng
            err16 msg_id (msdl, asdl)  = DifferentSDLs_RPCSCE     $ DifferentSDLs_PCLTE msg_id (msdl, asdl)
            err17 msg_id lng_list      = TplUniquenessViol_RPCSCE $ TplUniquenessViol_PCLTE msg_id lng_list
            err20 msg_id subcomp_id    = SDL_ToCompositeLinkRefsToNonexistent_RPCSCE
                                                                              msg_id subcomp_id
            -----------------------------------------------------------
            filterPrepareRawInputData :: ( PCLT_CatalogMap, [ReadPCSCatalogError] )
                                      ->   Map PCLT_CompositeKey (Map LanguageName Lazy.ByteString, PCLT_RequiredShowDetalizationLevel)
                                      -> ( Map PCLT_CompositeKey (Map LanguageName (Either LocalizedTemplate Lazy.ByteString), PCLT_RequiredShowDetalizationLevel)
                                         , Map PCLT_CompositeKey LocalizableTemplate -- PCLT_CatalogMap
                                         , [ReadPCSCatalogError]
                                         )
            filterPrepareRawInputData (processed_msgs_accum_map1, errs_accum) raw_msgs_src_map =
                     let makeInputEntry :: PCLT_ID
                                        -> ( Maybe LocalizableTemplate
                                           , Maybe (Map LanguageName Lazy.ByteString, PCLT_RequiredShowDetalizationLevel))
                                        -> ( Map LanguageName (Either LocalizedTemplate Lazy.ByteString)
                                           , PCLT_RequiredShowDetalizationLevel
                                           , [ReadPCSCatalogError]
                                           )
                         makeInputEntry k (mb_pclt, mb_mapByLng) =
                             let unionWithKey_3_ERROR = error "Severe internal error (in _readPCLTCatalog) unionWithKey_3(4) can't give (Nothing, Nothing) to the unifier !!!"
                              in case (mb_pclt, mb_mapByLng) of
                                    (Nothing  , Nothing)               -> unionWithKey_3_ERROR
                                    (Nothing  , Just (mapByLng, sdl))  -> (M.map Right mapByLng, sdl, [])
                                 -- (Just pclt, Nothing) case processed by the callerof makeInputEntry
                                    (Just pclt, Just (mapByLng, sdl2)) ->
                                        let sdl1 = pcltRequiredSDL pclt
                                            (result_mapByLng, uniqviol_lng_list) = unionWithKey_3
                                                (\ lng (mb_ldt, mb_rawInput) -> -- unifierF
                                                        case (mb_ldt, mb_rawInput) of
                                                            (Nothing  , Nothing)       -> unionWithKey_3_ERROR
                                                            (Nothing  , Just rawInput) -> (Right rawInput, [])
                                                            (Just ldt, Nothing)       -> (Left  ldt    , [])
                                                            (Just ldt, Just rawInput) -> (Left  ldt    , [lng])
                                                )
                                                (++) -- sideResultAccumF
                                                []
                                                (pcltLocalizationsMap pclt)
                                                mapByLng
                                            errs_add =
                                                   (case sdl1 == sdl2      of { True -> []; False -> [err16 k (sdl1, sdl2)] })
                                                ++ (case uniqviol_lng_list of { []   -> []; l     -> [err17 k l           ] })
                                         in (result_mapByLng, sdl1, errs_add)
                         (raw_msgs_src_map2, (processed_msgs_accum_map2, errs_accum2)) = unionWithKey_4
                                (\ k (mb_pclt, mb_mapByLng) ->
                                        case (mb_pclt, mb_mapByLng) of
                                            (Just pclt, Nothing) -> (Nothing, (M.singleton k pclt, []))
                                            _ -> let (      result_map, sdl ,           errs_add ) = makeInputEntry k (mb_pclt, mb_mapByLng)
                                                  in (Just (result_map, sdl), (M.empty, errs_add))
                                )
                                (apFor2ple (M.union, (++))) -- sideResultAccumF
                                (M.empty, errs_accum)
                                processed_msgs_accum_map1
                                raw_msgs_src_map
                      in (raw_msgs_src_map2, processed_msgs_accum_map2, errs_accum2)
            -----------------------------------------------------------
            processToPCLT_Catalog :: (PCLT_CatalogMap, [ReadPCSCatalogError])
                                  -> (Map PCLT_CompositeKey (Map LanguageName (Either LocalizedTemplate Lazy.ByteString), PCLT_RequiredShowDetalizationLevel))
                                  -> (PCLT_CatalogMap, [ReadPCSCatalogError])
            processToPCLT_Catalog (processed_msgs_accum_map, errs_accum) halfRaw_input =
                        case M.null halfRaw_input of
                            True  -> (processed_msgs_accum_map, errs_accum)
                            False -> let ((k, (msg_raw_lng_map, sdl)), new_raw_msgs_src_map) = M.deleteFindMin halfRaw_input
                                      in uncurry
                                            processToPCLT_Catalog
                                                (processToPCLT_Catalog_Unit
                                                        (processed_msgs_accum_map, errs_accum)
                                                        (k, msg_raw_lng_map, sdl)
                                                        []
                                                        new_raw_msgs_src_map
                                                )
            -----------------------------------------------------------
            parseMsgTemplates :: PCLT_CompositeKey
                              -> Map LanguageName (Either LocalizedTemplate Lazy.ByteString)
                              -> ( [ReadPCSCatalogError]
                                 , Map LanguageName
                                       (Either
                                           LocalizedTemplate
                                           ( LngTpl_AbstractedString
                                           , [PCLT_CompositeKey]
                                 )     )   )
            parseMsgTemplates k raw_msgs_by_lngs_map = -- traceCond (k == "HW" || k == "E_PCLT_P2TE") 5 ("**", raw_msgs_by_lngs_map) $
                case takeFromMap default_lng raw_msgs_by_lngs_map of
                    (Nothing, _) -> ([err13 k], M.empty)
                    (Just default_ldt_or_raw, rest_raw_msgs_by_lngs_map) ->
                        let (mb_default_ldt_or_freshParse, errs_list1, default_list_of_composites, default_list_of_params) =
                                case default_ldt_or_raw of
                                    Left             ldt -> (Just $ Left ldt, [], M.keys $ ldtSubcompositesMap ldt, listOfParams $ ldtAbstractedString ldt)
                                    Right default_raw_msg ->
                                        case doTheParse pcsc_config default_raw_msg of
                                            (errs_list1, Nothing) -> (Nothing, [err12 k False errs_list1], [], [])
                                            (errs_list1, Just default_freshParse@(dflt_str_struct, dflt_list_of_composites)) ->
                                                        ( Just $ Right default_freshParse
                                                        , case null errs_list1 of {True  -> []; False -> [err12 k True errs_list1]}
                                                        , dflt_list_of_composites
                                                        , listOfParams dflt_str_struct
                                                        )
                         in case mb_default_ldt_or_freshParse of
                                Nothing -> (errs_list1, M.empty)
                                Just default_ldt_or_freshParse -> foldr
                                          (\ (lng, ldt_or_raw) (errs_accum, parseds_accum_map) ->
                                                case ldt_or_raw of
                                                    Left ldt -> (errs_accum, M.insert lng (Left ldt) parseds_accum_map)
                                                    Right unparsed_tpl ->
                                                        case doTheParse pcsc_config unparsed_tpl of
                                                            (errs_list2, Nothing) -> (err14 k False lng errs_list2 : errs_accum, parseds_accum_map)
                                                            (errs_list2, Just freshParse@(str_struct, list_of_composites)) ->
                                                               let orients_doesit = _compareStrictOrientationOnDefault k so (default_list_of_composites, default_list_of_params) (list_of_composites, listOfParams str_struct)
                                                                in case orients_doesit of
                                                                       False -> (err15 k lng : errs_accum, parseds_accum_map)
                                                                       True  -> ( case null errs_list2 of
                                                                                      False -> err14 k True lng errs_list2 : errs_accum
                                                                                      True  -> errs_accum
                                                                                , M.insert lng (Right freshParse) parseds_accum_map
                                                                                )
                                          )
                                          (errs_list1, M.singleton default_lng default_ldt_or_freshParse)
                                          (M.toList rest_raw_msgs_by_lngs_map)
            -----------------------------------------------------------
            processToPCLT_Catalog_Unit :: (PCLT_CatalogMap, [ReadPCSCatalogError])
                                      -> (PCLT_CompositeKey, Map LanguageName (Either LocalizedTemplate Lazy.ByteString), PCLT_RequiredShowDetalizationLevel)
                                      -> [PCLT_CompositeKey]
                                      -> (Map PCLT_CompositeKey (Map LanguageName (Either LocalizedTemplate Lazy.ByteString), PCLT_RequiredShowDetalizationLevel))
                                      -> ( (PCLT_CatalogMap, [ReadPCSCatalogError])
                                         , (Map PCLT_CompositeKey (Map LanguageName (Either LocalizedTemplate Lazy.ByteString), PCLT_RequiredShowDetalizationLevel))
                                         )
            processToPCLT_Catalog_Unit (processed_msgs_accum_map, errs_accum) (k, msg_raw_or_ldt_mapByLng, sdl) superprocessed_buffer raw_msgs_src_map =
                let returnWithError err_str = ((processed_msgs_accum_map, err_str : errs_accum), raw_msgs_src_map) in
                case k `elem` superprocessed_buffer of
                    True  -> returnWithError (err6 k superprocessed_buffer)
                    False ->
                        let (errs_list, ldt_or_froshParseds_map) = parseMsgTemplates k msg_raw_or_ldt_mapByLng
                            (ldt_mapByLng1, absStr_mapByLng) = partition_2 isLeft (fromLeft, fromRight) ldt_or_froshParseds_map -- agree... not a best style
                            errs_accum2 = errs_accum ++ errs_list
                            (ldt_mapByLng2, (processed_msgs_accum_map2, raw_msgs_src_map2), errs_accum6) =
                                foldl
                                    (\ (ldt_mapByLng_accum, (_processed_msgs_accum_map1, _raw_msgs_src_map1), errs_accum3) (lng, (msg_chunks_struct, list_of_composites_keys)) ->
                                        let (msg_composites, (_processed_msgs_accum_map2, _raw_msgs_src_map2), errs_accum4) =
                                                foldl
                                                    gatherCatalogUnitSubcomposites
                                                    (M.empty, (_processed_msgs_accum_map1, _raw_msgs_src_map1), errs_accum3)
                                                    list_of_composites_keys
                                            ldt = LocalizedTemplate {
                                                                  ldtAbstractedString  = msg_chunks_struct
                                                                , ldtSubcompositesMap  = msg_composites
                                                         }
                                         in ( M.insert lng ldt ldt_mapByLng_accum
                                            , ( _processed_msgs_accum_map2
                                              , _raw_msgs_src_map2
                                              )
                                            , errs_accum4
                                            )
                                    )
                                    (ldt_mapByLng1, (processed_msgs_accum_map, raw_msgs_src_map), errs_accum2)
                                    (M.toList absStr_mapByLng)
                            pclt = LocalizableTemplate {
                                        pcltRequiredSDL = sdl
                                      , pcltLocalizationsMap = ldt_mapByLng2
                                  }
                         in ((M.insert k pclt processed_msgs_accum_map2, errs_accum6), raw_msgs_src_map2)
              -----------------------------------------------------------
              where
                gatherCatalogUnitSubcomposites :: ( LngTpl_SubCompositesMap
                                                  , ( Map PCLT_ID LocalizableTemplate
                                                    , Map PCLT_CompositeKey (Map LanguageName (Either LocalizedTemplate Lazy.ByteString), PCLT_RequiredShowDetalizationLevel)
                                                    )
                                                  , [ReadPCSCatalogError]
                                                  )
                                               -> PCLT_CompositeKey
                                               -> ( LngTpl_SubCompositesMap
                                                  , ( Map PCLT_ID LocalizableTemplate
                                                    , Map PCLT_CompositeKey (Map LanguageName (Either LocalizedTemplate Lazy.ByteString), PCLT_RequiredShowDetalizationLevel)
                                                    )
                                                  , [ReadPCSCatalogError]
                                                  )
                gatherCatalogUnitSubcomposites (msg_composites_accum, (processed_msgs_accum_map, raw_msgs_src_map), errs_accum) composite_key =
                    let returnWithError str = (msg_composites_accum, (processed_msgs_accum_map, raw_msgs_src_map), str : errs_accum)
                     in case M.lookup composite_key processed_msgs_accum_map of
                                Just pcs -> (M.insert composite_key pcs msg_composites_accum, (processed_msgs_accum_map, raw_msgs_src_map), errs_accum)
                                Nothing  ->
                                   case takeFromMap composite_key raw_msgs_src_map of
                                       (Nothing, _) -> case k == composite_key of
                                                           False -> case composite_key `elem` superprocessed_buffer of
                                                                        False -> returnWithError (err7 k composite_key)
                                                                        True  -> returnWithError (err6 composite_key (composite_key:k:superprocessed_buffer))
                                                           True  -> returnWithError (err6 k [k])
                                       (Just (raw_subcomp_msgs_map_by_lng, sub_sdl), new_raw_msgs_src_map) ->
                                               let ((new_processed_msgs_accum_map, new_errs_accum), new_new_raw_msgs_src_map) =
                                                               processToPCLT_Catalog_Unit
                                                                        (processed_msgs_accum_map, errs_accum)
                                                                        (composite_key, raw_subcomp_msgs_map_by_lng, sub_sdl)
                                                                        (k : superprocessed_buffer)
                                                                        new_raw_msgs_src_map
                                                   returnWithError3 err = (msg_composites_accum, (new_processed_msgs_accum_map, new_new_raw_msgs_src_map), err : new_errs_accum)
                                                in case M.lookup composite_key new_processed_msgs_accum_map of
                                                       Nothing  -> returnWithError3 (err8 k composite_key)
                                                       Just pcs -> (M.insert composite_key pcs msg_composites_accum, (new_processed_msgs_accum_map, new_new_raw_msgs_src_map), new_errs_accum)
                --------------------------------------------------------------------
-- | Error type for 'normalizeReferentialSDLs'.
data DRL_NormalizationError =
          SDL_ToCompositeLinksCycle_DRLNE      [PCLT_CompositeKey]
        | SDL_DetFail_ToCompositeLinkRefsToNonexistent_DRLNE
                                               RequirerCompositeKey RequiredCompositeKey
       deriving (Show, Typeable)

-- | Under \"Referential SDLs normalization\" here is understood the following.
-- Now consider paths like:
--
-- @ Tpl_1.'pcltRequiredSDL' -> Tpl_2.'pcltRequiredSDL'@
--
-- which means 'pcltRequiredSDL' of @Tpl_1@ is specified (using
-- 'PCLT_SDL_ToTemplateLink') to be the same as for @Tpl_2@. Here we can
-- reduce the 'PCLT_SDL_ToTemplateLink' of @Tpl_1@ by assinging instead of it
-- @Tpl_2.@'pcltRequiredSDL' value. Data consistency is respected. We win in
-- speed of @Tpl_1.@'pcltRequiredSDL' determination. The negative side of this,
-- is that we can't change @Tpl_2.@'pcltRequiredSDL' anymore without spoiling
-- data consistency. That's another reason why a PCLT catalog is so hard to
-- modify. Perhaps the problem will be solved in future versions of PCLT.
--
-- So this function reduces all 'PCLT_SDL_ToTemplateLink's in a given
-- 'PCLT_CatalogMap', BUT, only where they lead to 'PCLT_SDL's or
-- to other 'PCLT_SDL_ToTemplateLink's (which recursively gets considered
-- to be reduced), not when it leads to 'PCLT_SDL_ToParamCompositeLink'
-- or 'PCLT_SDL_Errornous'.
normalizeReferentialSDLs :: PCLT_CatalogMap -> (PCLT_CatalogMap, [DRL_NormalizationError])
normalizeReferentialSDLs pclt_catalog_map =
        let err21 buf           = SDL_ToCompositeLinksCycle_DRLNE             buf
            err23 rer_id red_id = SDL_DetFail_ToCompositeLinkRefsToNonexistent_DRLNE
                                                                              rer_id red_id
            determinePCLT_SDL :: (PCLT_ID, LocalizableTemplate)
                              -> (PCLT_CatalogMap, PCLT_CatalogMap, [DRL_NormalizationError], [PCLT_CompositeKey])
                              -> (PCLT_CatalogMap, PCLT_CatalogMap, [DRL_NormalizationError], PCLT_RequiredShowDetalizationLevel)
            determinePCLT_SDL (pclt_id, pclt) (processed_cat_accum, unprocessed_cat_map, err_msg_accum, useds_buf) =
                case pclt_id `elem` useds_buf of
                    True  ->
                            ( M.insert pclt_id pclt processed_cat_accum
                            , unprocessed_cat_map
                            , err_msg_accum ++ [err21 (pclt_id : useds_buf)]
                            , pcltRequiredSDL pclt
                            )
                    False ->
                        case pcltRequiredSDL pclt of
                            PCLT_SDL_ToTemplateLink ref_pclt_id ->
                                case M.lookup ref_pclt_id processed_cat_accum of
                                    Just refed_pclt ->
                                        let sdl = pcltRequiredSDL refed_pclt
                                            upd_sdl = case sdl of
                                                          PCLT_SDL                _ -> sdl
                                                          PCLT_SDL_ToTemplateLink _ -> sdl
                                                          _ -> pcltRequiredSDL pclt
                                            upd_pclt = pclt {pcltRequiredSDL = upd_sdl}
                                         in ( M.insert pclt_id upd_pclt processed_cat_accum
                                            , unprocessed_cat_map
                                            , err_msg_accum
                                            , upd_sdl
                                            )
                                    Nothing ->
                                        case takeFromMap ref_pclt_id unprocessed_cat_map of
                                            (Just unprocessed_refed_pclt, rest_unprocesseds) ->
                                                let (processed_cat_accum2, unprocessed_cat_map2, err_msg_accum2, sdl) = determinePCLT_SDL (ref_pclt_id, unprocessed_refed_pclt) (processed_cat_accum, rest_unprocesseds, err_msg_accum, pclt_id : useds_buf)
                                                    upd_sdl = case sdl of
                                                                  PCLT_SDL                _ -> sdl
                                                                  PCLT_SDL_ToTemplateLink _ -> sdl
                                                                  _ -> pcltRequiredSDL pclt
                                                    upd_pclt = pclt {pcltRequiredSDL = upd_sdl}
                                                 in ( M.insert pclt_id upd_pclt processed_cat_accum2
                                                    , unprocessed_cat_map2
                                                    , err_msg_accum2
                                                    , upd_sdl
                                                    )
                                            (Nothing, _) ->
                                                    ( M.insert pclt_id pclt processed_cat_accum
                                                    , unprocessed_cat_map
                                                    , err_msg_accum ++ [err23 pclt_id ref_pclt_id]
                                                    , pcltRequiredSDL pclt
                                                    )
                            _ -> ( M.insert pclt_id pclt processed_cat_accum
                                 , unprocessed_cat_map
                                 , err_msg_accum
                                 , pcltRequiredSDL pclt
                                 )
            ------------------------------------------------------
            normalizeReferentialSDLs_metaCycle :: (PCLT_CatalogMap, PCLT_CatalogMap, [DRL_NormalizationError])
                                                -> (PCLT_CatalogMap, PCLT_CatalogMap, [DRL_NormalizationError])
            normalizeReferentialSDLs_metaCycle i@(processeds, unprocesseds, errs_accum) =
                case M.null unprocesseds of
                    True -> i
                    False -> let ((pclt_id, pclt), rest_unprocesseds) = M.deleteFindMin unprocesseds
                                 (processeds2, unprocesseds2, errs_accum2, _) = determinePCLT_SDL (pclt_id, pclt) (processeds, rest_unprocesseds, errs_accum, [])
                              in normalizeReferentialSDLs_metaCycle (processeds2, unprocesseds2, errs_accum2)
            ------------------------------------------------------
            (results, _, result_errs_list) = normalizeReferentialSDLs_metaCycle (pclt_catalog_map, M.empty, [])
         in (results, result_errs_list)

------------------------------------------------------------------------
-- | Error type for 'adhoc_str2ldt'.
data AHSTR2LngTpl_Error =
          ParseFailure_AHS2PE               [PCLT_ParserLowLevelFailure]
        | RequiredCompositeIsMissing_AHS2PE RequiredCompositeIsMissing_PCLTE
       deriving (Show, Typeable)

-- | Make a 'Text.PCLT.Template.LocalizedTemplate'
-- out of a single 'ByteString'.
adhoc_str2ldt :: (Lazy.ByteString, LanguageName) -> PCLT_Catalog -> Either AHSTR2LngTpl_Error LocalizedTemplate
adhoc_str2ldt (str, lng) catalog =
        let cfg                = pcltcInnerConfig catalog
            (err_list, mb_pre_ldt) = doTheParse cfg str -- ( [PCLT_ParserLowLevelFailure], Maybe ( LngTpl_AbstractedString, [PCLT_ID] ))
            _allow_untmpl_msgs = pcsAllowUntemplatedMessages cfg
            err1    subcomp_id = RequiredCompositeIsMissing_AHS2PE $ RequiredCompositeIsMissing_PCLTE subcomp_id
         in case mb_pre_ldt of
                Nothing -> Left $ ParseFailure_AHS2PE err_list
                Just (marked_str, composites_keys_list) ->
                        let err_or_subcomposites_map =
                                foldr (\ subc_id err_or_pclt_accum ->
                                            case err_or_pclt_accum of
                                                Left _ -> err_or_pclt_accum
                                                Right pclt_accum ->
                                                    case M.lookup subc_id (pcltcCatalogMap catalog) of
                                                        Nothing ->
                                                                case _allow_untmpl_msgs of
                                                                    True -> err_or_pclt_accum
                                                                    False -> Left $ err1 subc_id
                                                        Just pclt -> Right $ M.insert subc_id pclt pclt_accum
                                      )
                                      (Right M.empty)
                                      composites_keys_list
                         in case err_or_subcomposites_map of
                                Left err_msg -> Left err_msg
                                Right sub_lat_map -> Right LocalizedTemplate {
                                                        ldtAbstractedString = marked_str
                                                      , ldtSubcompositesMap = sub_lat_map
                                                      }

------------------------------------------------------------------------

-- | Make a 'Text.PCLT.Template.LocalizableTemplate'
-- out of a single 'ByteString'.
str2pclt :: (PCLT_ID, PCLT_AllocatedShowDetalizationLevel) -> (LanguageName, Lazy.ByteString) -> PCLT_Catalog -> Either (ErrorWithPCSCatalog ReadPCSCatalogError) LocalizableTemplate
str2pclt (tpl_id, req_sdl) (lng, str) catalog =
        let err_or_ldt = adhoc_str2ldt (str, lng) catalog
         in case err_or_ldt of
                Right ldt -> Right LocalizableTemplate { pcltLocalizationsMap = M.singleton lng ldt, pcltRequiredSDL = req_sdl }
                Left   err -> Left $ ErrorWithPCSCatalog (pcltcCatalogID catalog) $
                                     case err of
                                         ParseFailure_AHS2PE  ll_errs_list -> case lng == catDfltLng catalog of
                                                                                  True  -> ParseFailedForDefaultLng_RPCSCE    tpl_id False     ll_errs_list
                                                                                  False -> ParseFailedForNondefaultLng_RPCSCE tpl_id False lng ll_errs_list
                                         RequiredCompositeIsMissing_AHS2PE red_id -> RequiredCompositeIsMissing_RPCSCE $ RequiredByRequirerCompositeIsMissing_PCLTE tpl_id red_id

-- | Make a 'Text.PCLT.Template.LocalizableTemplate' out of list
-- of 'ByteString's.
str_list2pclt :: (PCLT_ID, PCLT_AllocatedShowDetalizationLevel) -> Map LanguageName Lazy.ByteString -> PCLT_Catalog -> (LocalizableTemplate, [ErrorWithPCSCatalog ReadPCSCatalogError])
str_list2pclt (tpl_id, req_sdl) str_map catalog =
        let so = catStrictOrient catalog
            dflt_lng = catDfltLng catalog
            (mb_err_or_pclt_w_dfltlng, nodfltlng_str_list) =
                    let (mb_dfltlng_str, left_) = takeFromMap dflt_lng str_map
                     in ( mb_dfltlng_str >>= \ str -> return $ str2pclt (tpl_id, req_sdl) (dflt_lng, str) catalog
                        , left_
                        )
            (errs_list1, pclt1, mb_dflt_ldt) =
                                  case mb_err_or_pclt_w_dfltlng of
                                      Nothing           -> ([]   , LocalizableTemplate { pcltLocalizationsMap = M.empty, pcltRequiredSDL = req_sdl}, Nothing)
                                      Just (Left   err) -> ([err], LocalizableTemplate { pcltLocalizationsMap = M.empty, pcltRequiredSDL = req_sdl}, Nothing)
                                      Just (Right pclt) ->
                                                       let dflt_ldt = pcltLocalizationsMap pclt ! dflt_lng -- compareStrictOrientationOnDefault
                                                        in ([]   , pclt, Just dflt_ldt)
            dflt_persists = isJust mb_dflt_ldt
         in foldr
                (\ (lng, str) (pclt_accum, errs_accum) ->
                        let err_or_pclt2 = str2pclt (tpl_id, req_sdl) (lng, str) catalog
                         in case err_or_pclt2 of
                                Left  arpcsce -> (pclt_accum, arpcsce : errs_accum)
                                Right pclt2 ->
                                        let good_outcome = ( pclt_accum { pcltLocalizationsMap = M.union (pcltLocalizationsMap pclt_accum) (pcltLocalizationsMap pclt2)}
                                                           , errs_accum
                                                           )
                                         in case dflt_persists of
                                                False -> good_outcome
                                                True  ->
                                                     let cur_ldt  = pcltLocalizationsMap pclt2 ! lng
                                                         dflt_ldt = fromJust mb_dflt_ldt
                                                      in case compareStrictOrientationOnDefault tpl_id so cur_ldt dflt_ldt of
                                                             True  -> good_outcome
                                                             False -> let err = ErrorWithPCSCatalog
                                                                                        (pcltcCatalogID catalog)
                                                                                        (DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_RPCSCE $
                                                                                                DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_PCLTE tpl_id lng)
                                                                       in (pclt_accum, err : errs_accum)
                )
                (pclt1, errs_list1)
                (M.toList nodfltlng_str_list)