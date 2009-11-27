{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | Functions from this module isn't used for catalog formation.
-- It's not very wise to have two similar codes of catalog maths here and
-- in module "Text.PCLT.MakeCatalog", since it raises double maintenence
-- problem. Perhaps the problem will be solved in future versions.
--
-- The math's errors, however, seems to be of use in other modules.
module Text.PCLT.CatalogMaths where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe
import Data.MyHelpers
import Data.Word
import Data.Typeable
import Text.ConstraintedLBS
import Text.PCLT.Catalog
import Text.PCLT.Config
import Text.PCLT.CommonTypes
import Text.PCLT.MakeCatalog
import Text.PCLT.SDL
import Text.PCLT.Template

----------------------------------------------------------------------------------------------------------------
-- | Error type for 'addLngTpl_toPCLT'.
data AddLngTpl_toPCLT_Error =
          TplUniquenessViol_APSTPTE TplUniquenessViol_PCLTE
        | DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_APSTPTE DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_PCLTE
      deriving (Show, Typeable)

-- | Adds localized template as a special case of localizable template.
addLngTpl_toPCLT :: PCLT_ID -> (LanguageName, LocalizedTemplate) -> LocalizableTemplate -> PCLT_InnerConfig -> (LocalizableTemplate, [AddLngTpl_toPCLT_Error])
addLngTpl_toPCLT tpl_id (lng, ldt) pclt cfg =
        let dflt_lng = pcsDefaultLanguage cfg
            so = pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets cfg
            aswas_ldt_lng_map = pcltLocalizationsMap pclt
            dltcpsdfoon_err _tpl_id _lng = DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_APSTPTE $ DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_PCLTE _tpl_id _lng
         in case M.member lng aswas_ldt_lng_map of
                True  -> (pclt, [TplUniquenessViol_APSTPTE $ TplUniquenessViol_PCLTE tpl_id [lng]])
                False ->
                     let (mb_dflt_ldt, ldt_lng_map_tocheck, ldt_lng_map_tostay) =
                             case lng == dflt_lng of
                                 True  -> (Just ldt, aswas_ldt_lng_map, M.empty)
                                 False ->
                                      let (mb_dfltlng_ldt, left_to_stay) = takeFromMap dflt_lng aswas_ldt_lng_map
                                       in ( mb_dfltlng_ldt
                                          , M.singleton lng ldt
                                          , left_to_stay
                                          )
                      in case mb_dflt_ldt of
                             Nothing -> (pclt { pcltLocalizationsMap = M.union ldt_lng_map_tocheck ldt_lng_map_tostay } , [])
                             Just dflt_ldt ->
                                        foldr
                                          (\ (lng, ldt) (pclt_accum, errs_accum) ->
                                                case compareStrictOrientationOnDefault tpl_id so ldt dflt_ldt of
                                                    True  -> (pclt_accum { pcltLocalizationsMap = M.insert lng ldt (pcltLocalizationsMap pclt_accum) }, errs_accum )
                                                    False -> (pclt_accum , dltcpsdfoon_err tpl_id lng : errs_accum )
                                          )
                                          (pclt { pcltLocalizationsMap = M.insert dflt_lng dflt_ldt ldt_lng_map_tostay }, [])
                                          (M.toList ldt_lng_map_tocheck)

-- | Error type for 'addPCLT_toPCLT'.
data AddPCLT_toPCLT_Error =
          AddLngTpl_toPCLT_Error_APTTPTE AddLngTpl_toPCLT_Error
        | DifferentSDLs_APTTPTE DifferentSDLs_PCLTE
       deriving (Show, Typeable)

-- | Adds up localizable templates, their localized cases.
addPCLT_toPCLT :: PCLT_ID -> LocalizableTemplate -> LocalizableTemplate -> PCLT_InnerConfig -> (LocalizableTemplate, [AddPCLT_toPCLT_Error])
addPCLT_toPCLT tpl_id add_pclt main_pclt cfg =
        foldr (\ (lng, added_ldt) (main_pclt_accum, errs_accum) ->
                        let (new_pclt_accum, new_errs) = addLngTpl_toPCLT tpl_id (lng, added_ldt) main_pclt_accum cfg
                         in (new_pclt_accum, map AddLngTpl_toPCLT_Error_APTTPTE new_errs ++ errs_accum)
              )
              ( main_pclt
              , let sdls@(main_sdl, add_sdl) = (pcltRequiredSDL main_pclt, pcltRequiredSDL add_pclt)
                 in case add_sdl == main_sdl of
                        True  -> []
                        False -> [DifferentSDLs_APTTPTE $ DifferentSDLs_PCLTE tpl_id sdls]
              )
              (M.toList $ pcltLocalizationsMap add_pclt)

