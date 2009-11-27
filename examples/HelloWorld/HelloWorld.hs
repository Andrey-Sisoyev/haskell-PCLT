{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- Without extending HelloWorld application with PCLT catalog, it would look like the code in "HelloWorld.NoPCLT.hs" file

module HelloWorld where

-----------------------------------------------------
-- Modules necessary for our PCLTCatalog
import Prelude hiding (putStrLn) -- Prelude putStrLn doesn't work properly with utf8
import Data.ByteString.Lazy.UTF8

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString) -- all catalog business is done using lazy ByteStrings
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString) -- since ByteStrings (and System.IO) is not very friendly to unicode symbols "utf8-string" package us used
import qualified Data.Map as M
import Data.Map (Map, (!))
import System.IO.UTF8 hiding (readLn)

import           Text.PCLT -- this module exports most important PCLT modules - there (in Text.PCLT) are some comments about exported modules
import qualified Text.ConstraintedLBS as CB -- a constrainting (the constraint here is on it's size) wrapper for a lazy ByteString (LBS) - this container is used for messages generated from PCLT templates

-----------------------------------------------------
-----------------------------------------------------
-- Application specific modules
import Control.Exception

-----------------------------------------------------
-----------------------------------------------------
-- Application specific data structures

-- \/ data
data HelloWorld = HelloWorld

-- \/ errors
type WorldName = String
type WorldIndex = Int
data HelloWorldError =
          NoWorld_HWE
        | AmbiguousChoiceOfWorlds_HWE (WorldName, WorldIndex) (WorldName, WorldIndex) [(WorldName, WorldIndex)]
        | SomeVeryStrangeError_HWE Int String Bool (Maybe Bool) (Maybe Bool) SomeException
        | FailedDueToErrorInSubsystem_HWE ErrorInSubsystem

data ErrorInSubsystem =
          ErrorType1_EIS
        | ErrorType2_EIS
        | FailedDueToErrorInSub_sub_system_EIS ErrorInSub_sub_system

data ErrorInSub_sub_system =
          ErrorType1_EISS
        | ErrorType2_EISS

-----------------------------------------------------
-----------------------------------------------------
-- Functional part of app

type SayHelloWorld_Mode = Int
sayHelloWorld :: SayHelloWorld_Mode -> Either HelloWorldError HelloWorld
sayHelloWorld mode =
        case mode of
            0 -> Right HelloWorld
            1 -> Left NoWorld_HWE
            2 -> Left $ AmbiguousChoiceOfWorlds_HWE ("RealWorld", 1) ("VirtualWorld", 2) [("OtherWorld1", 3), ("OtherWorld2", 4), ("OtherWorld3", 5)]
            3 -> Left $ SomeVeryStrangeError_HWE 5789 "Noise..." True (Just True) Nothing (SomeException DivideByZero)
            4 -> Left $ FailedDueToErrorInSubsystem_HWE ErrorType1_EIS
            5 -> Left $ FailedDueToErrorInSubsystem_HWE ErrorType2_EIS
            6 -> Left $ FailedDueToErrorInSubsystem_HWE $ FailedDueToErrorInSub_sub_system_EIS ErrorType1_EISS
            7 -> Left $ FailedDueToErrorInSubsystem_HWE $ FailedDueToErrorInSub_sub_system_EIS ErrorType2_EISS

acquireCatalog :: StdErr_CLBS -> (ShowDetalizationLevel, LanguageName) -> (PCLT_Catalog, StdErr_CLBS)
acquireCatalog stderr_clbs (stderr_sdl, stderr_lng) =
        let catalog_id = 777
            catalog_config = defaultPCLTInnerConfig { -- config, that influences catalog formation and messages generation
                                                     -- a catalog always hasa default language (primary)
                                                     -- by default a strict orientation of other languages on this primary language is turned on - for every nonprimary language template, it's sets of parameters and composites must be equal to the sets in primary language
                                                     -- this stricness is easy to turn off or to add exclusions to (when not to be strict on composites and parameters sets)
                                                     -- (but one can't turn off (or make exclusions for) the constraint, according to which, a template can't be added to a catalog if it's version for primary language is nowhere (in catalog, in added template) to be found)
                                  pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets = -- let's add one such exclusion: we don't want catalogto be strict on a parameter "__row_idx" in template "E_HWE_AMBWRLDCH_OW"
                                        (pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets defaultPCLTInnerConfig) {
                                                soExcludingCompParameters = soExcludingCompParameters (pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets defaultPCLTInnerConfig)
                                                                         ++ [("E_HWE_AMBWRLDCH_OW", "__row_idx")]
                                        }
                                }

            -- \/ these here aren't to be believed to be lightweight operations - don't use them too often, just intitialize catalog once when program starts
            (catalog1, stderr_clbs2) = initDefaultCatalog_2 -- it contains temlates of messages used by PCLT pacakage itself (error messages and some shows)
                                                catalog_config
                                                catalog_id
                                                (stderr_clbs, stderr_sdl, stderr_lng) -- collect error messages if any
            (catalog2, stderr_clbs3) = addFromHSRTToCatalog_2
                                                PCLTRawCatalog__HelloWorld -- an instance of HasStaticRawPCLTs, a data type to which we bind all the application specifc templates
                                                catalog1 -- we add it to the initial version of catalog
                                                (stderr_clbs2, stderr_sdl, stderr_lng) -- collect error messages if any

         in (catalog2, stderr_clbs3)

showHelloWorld :: SayHelloWorld_Mode -> (StdOut_CLBS, StdErr_CLBS) -> (ShowDetalizationLevel, LanguageName, PCLT_Catalog) -> (StdOut_CLBS, StdErr_CLBS) -- first CLBS - result; second - representation of catalog errors list
showHelloWorld mode (stdout_clbs, stderr_clbs) (sdl, lng, catalog) =
        let err_or_HelloWorld = sayHelloWorld mode
            (new_stdout_clbs, new_stderr_clbs) = -- note: here in stderr_clbs goes only catalog-works errors, HelloWorldError (if any) goes into stdout_clbs
                pcsi2text_plus_errs_2
                        (stdout_clbs, stderr_clbs)
                        (showAsPCSI err_or_HelloWorld) -- this means, thet err_or_HelloWorld is of ShowAsPCSI class - the according instance is declared in the bottom of this example
                        (sdl, lng)
                        catalog
         in (new_stdout_clbs, new_stderr_clbs) -- these are showable, and one may print them "CB.putStrLn stdout_clbs"

main = run_test "rus" (SDL 15000) -- possible values here: ["rus", "eng", "hs_"] x [Zero_SDL, One_SDL, SDL <Int>, InfinitelyBig_SDL]

run_test _lng _sdl =
       let my_lng = _lng
           my_sdl = _sdl
           my_stderr_clbs_size = 50000
           my_stdout_clbs_size = 50000
           stderr_clbs0 = newCLBS my_stderr_clbs_size
           stdout_clbs0 = newCLBS my_stdout_clbs_size
           (catalogue, stderr_clbs1) = acquireCatalog stderr_clbs0 (my_sdl, my_lng)
           iterate_ = do
                  putStrLn "----New-iteration:---------------"
                  putStrLn "Input sayHelloWorld mode (0-7; '-1' to exit): "
                  mode <- readLn
                  case mode >= 0 && mode <= 7 of
                      True -> do let (stdout_clbs1, stderr_clbs1) =
                                           showHelloWorld
                                                mode
                                                (stdout_clbs0, stderr_clbs0)
                                                (my_sdl, my_lng, catalogue)
                                 putStrLn "----Errors:----------------------"
                                 putStrLn $ toString $ clbsLBS stderr_clbs1
                                 putStrLn "----Output:----------------------"
                                 putStrLn $ toString $ clbsLBS stdout_clbs1
                                 --putStrLn $ show $ toString $ clbsLBS stdout_clbs1
                                 iterate_
                      False -> case mode == (-1) of
                                   True  -> return ()
                                   False -> iterate_
        in do putStrLn ("Language, SDL (detailization level): " ++ show (_lng, _sdl))
              putStrLn "----Init-errors:-----------------"
              putStrLn $ show stderr_clbs1
              putStrLn "----Cycle-start:-----------------"
              iterate_

-----------------------------------------------------
-----------------------------------------------------
-- Representations
-- Note: it's a recommended practice to put these declarations below into a separate file...
-- IMPORTANT!!! : It is highly recommended to use ISO 639(3) standard for language names, since PCLT-DB package that addons a DBMS powers to PCLT catalogs management is oriented on 3 letters (not bigger) language names. Without modifications PCLT-DB won't work for bigger (then 3-letters) names.

instance (ShowAsPCSI HelloWorldError, ShowAsPCSI HelloWorld) => ShowAsPCSI (Either HelloWorldError HelloWorld) where
        showAsPCSI err_or_hw =
                case err_or_hw of
                    Right hw  -> showAsPCSI hw
                    Left  hwe -> showAsPCSI hwe

instance ShowAsPCSI HelloWorld where
     showAsPCSI hw = empPCSI "HW"
instance HasStaticRawPCLTs HelloWorld where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [ ( "HW"
                          , ( M.fromList
                                  [ ("rus", B.pack "Привет, Мир!")
                                  , ("eng", B.pack "Hello world!")
                                  , ("hs_", B.pack "HelloWorld")
                                  ]
                            , str2PCLT_SDL Required_SDLM "0" inner_cfg
                            )
                          )
                        ]

instance ShowAsPCSI HelloWorldError where
     showAsPCSI hwe = thePCSI "E_HWE" [ ("hwe_details_pcsi", PCSI_PV hwe_details_pcsi) ]
        where
           hwe_details_pcsi =
                case hwe of
                    NoWorld_HWE ->
                        empPCSI "E_HWE_NOWRLD"
                    AmbiguousChoiceOfWorlds_HWE (wn1, wi1) (wn2, wi2) other_worlds ->
                        thePCSI "E_HWE_AMBWRLDCH"
                                [ ("w_name_1", PlainText_PV wn1)
                                , ("w_idx_1" , PlainText_PV $ show wi1)
                                , ("w_name_2", PlainTextLBS_PV $ B.pack wn1) -- lazy ByteString is also welcome
                                , ("w_idx_2" , PlainTextLBS_PV $ B.pack $ show wi2)
                                , ("other_worlds", case null other_worlds of -- this is a #1 way to include templates in a template - through parameters using PCSI_PV and/or PCSIList_PV parameter value types
                                                       True  -> PCSI_PV $ empPCSI "E_HWE_AMBWRLDCH_NOMORE"
                                                       False ->
                                                            Indented_PV 3 $ -- a way to insert 3 whitespaces after each '\n' in a string that results from what's wrapped in
                                                                  PCSIList_PV
                                                                       (map (\ (w_name, w_idx) ->
                                                                                thePCSI
                                                                                     "E_HWE_AMBWRLDCH_OW"
                                                                                     [ ("w_name", PlainText_PV w_name)
                                                                                     , ("w_idx" , PlainText_PV $ show w_idx)
                                                                                     ]
                                                                            )
                                                                            other_worlds
                                                                       )
                                                                       (PCSI_PV $ empPCSI "E_HWE_AMBWRLDCH_OW_SEP") -- this is a separator between rows in representation (put Nothing_PV here, if you want to omit separator)
                                  )
                                ]
                    SomeVeryStrangeError_HWE i s b mb_b1 mb_b2 sm_excpt ->
                        thePCSI "E_HWE_STRNGERR"
                                [ ("int"     , PlainText_PV $ show i)
                                , ("str"     , PlainText_PV s)
                                , ("bool"    , PCSI_PV $ showAsPCSI b) -- Text.PCLT.ShowAsPCSI__ module conains ShowAsPCSI instances for some basic general types (Bool, ShowAsPCSI a => Maybe a, SomeException)
                                , ("mb_bool1", PCSI_PV $ showAsPCSI mb_b1) -- such treatment is possible only if type under Maybe is of ShowAsPCSI class
                                , ("mb_bool2", PCSI_PV $ showAsPCSI mb_b2)
                                , ("sm_excpt", PCSI_PV $ showAsPCSI sm_excpt) -- uses SomeException instaniation from Text.PCLT.ShowAsPCSI__
                                ]
                    FailedDueToErrorInSubsystem_HWE eis ->
                        [showAsPCSI eis] `addToPCSI` empPCSI "E_HWE_EIS" -- if summed PCSIs parameters names happen coincide, the walue is kept from first parameter, from earliest PCSI from list

-- E_HWE_STRNGERR template refers to some predefined standard templates, assumming, that Bool, (Maybe a) and SomeException already are instances of ShowAsPCSI class.
-- And they really are instaniated in Text.PCLT.ShowAsPCLT__ module. However, HasStaticRawPCLTs instances of Bool, (Maybe a) and SomeException support only 2 languages: eng, rus.
-- But we use 3 languages here: eng, rus and haskell!
-- So, in order to make this example complete, we also need to add representations for Bool, (Maybe a) and SomeException types in a 'haskell' language:
data PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons__addon_for_haskell_lng = PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons__addon_for_haskell_lng
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons__addon_for_haskell_lng where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
             PCLT_RawCatalogData $ M.fromList
                                        [
                                          ("TRUE", (M.fromList [("hs_", B.pack "True")], str2PCLT_SDL Required_SDLM "one" inner_cfg))
                                        , ("FALSE", (M.fromList [("hs_", B.pack "False")], str2PCLT_SDL Required_SDLM "##|TRUE##|" inner_cfg))

                                        , ("MAYBE_A", (M.fromList [("hs_", B.pack "@@|maybe_cnstr@@|")], str2PCLT_SDL Required_SDLM "##|TRUE##|" inner_cfg))
                                        , ("MAYBE_JUST", (M.fromList [("hs_", B.pack "Just @@|a@@|")], str2PCLT_SDL Required_SDLM "##|MAYBE_A##|" inner_cfg))
                                        , ("MAYBE_NOTHING", (M.fromList [("hs_", B.pack "Nothing")], str2PCLT_SDL Required_SDLM "##|MAYBE_A##|" inner_cfg))

                                        , ("LLEXCPT", (M.fromList [("hs_", B.pack "SomeException (ErrorCall \"@@|excpt_msg@@|\")")], str2PCLT_SDL Required_SDLM "1000" inner_cfg))
                                        ]

instance HasStaticRawPCLTs HelloWorldError where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [ ( "E_HWE"
                          , ( let same_tpl = B.pack "##|E_HWE_PREFIX##|@@|hwe_details_pcsi@@|" -- the E_HWE_PREFIX is a composite; the hwe_details_pcsi is a parameter
                               in M.fromList
                                  [ ("rus", same_tpl)
                                  , ("eng", same_tpl)
                                  , ("hs_", same_tpl)
                                  ]
                            , str2PCLT_SDL Required_SDLM "10" inner_cfg -- == PCLT_SDL $ SDL 10
                            )
                          )
                        , ( "E_HWE_PREFIX"
                          , ( M.fromList
                                  [ ("rus", B.pack "Приветствие мира не удалось!\nПричина: ")
                                  , ("eng", B.pack "Hello world failure!\nReason: ")
                                  , ("hs_", B.empty)
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg -- the SDL must be the same as specified for E_HWE template
                                                                                 -- == PCLT_SDL_ToTemplateLink "E_HWE"
                            )
                          )
                        , ( "E_HWE_NOWRLD"
                          , ( M.fromList
                                  [ ("rus", B.pack "некого приветствовать (нет мира)!")
                                  , ("eng", B.pack "no world!")
                                  , ("hs_", B.pack "NoWorld_HWE")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_AMBWRLDCH"
                          , ( M.fromList
                                  [ ("rus", B.pack "неясно, какой из миров приветствовать - их несколько!\nПервый мир: [имя: '@@|w_name_1@@|', индекс: @@|w_idx_1@@|].\nВторой мир: [имя: '@@|w_name_2@@|', индекс: @@|w_idx_2@@|].\nА так же эти миры: \n   @@|other_worlds@@|.")
                                  , ("eng", B.pack "ambiguous choice of worlds!\nFirst world: [name: '@@|w_name_1@@|', index: @@|w_idx_1@@|].\nSecond world: [name: '@@|w_name_2@@|', index: @@|w_idx_2@@|].\nAnd also these worlds: \n   @@|other_worlds@@|.")
                                  , ("hs_", B.pack "AmbiguousChoiceOfWorlds_HWE\n   (\"@@|w_name_1@@|\", @@|w_idx_1@@|)\n   (\"@@|w_name_2@@|\", @@|w_idx_2@@|)\n   [ @@|other_worlds@@|\n   ]")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_AMBWRLDCH_OW"
                          , ( M.fromList
                                  [ ("rus", B.pack "@@|__row_idx@@|) мир [имя: '@@|w_name@@|', индекс: @@|w_idx@@|]") -- __row_idx is an implicit parameter, that can be used by templates that are guaranteed to be wrapped in a PCSIList_PV (not PVList_PV !!!) parameter value wrapper
                                  , ("eng", B.pack "@@|__row_idx@@|) world [name: '@@|w_name@@|', index: @@|w_idx@@|]")
                                  , ("hs_", B.pack "(\"@@|w_name@@|\", @@|w_idx@@|)")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE_AMBWRLDCH##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_AMBWRLDCH_OW_SEP"
                          , ( M.fromList
                                  [ ("rus", B.pack "\n")
                                  , ("eng", B.pack "\n")
                                  , ("hs_", B.pack "\n, ")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE_AMBWRLDCH_OW##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_AMBWRLDCH_NOMORE"
                          , ( M.fromList
                                  [ ("rus", B.pack "список пуст.")
                                  , ("eng", B.pack "empty list.")
                                  , ("hs_", B.empty)
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE_AMBWRLDCH_OW##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_STRNGERR"
                          , ( M.fromList
                                  [ ("rus", B.pack "какая-то странная непонятная ошибка! Данные: @@|int@@| \"@@|str@@|\" @@|bool@@| (@@|mb_bool1@@|) (@@|mb_bool2@@|) { @@|sm_excpt@@| }")
                                  , ("eng", B.pack "some very strange error! Data: @@|int@@| \"@@|str@@|\" @@|bool@@| (@@|mb_bool1@@|) (@@|mb_bool2@@|) { @@|sm_excpt@@| }")
                                  , ("hs_", B.pack "SomeVeryStrangeError_HWE @@|int@@| \"@@|str@@|\" @@|bool@@| (@@|mb_bool1@@|) (@@|mb_bool2@@|) (@@|sm_excpt@@|)")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_EIS"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка в подсистеме!\nТекст исключения уровнем ниже:\n##|E_EIS##|")
                                  , ("eng", B.pack "failed due to error(s) in subsystem!\nLower level exception message:\n##|E_EIS##|")
                                  , ("hs_", B.pack "FailedDueToErrorInSubsystem_HWE (##|E_EIS##|)") -- include a template E_EIS here (note: it's representation together with E_HWE_EIS will share one parameters values map)
                                                                                                        -- this is a #2 way to include templates in a template - as composites
                                                                                                        -- the difference from #1 way is such, that in #2 template and it's composites share one same parameters map, but in #1 each inclusion has it's own parameters map
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg
                            )
                          )
                        ]

instance ShowAsPCSI ErrorInSubsystem where
     showAsPCSI eis = thePCSI "E_EIS" [ ("eis_details_pcsi", PCSI_PV eis_details_pcsi) ]
        where
           eis_details_pcsi =
                case eis of
                    ErrorType1_EIS ->
                        empPCSI "E_EIS_ET1"
                    ErrorType2_EIS ->
                        empPCSI "E_EIS_ET2"
                    FailedDueToErrorInSub_sub_system_EIS eiss ->
                        thePCSI "E_EIS_EISS" [("e_eiss", PCSI_PV $ showAsPCSI eiss)]
instance HasStaticRawPCLTs ErrorInSubsystem where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [ ( "E_EIS"
                          , ( let same_tpl = B.pack "##|E_EIS_PREFIX##|@@|eis_details_pcsi@@|"
                               in M.fromList
                                  [ ("rus", same_tpl)
                                  , ("eng", same_tpl)
                                  , ("hs_", same_tpl)
                                  ]
                            , str2PCLT_SDL Required_SDLM "20" inner_cfg
                            )
                          )
                        , ( "E_EIS_PREFIX"
                          , ( M.fromList
                                  [ ("rus", B.pack "Сбой в подсистеме!\nПричина: ")
                                  , ("eng", B.pack "Subsystem failure!\nReason: ")
                                  , ("hs_", B.empty)
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EIS##|" inner_cfg
                            )
                          )
                        , ( "E_EIS_ET1"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка типа #1!")
                                  , ("eng", B.pack "error of type #1!")
                                  , ("hs_", B.pack "ErrorType1_EIS")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EIS##|" inner_cfg
                            )
                          )
                        , ( "E_EIS_ET2"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка типа #2!")
                                  , ("eng", B.pack "error of type #2!")
                                  , ("hs_", B.pack "ErrorType2_EIS")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EIS##|" inner_cfg
                            )
                          )
                        , ( "E_EIS_EISS"
                          , ( M.fromList
                                  [ ("rus", B.pack "сбой в подПОДсистеме! Текст исключения уровнем ниже: @@|e_eiss@@|")
                                  , ("eng", B.pack "failed due to error(s) in subSUBsystem!\nLower level exception message:\n@@|e_eiss@@|")
                                  , ("hs_", B.pack "FailedDueToErrorInSub_sub_system_EIS (@@|e_eiss@@|)")
                                  -- i could have included representation of ErrorInSub_sub_system as a composite here ##|E_EISS##| (the same as we did in E_HWE_EIS template)
                                  -- but for example purpose i do the same work in a different way through @@|e_eiss@@| parameter
                                  -- this way is less strict because user may choose not to (forget to) put under a parameter "e_eiss" the template "E_EISS"
                                  ]
                            , str2PCLT_SDL Required_SDLM "@@|e_eiss@@|" inner_cfg -- == PCLT_SDL_ToParamCompositeLink "e_eiss"
                              -- now if user forgets to put a PCSI_PV in the value of param "e_eiss", the engine will complain with an error
                              -- because here we specify, that SDL requirement must be the same as for TEMPLATE that is under "e_eiss" parameter
                            )
                          )
                        ]

instance ShowAsPCSI ErrorInSub_sub_system where
     showAsPCSI eiss = thePCSI "E_EISS" [ ("eiss_details_pcsi", PCSI_PV eiss_details_pcsi) ]
        where
           eiss_details_pcsi =
                case eiss of
                    ErrorType1_EISS ->
                        empPCSI "E_EISS_ET1"
                    ErrorType2_EISS ->
                        empPCSI "E_EISS_ET2"
instance HasStaticRawPCLTs ErrorInSub_sub_system where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [ ( "E_EISS"
                          , ( let same_tpl = B.pack "##|E_EISS_PREFIX##|@@|eiss_details_pcsi@@|"
                               in M.fromList
                                  [ ("rus", same_tpl)
                                  , ("eng", same_tpl)
                                  , ("hs_", same_tpl)
                                  ]
                            , str2PCLT_SDL Required_SDLM "30" inner_cfg
                            )
                          )
                        , ( "E_EISS_PREFIX"
                          , ( M.fromList
                                  [ ("rus", B.pack "Сбой в подПОДсистеме!\nПричина: ")
                                  , ("eng", B.pack "SubSUBsystem failure!\nReason: ")
                                  , ("hs_", B.empty)
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EISS##|" inner_cfg
                            )
                          )
                        , ( "E_EISS_ET1"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка типа #1!")
                                  , ("eng", B.pack "error of type #1!")
                                  , ("hs_", B.pack "ErrorType1_EISS")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EISS##|" inner_cfg
                            )
                          )
                        , ( "E_EISS_ET2"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка типа #2!")
                                  , ("eng", B.pack "error of type #2!")
                                  , ("hs_", B.pack "ErrorType2_EISS")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EISS##|" inner_cfg
                            )
                          )
                        ]

------------------------------------
-- Gathering all HasStaticRawPCLTs insances and sticking them to one data type. This way we may scope all our templates, for example, when we need a single reference on catalog input data (as in addFromHSRTToCatalog)
data PCLTRawCatalog__HelloWorld = PCLTRawCatalog__HelloWorld
instance HasStaticRawPCLTs PCLTRawCatalog__HelloWorld where -- here we gather all the local to module HasStaticRawPCLTs instances, so tha it's easy to mention them by one word
     widenessOfStaticRawPCLTsSet _ = Module_RPSW -- this is apurely informative declaration, no functions make use of it so far
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: HelloWorld)
                        , getStaticRawPCLTs inner_cfg (undefined :: HelloWorldError)
                        , getStaticRawPCLTs inner_cfg (undefined :: ErrorInSubsystem)
                        , getStaticRawPCLTs inner_cfg (undefined :: ErrorInSub_sub_system)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons__addon_for_haskell_lng)
                        ]
-- if we also add (undefined :: PCLTRawCatalog__PCLT_InitialDefaultCatalog) in this list,
-- then instead of using "initDefaultCatalog_2" and "addFromHSRTToCatalog_2" we can use just single "initCatalogFromHSRT_2"

{-
-------------------------------------------------------------------------
-- CONCLUSION

One may see here, that adding a multilinguality to a system is not a task for a lazy guy.
While developing this (PCLT engine) package a recomended practice was explicated:
1) Separate program and representations - put all ShowAsPCSI and HasStaticRawPCLTs instances to a separate files.
   For example, author would make his file into two: "HelloWorld.hs" and "HelloWorld__.hs". The HelloWorld__ should have imported at least "Text.PCLT.SH__" and HelloWorld
2) Keep all templates in tables in OpenOffice Calc file (or Excel's analogue). An example of such file is provided in package folder, in [initial_data/PCLT.InitialCatalog.ods] - it is filled with templates data actually used by PCLT engine.

This package comes before another one - "PCLT-DB", where all templates are kept in a PostgreSQL v8.4.(not less) data base, and it is possible to run a thread, that regularly checks, if a catalog in operative memory is up tu date, and if not, rereads it from DB.
Check it in Hackage.

Best regards, Belka
-}