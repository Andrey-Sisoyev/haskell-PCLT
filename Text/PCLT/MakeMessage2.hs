{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | Addition to "Text.PCLT.MakeMessage" module. Some wrappers around 'pcsi2text' function.
module Text.PCLT.MakeMessage2 where

import Data.Int
import Data.MyHelpers
import Text.ConstraintedLBS
import Text.PCLT.Catalog
import Text.PCLT.CommonTypes
import Text.PCLT.Config
import Text.PCLT.MakeMessage
import Text.PCLT.MakeMessage__
import Text.PCLT.PCSI
import Text.PCLT.SDL
import Text.PCLT.ShowAsPCSI

-- | Wrapper around 'pcsi2text_plus_errs_1' for cases, when new 'CLBS'
-- is to be created. It's maximal length is set to be same
-- as configured in parameter 'pcsInstaniationResultMaxSize'
pcsi2new_text_plus_errs_1 :: PCSI -> (ShowDetalizationLevel, LanguageName) -> PCLT_Catalog -> StdOutAndErr_CLBS
pcsi2new_text_plus_errs_1 pcsi (sdl, lng) msg_tpls_ctlg =
        pcsi2text_plus_errs_1 (newCLBS $ catInstMaxLen msg_tpls_ctlg) pcsi (sdl, lng) msg_tpls_ctlg
-- |
--
-- (1) generates message from given PCSI
--
-- (2) generates representation of errors list (that occurred, when doing step (1))
--
-- (3) generates representation of errors list (that occurred, when doing step (2))
--
-- (4) appends text result of step (3) to given CLBS
--
-- (5) appends text result of step (2) to given CLBS
--
-- (6) appends text result of step (1) to given CLBS
pcsi2text_plus_errs_1 :: StdOutAndErr_CLBS -> PCSI -> (ShowDetalizationLevel, LanguageName) -> PCLT_Catalog -> StdOutAndErr_CLBS
pcsi2text_plus_errs_1 _init_clbs pcsi (sdl, lng) msg_tpls_ctlg =
              let init_clbs = freeSpaceCLBS _init_clbs
                  (result_clbs, errs_list1) = pcsi2text
                                                init_clbs
                                                pcsi
                                                (sdl, lng)
                                                msg_tpls_ctlg
                  (errs_clbs, errs_list2) =
                                  case null errs_list1 of
                                      True  -> (init_clbs, [])
                                      False -> pcsi2text
                                                        init_clbs
                                                        (thePCSI "E_PCLT_P2TE_LIST" [("errors_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI errs_list1) usualSeparatorInPCSIList)])
                                                        (sdl, lng)
                                                        msg_tpls_ctlg
                  (errs_errs_clbs, _) =
                                  case null errs_list2 of
                                      True  -> (init_clbs, [])
                                      False -> pcsi2text
                                                        init_clbs
                                                        (thePCSI "E_PCLT_P2TE_LIST" [("errors_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI errs_list2) usualSeparatorInPCSIList)])
                                                        (sdl, lng)
                                                        msg_tpls_ctlg
               in (result_clbs `addToCLBS_2` (errs_clbs `addToCLBS_2` (errs_errs_clbs `addToCLBS_2` _init_clbs)))

---------------------------------------------------------------------------------------

-- | Wrapper around 'pcsi2text_plus_errs_2' for cases, when new 'CLBS's
-- are to be created:
--
-- * One - for representation of normal output. It's maximal length is set
-- to be same as configured in parameter 'pcsInstaniationResultMaxSize'
--
-- * Another - for representation of errors. It's maximal size is specified
-- in the first argument of function.
pcsi2new_text_plus_errs_2 :: Int64 -> PCSI -> (ShowDetalizationLevel, LanguageName) -> PCLT_Catalog -> (StdOut_CLBS, StdErr_CLBS)
pcsi2new_text_plus_errs_2 inst_max_len pcsi (sdl, lng) msg_tpls_ctlg =
        pcsi2text_plus_errs_2 (newCLBS $ catInstMaxLen msg_tpls_ctlg, newCLBS inst_max_len) pcsi (sdl, lng) msg_tpls_ctlg

-- |
--
-- (1) generates message from given PCSI
--
-- (2) appends text result of step (1) to given CLBS_1
--
-- (3) generates representation of errors list (that occurred, when doing step (1))
--
-- (4) generates representation of errors list (that occurred, when doing step (3))
--
-- (5) appends text result of step (4) to given CLBS_2
--
-- (6) appends text result of step (3) to given CLBS_2
pcsi2text_plus_errs_2 :: (StdOut_CLBS, StdErr_CLBS) -> PCSI -> (ShowDetalizationLevel, LanguageName) -> PCLT_Catalog -> (StdOut_CLBS, StdErr_CLBS)
pcsi2text_plus_errs_2 (init_clbs, _errs_init_clbs) pcsi (sdl, lng) msg_tpls_ctlg =
              let errs_init_clbs = freeSpaceCLBS _errs_init_clbs
                  (result_clbs, errs_list1) = pcsi2text
                                                 init_clbs
                                                 pcsi
                                                 (sdl, lng)
                                                 msg_tpls_ctlg
                  (errs_clbs, errs_list2) =
                                  case null errs_list1 of
                                      True  -> (errs_init_clbs, [])
                                      False -> pcsi2text
                                                        init_clbs
                                                        (thePCSI "E_PCLT_P2TE_LIST" [("errors_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI errs_list1) usualSeparatorInPCSIList)])
                                                        (sdl, lng)
                                                        msg_tpls_ctlg
                  (errs_errs_clbs, _) =
                                  case null errs_list2 of
                                      True  -> (errs_init_clbs, [])
                                      False -> pcsi2text
                                                        init_clbs
                                                        (thePCSI "E_PCLT_P2TE_LIST" [("errors_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI errs_list2) usualSeparatorInPCSIList)])
                                                        (sdl, lng)
                                                        msg_tpls_ctlg
               in (result_clbs, errs_clbs `addToCLBS_2` (errs_errs_clbs `addToCLBS_2` errs_init_clbs))
