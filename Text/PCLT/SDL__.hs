{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}

module Text.PCLT.SDL__ where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Text.PCLT.SH__
import Text.PCLT.SDL

-------------------------------------------

data ShowDetalizationLevel_PCSIWrapper = ShowDetalizationLevel_PCSIWrapper ShowDetalizationLevel
instance ShowAsPCSI ShowDetalizationLevel_PCSIWrapper where
        showAsPCSI (ShowDetalizationLevel_PCSIWrapper sdl) = thePCSI "PCLT_SDL" [("pclt_sdl_details", PCSI_PV $ showAsPCSI sdl)]

instance ShowAsPCSI ShowDetalizationLevel where
        showAsPCSI sdl =
             let pcsi_id_suffix = case sdl of
                                     InfinitelyBig_SDL -> "INFBIG"
                                     Zero_SDL          -> "ZERO"
                                     One_SDL           -> "ONE"
                                     (SDL _)           -> "NUM"
              in thePCSI
                        ("PCLT_SDL_" ++ pcsi_id_suffix)
                        [("sdl_m", PlainText_PV $ show sdl)]

instance HasStaticRawPCLTs ShowDetalizationLevel where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("PCLT_SDL", (M.fromList [("rus", B.pack "##|PCLT_SDL_PREFIX##|@@|pclt_sdl_details@@|."), ("eng", B.pack "##|PCLT_SDL_PREFIX##|@@|pclt_sdl_details@@|.")], str2PCLT_SDL Required_SDLM "5000" inner_cfg))
                        , ("PCLT_SDL_PREFIX", (M.fromList [("rus", B.pack "Уровень детализации отображения сообщения из шаблона: "), ("eng", B.pack "Show detalization level from a message template:")], str2PCLT_SDL Required_SDLM "##|PCLT_SDL##|" inner_cfg))
                        , ("PCLT_SDL_INFBIG", (M.fromList [("rus", B.pack "бесконечно большой (\"@@|sdl_m@@|\")"), ("eng", B.pack "infinitely big (\"@@|sdl_m@@|\")")], str2PCLT_SDL Required_SDLM "##|PCLT_SDL##|" inner_cfg))
                        , ("PCLT_SDL_ZERO", (M.fromList [("rus", B.pack "ноль (\"@@|sdl_m@@|\")"), ("eng", B.pack "zero (\"@@|sdl_m@@|\")")], str2PCLT_SDL Required_SDLM "##|PCLT_SDL##|" inner_cfg))
                        , ("PCLT_SDL_ONE", (M.fromList [("rus", B.pack "единица (\"@@|sdl_m@@|\")"), ("eng", B.pack "one (\"@@|sdl_m@@|\")")], str2PCLT_SDL Required_SDLM "##|PCLT_SDL##|" inner_cfg))
                        , ("PCLT_SDL_NUM", (M.fromList [("rus", B.pack "номер: \"@@|sdl_m@@|\""), ("eng", B.pack "numeric: \"@@|sdl_m@@|\"")], str2PCLT_SDL Required_SDLM "##|PCLT_SDL##|" inner_cfg))
                        ]

------------------------------------------------------------------------------

data SDLModus_PCSIWrapper = SDLModus_PCSIWrapper SDLModus
instance ShowAsPCSI SDLModus_PCSIWrapper where
        showAsPCSI (SDLModus_PCSIWrapper sdlm) = thePCSI "PCLT_SDLM" [("pclt_sdlm_details", PCSI_PV $ showAsPCSI sdlm)]

instance ShowAsPCSI SDLModus where
        showAsPCSI sdlm =
             let pcsi_id_suffix = case sdlm of
                                     Allocated_SDLM -> "ALLOC"
                                     Required_SDLM          -> "REQ"
              in thePCSI
                        ("PCLT_SDLM_" ++ pcsi_id_suffix)
                        []

instance HasStaticRawPCLTs SDLModus where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("PCLT_SDLM", (M.fromList [("rus", B.pack "##|PCLT_SDLM_PREFIX##|@@|pclt_sdlm_details@@|."), ("eng", B.pack "##|PCLT_SDLM_PREFIX##|@@|pclt_sdlm_details@@|.")], str2PCLT_SDL Required_SDLM "##|PCLT_SDL##|" inner_cfg))
                        , ("PCLT_SDLM_PREFIX", (M.fromList [("rus", B.pack "Контекст уровня детализации отображения сообщения: "), ("eng", B.pack "Show detalization level usage:")], str2PCLT_SDL Required_SDLM "##|PCLT_SDLM##|" inner_cfg))
                        , ("PCLT_SDLM_ALLOC", (M.fromList [("rus", B.pack "дозволенный получателю сообщения"), ("eng", B.pack "allowed to message reader")], str2PCLT_SDL Required_SDLM "##|PCLT_SDLM##|" inner_cfg))
                        , ("PCLT_SDLM_REQ", (M.fromList [("rus", B.pack "требуемый для отоброжения"), ("eng", B.pack "required for message representation")], str2PCLT_SDL Required_SDLM "##|PCLT_SDLM##|" inner_cfg))
                        ]

------------------------------------------------------------------------------

data PCLTRawCatalog__Text_PCLT_SDL = PCLTRawCatalog__Text_PCLT_SDL
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_SDL where
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: ShowDetalizationLevel)
                        , getStaticRawPCLTs inner_cfg (undefined :: SDLModus)
                        ] -- i wish i knew an easy way how to achieve a smaller code of this by using a "map (getStaticRawPCLTs inner_cfg) [...]" function