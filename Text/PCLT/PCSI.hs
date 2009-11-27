{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | The abbreviation \"PCSI\" means \"Parametric Composable String
-- Instaniation\". Agree, not a very clear name, a more apropriate would be
-- something like that: \"Input Data for Template Representation\", but
-- clear name came to my head when millions (dozens) of ants (variables) were
-- already bearing this name. And I'm a bit lazy to rename everything to sound
-- apropriate, sorry.
module Text.PCLT.PCSI where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Typeable
import Text.ConstraintedLBS
import Text.PCLT.CommonTypes

-- | By these user fills parameters of templates.
data PCLT_ParamVal =
        PlainText_PV    String
      | PlainTextLBS_PV Lazy.ByteString
      | PCSI_PV         PCSI
   --   Reparsable_PV   PCLT_ParamVal PCSI_ParamsValuesMap -- reserved, currently doesn't work
      -- | Second argument is a separator between 'PCSI's in first argument
      -- Here representation generator automatically adds to each PCSI in
      -- list an implicit parameter @__row_idx@, which holds a value
      -- of current PCSI index in list, starting from 1.
      | PCSIList_PV     [PCSI] PCLT_ParamVal
      | PVList_PV       [PCLT_ParamVal]
      -- | In message generation procedure this stands for a value of
      -- 2nd argument in which every occurence of newline (as is configured in
      -- "Text.PCLT.Config" in config's parameter @pcsNewlineLBS@)
      -- is substituded by @newline ++ (replicate n ' ')@,
      -- where n is the 1st argument
      | Indented_PV     Int PCLT_ParamVal
      -- | In message generation procedure this stands for a value of
      -- @pcsNewlineLBS@ parameter declared in "Text.PCLT.Config".
      -- Different systems means different symbol sequences
      -- under \"newline\"...
      | NewLine_PV
      | Nothing_PV
    deriving (Show, Typeable)
type PCSI_ParamsValuesMap = Map PCLT_ParamKey PCLT_ParamVal

-- | PCSI is an output of our extended Show (to which this package
-- is dedicated). And an input to generate a message using catalog.
data PCSI =
         PCSI {
           pcsiTplID         :: PCLT_ID
         , pcsiParamsValsMap :: PCSI_ParamsValuesMap
         }
    deriving (Show, Typeable)

-- * PCSI constructors

-- | PCSI with an empty set of parameters values.
empPCSI :: PCLT_ID -> PCSI
empPCSI k = PCSI { pcsiTplID = k, pcsiParamsValsMap = M.empty }

thePCSI :: PCLT_ID -> [(PCLT_ParamKey, PCLT_ParamVal)] -> PCSI
thePCSI pcsi_id params_alist =
         PCSI {
           pcsiTplID            = pcsi_id
         , pcsiParamsValsMap = M.fromList params_alist
         }

-- * PCSI maths

addToPCSI :: [PCSI] -> PCSI -> PCSI
addToPCSI l t = foldr
        (\ pcsi2 pcsi1_accum ->
                pcsi1_accum {
                        pcsiParamsValsMap = sumPCSI_PVMs
                                                (pcsiParamsValsMap pcsi1_accum)
                                                (pcsiParamsValsMap pcsi2)
                }
        ) t l

sumPCSI_PVMs :: PCSI_ParamsValuesMap -> PCSI_ParamsValuesMap -> PCSI_ParamsValuesMap
sumPCSI_PVMs pcsi_pvm1 pcsi_pvm2 = M.union pcsi_pvm1 pcsi_pvm2

addPVs2PCSI :: [(PCLT_ParamKey, PCLT_ParamVal)] -> PCSI -> PCSI
addPVs2PCSI pvs pcsi = pcsi { pcsiParamsValsMap = foldr (\ (k, v) accum -> M.insert k v accum) (pcsiParamsValsMap pcsi) pvs}

-- * Standards
-- | It's used in some places of package in errors' representations
-- (in instances of @ShowAsPCSI@ - class declared in "Text.PCLT.ShowAsPCSI").
-- @usualSeparatorInPCSIList = PVList_PV [NewLine_PV, PlainText_PV "|----", NewLine_PV]@
usualSeparatorInPCSIList :: PCLT_ParamVal
usualSeparatorInPCSIList = PVList_PV [NewLine_PV, PlainText_PV "|----", NewLine_PV]