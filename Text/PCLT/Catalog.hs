{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

module Text.PCLT.Catalog where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import Data.Int
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.MyHelpers
import Data.Typeable
import Text.PCLT.Config
import Text.PCLT.Template
import Text.PCLT.CommonTypes

type PCLT_CatalogID = Int

data PCLT_Catalog = PCLT_Catalog {
                  pcltcCatalogID   :: PCLT_CatalogID
                , pcltcCatalogMap  :: PCLT_CatalogMap
                -- | It is highly recommended not to change this param
                -- values after catalog is formed, since it's easy to
                -- spoil catalog content that way.
                , pcltcInnerConfig :: PCLT_InnerConfig
                }
     deriving (Show, Typeable)

catInstMaxLen :: PCLT_Catalog -> Int64
catInstMaxLen = pcsInstaniationResultMaxSize . pcltcInnerConfig
catDfltLng :: PCLT_Catalog -> LanguageName
catDfltLng = pcsDefaultLanguage . pcltcInnerConfig
catStrictOrient :: PCLT_Catalog -> StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets
catStrictOrient = pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets . pcltcInnerConfig
catSize :: PCLT_Catalog -> Int
catSize c = M.size $ pcltcCatalogMap c

-----------------------------------------
-- * Common errors related to catalog, used by diffent functions
data TplDefaultLngIsMissing_PCLTE     = TplDefaultLngIsMissing_PCLTE PCLT_CompositeKey               deriving (Show, Typeable)
data DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_PCLTE
                                      = DefaultLngTplComponentsParamsSetsDiffersFromOnesOfNondefault_PCLTE PCLT_CompositeKey LanguageName
                                                                                                     deriving (Show, Typeable)
data RequiredCompositeIsMissing_PCLTE = RequiredCompositeIsMissing_PCLTE RequiredCompositeKey        deriving (Show, Typeable)
data RequiredByRequirerCompositeIsMissing_PCLTE
                                      = RequiredByRequirerCompositeIsMissing_PCLTE RequirerCompositeKey RequiredCompositeIsMissing_PCLTE
                                                                                                     deriving (Show, Typeable)
data CompositionCycle_PCLTE           = CompositionCycle_PCLTE PCLT_CompositeKey [PCLT_CompositeKey] deriving (Show, Typeable)
data TplUniquenessViol_PCLTE          = TplUniquenessViol_PCLTE PCLT_ID [LanguageName]               deriving (Show, Typeable)

type MainUnit_SDL  = PCLT_ShowDetalizationLevel
type AddedUnit_SDL = PCLT_ShowDetalizationLevel
data DifferentSDLs_PCLTE              = DifferentSDLs_PCLTE PCLT_ID (MainUnit_SDL, AddedUnit_SDL)    deriving (Show, Typeable)
----
data ErrorWithPCSCatalog            a = ErrorWithPCSCatalog PCLT_CatalogID a                         deriving (Show, Typeable)

-----------------------------------------------------------------

