{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | This is thought to be imported outside of PCLT package by modules,
-- that use PCLT logics (catalog formation routines and
-- messages generation routines)
--
-- And another briefing on what is PCLT. First is in the top level of
-- Haddock documentation provided for the package.
--
-- _______________
--
-- > export "Text.PCLT.SH__"
--
-- This module provides interfaces to the classes 'ShowAsPCSI' and
-- 'HasStaticRawPCLTs' an all the routines, that usually are used for
-- declaration of their instanitations.
--
-- _______________
--
-- > export "Text.ConstraintedLBS"
--
-- A constrainting (the constraint here is on it's size) wrapper for a
-- lazy 'ByteString' (LBS) - this container is used for messages
-- generated from PCLT templates
--
-- _______________
--
-- > export "Text.PCLT.InitialDefaultCatalog"
--
-- > initDefaultCatalog_3 :: Text.PCLT.Catalog.PCLT_CatalogID -> (StdErr_CLBS, ShowDetalizationLevel, LanguageName) -> (PCLT_Catalog, StdErr_CLBS)
--
-- _______________
--
-- > export "Text.PCLT.Catalog"
--
-- Catalog is a unit with 3 fields: catalog ID, config, and a map by
-- template_IDs of templates, where each template is: minimal SDL required
-- to represent message from this template, and a maps by languages of
-- localized templates.
--
-- _______________
--
-- > export "Text.PCLT.CatalogFromHSRT"
--
-- We want to add to the default catalog some our application specific
-- entries (templates)
--
-- > addFromHSRTToCatalog_2 :: HasStaticRawPCLTs a => a -> PCLT_Catalog -> (StdErr_CLBS, ShowDetalizationLevel, LanguageName) -> (PCLT_Catalog, StdErr_CLBS)
--
-- _______________
--
-- > export "Text.PCLT.CommonTypes"
--
-- Some type aliases, like @'LanguageName' = 'String'@
--
-- _______________
--
-- > export "Text.PCLT.Config"
--
-- Configuration that influences the behaviour of catalog formation routines
-- and messages generation routines.
--
-- _______________
--
-- > export "Text.PCLT.MakeMessage"
--
-- PCSI(template_id + params) + language_name + recepient_SDL + catalog >---(Text.PCLT.MakeMessage)---> message
--
-- _______________
--
-- > export "Text.PCLT.MakeMessage2"
--
-- Some comfort wrappers for "Text.PCLT.MakeMessage"
--
-- _______________
--
-- > export "Text.PCLT.SDL"
--
-- SDL (Show Detalization Level) is a 1-dimensional variable type, built
-- on Int, but extended with additional values:
--
-- @Zero_SDL@ (absolute minimal level) @< One_SDL@ (minimal something) @< SDL Int < InfinitelyBig_SDL@
--
-- With SDL we regulate, how much some Reader (of our generated messages)
-- wishes (is allowed) to see.
--
-- _______________
--
-- > export "Text.PCLT.ShowAsPCSI__"
--
-- Some general instances of 'ShowAsPCSI' class are to be found here
-- (Bool, ShowAsPCSI a => Maybe a, SomeException)
module Text.PCLT (
          module Text.PCLT.SH__
        , module Text.ConstraintedLBS
        , module Text.PCLT.InitialDefaultCatalog
        , module Text.PCLT.Catalog
        , module Text.PCLT.CatalogFromHSRT
        , module Text.PCLT.CommonTypes
        , module Text.PCLT.Config
        , module Text.PCLT.MakeMessage
        , module Text.PCLT.MakeMessage2
        , module Text.PCLT.SDL
        , module Text.PCLT.ShowAsPCSI__
        ) where
-- It's a pitty one can't comment with Haddock on imports...
import           Text.PCLT.SH__
import           Text.ConstraintedLBS
import           Text.PCLT.InitialDefaultCatalog
import           Text.PCLT.Catalog
import           Text.PCLT.CatalogFromHSRT
import           Text.PCLT.CommonTypes
import           Text.PCLT.Config
import           Text.PCLT.MakeMessage
import           Text.PCLT.MakeMessage2
import           Text.PCLT.SDL
import           Text.PCLT.ShowAsPCSI__