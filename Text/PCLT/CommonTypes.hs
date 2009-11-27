{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

module Text.PCLT.CommonTypes where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)

-- | It is highly recommended to use /ISO 639-3/ here - I mean
-- it was originally intended to do so, and now a related package PCLT-DB
-- works only with 3-letters (not bigger) languages names -
-- there is a constraint in DB for that.
type LanguageName = String

type PCLT_ID                = String
-- | Localizable template ID.
type PCLT_ParamKey          = String
-- | Here and in many places of package by
-- \"composite\" is meant \"template which is included in
-- composed template\".
type PCLT_CompositeKey      = PCLT_ID
type PCLT_SuperCompositeKey = PCLT_CompositeKey
type PCLT_SubCompositeKey   = PCLT_CompositeKey
type RequiredCompositeKey   = PCLT_CompositeKey
type RequirerCompositeKey   = PCLT_CompositeKey

type ParamName_LBS = Lazy.ByteString