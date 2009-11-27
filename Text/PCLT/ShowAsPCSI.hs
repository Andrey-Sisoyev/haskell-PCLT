{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | WARNING: Creating an instance of 'ShowAsPCSI' for 'String', @ByteString@s
-- and/or other text types is not recommended. Use of such instaniations
-- would dread strictness of templates catalog structure - make it's
-- use workaroundish, less systematic and less strict, which is a way
-- to badmade applications.
module Text.PCLT.ShowAsPCSI where

import Text.PCLT.PCSI
import Text.PCLT.Parser.AdvancedSepBy

-- | Our extended version of @Show@ class
class ShowAsPCSI t where
     showAsPCSI :: t -> PCSI
