{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- |The analogue to ordinary lazy 'ByteString', but with a constraint on size,
-- and some routines, thet respects the constraint.
module Text.ConstraintedLBS where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import Data.Int
import Data.List
import Data.MyHelpers
import System.IO.Unsafe
import Text.PCLT.Parser.AdvancedSepBy (insertInsteadOf_inLBS)

-- |The analogue to ordinary lazy 'ByteString', but with a constraint on size.
data CLBS = CLBS {
                  clbsLBS            :: Lazy.ByteString
                , clbsLen            :: Int64
                , clbsMaxLen         :: Int64
                -- | Bytestring is finalized, when something is appended to it,
                -- that makes @clbsMaxLen@ to be reached. The last 3 bytes
                -- of a finalized bytestring are always made \"...\"
                -- (by routines of this module, that manage finalization).
                , clbsFinalized_isit :: Bool
                }

clbsFreeSpaceLeft :: CLBS -> Int64
clbsFreeSpaceLeft clbs =
        case clbsFinalized_isit clbs of
            True  -> 0
            False -> clbsMaxLen clbs - clbsLen clbs

-- | Make a 'CLBS' with a specified maximum on length.
newCLBS :: Int64 -> CLBS
newCLBS sp_av = CLBS {
                    clbsLBS            = B.empty
                  , clbsLen            = 0
                  , clbsMaxLen         = sp_av
                  , clbsFinalized_isit = False
                  }

-- | Append first lazy 'ByteString' (given in tuple with
-- it's (trusted) length) to a 'CLBS'. If the result of appending
-- violates clbsMaxLen constraint, then the content gets truncated,
-- tailed with \"...\" and CLBS is finalized (nothing more can be added to it)
addToCLBS_1 :: (Lazy.ByteString, Int64) -> CLBS -> CLBS
addToCLBS_1 add clbs =
        case clbsFinalized_isit clbs of
            True  -> clbs
            False ->
                let (new_lbs, new_len, finalizsed_isit) = concatTruncedLiteraryLBS (apFrom2ple (clbsLBS, clbsLen) clbs) add (clbsFreeSpaceLeft clbs)
                 in clbs { clbsLBS            = new_lbs
                         , clbsLen            = new_len
                         , clbsFinalized_isit = finalizsed_isit
                         }

-- | Produce an empty 'CLBS', that would have the maximum of length equal
-- to free space left available in specified 'CLBS'.
freeSpaceCLBS :: CLBS -> CLBS
freeSpaceCLBS clbs = clbs {
                    clbsLBS    = B.empty
                  , clbsLen    = 0
                  , clbsMaxLen = clbsMaxLen clbs - clbsLen clbs
                  }

-- | Append first to second. If the result of appending
-- violates clbsMaxLen constraint, then the content gets truncated,
-- tailed with \"...\" and CLBS is finalized (nothing more can be added to it)
addToCLBS_2 :: CLBS -> CLBS -> CLBS
addToCLBS_2 add_clbs main_clbs = ((clbsLBS, clbsLen) `apFrom2ple` add_clbs) `addToCLBS_1` main_clbs

-- | Make 'CLBS' contain specified 'ByteString', keeping the length constraint.
-- If the specified bytestring violates clbsMaxLen constraint, then
-- the content gets truncated, tailed with \"...\"
-- and CLBS is finalized (nothing more can be added to it)
adjustCLBS_to :: CLBS -> Lazy.ByteString -> CLBS
adjustCLBS_to clbs new_lbs =
        let new_lbs_len = B.length new_lbs
            max_len     = max 0 (clbsMaxLen clbs)
            tri_p       = B.take (min 3 max_len) (B.pack "...")
         in case new_lbs_len > max_len of
                False -> clbs {
                            clbsLBS = new_lbs
                          , clbsLen = new_lbs_len
                          , clbsFinalized_isit = False
                          }
                True  -> clbs {
                            clbsLBS = B.concat [B.take (max_len - 3) new_lbs, tri_p]
                          , clbsLen = max_len
                          , clbsFinalized_isit = True
                          }

-- | Given @insertInsteadOf_inCLBS (old_separator, new_separator) clbs@,
-- function replaces every occurence of @old_separator@ on @new_separator@
-- respecting length constraint (and possibly finalizing 'CLBS').
insertInsteadOf_inCLBS :: (Lazy.ByteString, Lazy.ByteString) -> CLBS -> CLBS
insertInsteadOf_inCLBS (old_sep, new_sep) clbs =
        let new_lbs = insertInsteadOf_inLBS (old_sep, new_sep) (clbsLBS clbs)
         in clbs `adjustCLBS_to` new_lbs

instance Show CLBS where
        show clbs = B.unpack $ clbsLBS clbs

-- | Empify, and if finalized, make not.
resetCLBS :: CLBS -> CLBS
resetCLBS clbs = clbs { clbsFinalized_isit = False, clbsLen = 0, clbsLBS = B.empty}

-- * Some type aliaces

type StdOut_CLBS = CLBS
type StdErr_CLBS = CLBS
type StdOutAndErr_CLBS = CLBS

-- * Base

----------------------------------------------------------------

-- |Append to first lazy bytestring the second one. The available space
-- for append is constrainted by the 3rd argument. Every bytestring must
-- be specified in tuple together with it's length (this is done to
-- speed up by reducing repeating @length@ calls), which is trusted.
-- If length of added string is bigger then available for append,
-- the result will be truncated and will have a \"...\" tail.
--
-- Function returns resulting ByteString, it's length and boolean indicating
-- if length of added string was bigger than available for append space.
concatTruncedLiteraryLBS :: (Lazy.ByteString, Int64) -> (Lazy.ByteString, Int64) -> Int64 -> (Lazy.ByteString, Int64, Bool)
concatTruncedLiteraryLBS (main_ch, main_len) (add_ch, add_len) _space_for_add =
      let space_for_add = max _space_for_add 0
          tri_p = take (min 3 $ fromIntegral space_for_add) "..."
       in case add_len > space_for_add of
              False -> (B.concat [main_ch, add_ch], main_len + add_len, False)
              True  ->
                 let lbs_list =
                         case space_for_add `compare` 3 of
                             LT -> [B.take (main_len + space_for_add - 3) main_ch                                   ]
                             EQ -> [                                      main_ch                                   ]
                             GT -> [                                      main_ch, B.take (space_for_add - 3) add_ch]
                  in (B.concat (lbs_list ++ [B.pack tri_p]), main_len + space_for_add, True)
