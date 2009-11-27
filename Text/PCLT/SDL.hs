{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | Simply saying: with SDL we regulate, how much some Reader (of our
-- generated messages) wishes (is allowed) to see.
--
-- \"SDL\" is an abbreviation for \"Show Detalization Level\".
-- Perhaps it better sound \"Representation Detalization Level\", but it
-- was too late to change term - too many variables has names @sdl@,
-- and author is too lazy to change them on @rdl@s.
-- However, \"Representation Detalization Level\" version is sometimes used -
-- it is to be understood as synonym to SDL.
-- Term \"Detailizable content (message)\" in this package has a following
-- meaning: some content, representing which it is possible to regulate,
-- in how much of details it is to be represented.
module Text.PCLT.SDL where

import Data.MyHelpers
import Data.Typeable

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

-- | @Zero_SDL < One_SDL < SDL Int < InfinitelyBig_SDL@
data ShowDetalizationLevel =
        Zero_SDL
      | One_SDL
      | SDL Int
      | InfinitelyBig_SDL
     deriving (Typeable)

-- | SDL may be seen in two different moduses:
--
-- * as a requirement for representation by a localizable template:
-- \"SDL of repesentation reciever must be equal or bigger then that,
-- orelse message from this template won't be generated\"
--
-- * as an allocateion to a repesentation reciever.
data SDLModus = Allocated_SDLM | Required_SDLM deriving (Eq, Show, Typeable)

-- | In case, if SDL (of reciever of template requirement) is unclear,
-- and if it is allowed by config - then SDL gets assigned according to margin:
--
-- * if modus is allocation to reciever - the we give him @Zero_SDL@ (minimum)
--
-- * if modus is requirement by template - the we give
-- him @InfinitelyBig_SDL@ (maximum)
marginOfSDLModus :: SDLModus -> ShowDetalizationLevel
marginOfSDLModus sdlm =
            case sdlm of
                Required_SDLM  -> InfinitelyBig_SDL
                Allocated_SDLM -> Zero_SDL

strict_str2sdl :: String -> Maybe ShowDetalizationLevel
strict_str2sdl x =
                if x == "+inf" then Just InfinitelyBig_SDL
           else if x == "one"    then Just One_SDL
           else if x == "zero"   then Just Zero_SDL
           else case str2Numeric x of
                    Just  n -> Just $ SDL n
                    Nothing -> Nothing

instance Show ShowDetalizationLevel where
        show InfinitelyBig_SDL = "+inf"
        show Zero_SDL          = "zero"
        show One_SDL           = "one"
        show (SDL i)           = show i

instance Eq ShowDetalizationLevel where
        sdl1 == sdl2 =
                case (sdl1, sdl2) of
                    (InfinitelyBig_SDL, InfinitelyBig_SDL) -> True
                    (Zero_SDL         , Zero_SDL         ) -> True
                    (One_SDL          , One_SDL          ) -> True
                    (SDL i1           , SDL i2           ) -> i1 == i2
                    _ -> False

instance Ord ShowDetalizationLevel where
        sdl1 `compare` sdl2 =
                case (sdl1, sdl2) of
                    (InfinitelyBig_SDL, InfinitelyBig_SDL) -> EQ
                    (_                , InfinitelyBig_SDL) -> LT
                    (InfinitelyBig_SDL, _                ) -> GT
                    (Zero_SDL         , Zero_SDL         ) -> EQ
                    (Zero_SDL         , _                ) -> LT
                    (_                , Zero_SDL         ) -> GT
                    (One_SDL          , One_SDL          ) -> EQ
                    (One_SDL          , _                ) -> LT
                    (_                , One_SDL          ) -> GT
                    (SDL            i1, SDL i2           ) -> i1 `compare` i2

