{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

module Text.PCLT.Config where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import Data.Int
import Text.PCLT.CommonTypes
import Data.Typeable

----------------------------------------------
data StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets =
        StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets {
                  -- | Abbreviation = \"SO\". The exclusions has opposite
                  -- roles for different values of this parameter.
                  --
                  -- * if SO is True ,       be strict with templates
                  -- (when adding them and to a catalog) in such manner,
                  -- that set of composite and set of parameters MUST be
                  -- the same for ALL language localizations under
                  -- one template ID; if for a template any set (of
                  -- parameters or of composites) is different from one
                  -- determined for template in default language, then this
                  -- template will be discarded (not included in catalog)
                  --
                  -- * if SO is False, don't be strict with templates -
                  -- different language versions of one same template are
                  -- allowed to have different sets of composites and
                  -- parameters
                  soStrict_IsIt :: Bool
                  -- |
                  --
                  -- * if SO is True , don't be strict with contents of
                  --      these templates;
                  --
                  -- * if SO is False,       be strict with contents of
                  -- ONLY these templates
                , soExcludingInComposites   :: [PCLT_ID]
                  -- |
                  --
                  -- * if SO is True , don't be strict with occurences of
                  --      these templates as composites;
                  --
                  -- * if SO is False,       be strict with occurences of
                  -- ONLY these templates as composites
                , soExcludingComposites     :: [PCLT_ID]
                  -- |
                  --
                  -- * if SO is True , don't be strict with
                  --      these parameters;
                  --
                  -- * if SO is False,       be strict with
                  -- ONLY these parameters
                , soExcludingParameters     :: [PCLT_ParamKey]
                  -- |
                  --
                  -- * if SO is True , don't be strict with      these
                  -- referenced parameters in referenced templates;
                  --
                  -- * if SO is False,       be strict with ONLY these
                  -- referenced parameters in referenced templates
                , soExcludingCompComposites :: [(PCLT_ID, PCLT_ID)]
                  -- |
                  --
                  -- * if SO is True , don't be strict with      these
                  -- referenced composites in referenced templates;
                  --
                  -- * if SO is False,       be strict with ONLY these
                  -- referenced composites in referenced templates
                , soExcludingCompParameters :: [(PCLT_ID, PCLT_ParamKey)]
        }
      deriving (Read, Show, Typeable)

----------------------------------------------

type PCLT_InnerConfigID = Int
-- | NOT USED, RESERVED FOR FUTURE VERSIONS OF PCLT!
type ReparsingDepth     = Int

data PCLT_InnerConfig = PCLT_InnerConfig {
                pcsInnerConfigID :: PCLT_InnerConfigID
              -- | Symbols sequence denoting wrapping parentheses (identical
              -- for both - openning ang closing), that are to be put
              -- around *composite* name in a template text.
              , pcsCompositePlaceholderWrapper :: Lazy.ByteString
              -- | Symbols sequence denoting wrapping parentheses (identical
              -- for both - openning ang closing), that are to be put
              -- around *parameter* name in a template text.
              , pcsParameterPlaceholderWrapper :: Lazy.ByteString
              -- | For case, when representation reciever uses detalization
              -- level, that isn't enough to meet requirement specified by
              -- the template (from which representation was to be made):
              -- symbols sequence, that PCLT engine must place
              -- instead of content from template/composite.
              , pcsInsuficientDetLevelPlaceholder :: Lazy.ByteString
              -- | Symbols sequence denoting wrapping parentheses (identical
              -- for both - openning ang closing), that are to be put
              -- (by PCLT engine) in representation, where it failed to
              -- make a proper representation from a template piece
              -- (composite or parameter).
              , pcsMarkingErrorPlaceholderWrapper :: Lazy.ByteString
              -- | Catalog default (primary) language.
              , pcsDefaultLanguage                :: LanguageName
              -- | Object:
              --
              -- 1. Should all localizations of template be strictly oriented
              -- on localization in default (primary) language? (in case of
              -- such orientation, NO template localized in a nondefault
              -- language is allowed to have sets of composites and
              -- parameters different from ones specified for template
              -- localization in the default language)
              --
              -- 2. Exclusions - when not to be strict with templates
              -- structures, - if strictness is on;
              -- or when to be strict, - if strictness is off.
              , pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets :: StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets
              -- | Is it allowed to demand representations from catalog, when
              -- referenced template/composite ID isn't known in given
              -- catalog?
              -- In case, if allowed, on place of unknown composite
              -- the engine (when making representation) will simply put
              -- it's (unknown) ID (given that recievers detailization level
              -- statisfies a requirement: it must be maximal (infinitely
              -- big)), followed by (if enabled by other config parameter)
              -- all the parameters used by current instaniation.
              , pcsAllowUntemplatedMessages                :: Bool
              -- | Is it allowed to demand representations from catalog, when
              -- referenced template/composite ID is persistent in catalog,
              -- but not in representation language?
              -- In case, if allowed, on place of unknown composite
              -- the engine (when making representation) will simply put
              -- it's (unknown) ID (given that recievers detailization level
              -- statisfies a requirement: it must be maximal (infinitely
              -- big)), followed by (if enabled by other config parameter)
              -- all the parameters used by current instaniation.
              , pcsAllowUntemplatedLocalizationsOfMessages :: Bool
              -- | If it's allowed to require representation of
              -- a template/composite (or it's localisation), that is absent
              -- in catalog then (given that reciever's detailization level
              -- statisfies a requirement: it must be maximal (infinitely
              -- big)) together with it's (unknown composite) ID:
              -- should there also be put all the parameters (array of
              -- name:value pairs), used by current instaniation?
              , pcsShowAdhocParamsInResultOfUntemplated    :: Bool
              -- | DEFAULT maximal allowed size of representation resulting from
              -- instaniation of a single PCSI. If user gives on the input
              -- his ConstraintedLBS with different constraint on size, then his
              -- constraint is used instead.
              , pcsInstaniationResultMaxSize      :: Int64
              -- | Is it allowed to give an empty value to the field
              -- \"representation detailization level\" ("Text.PCLT.SDL")?
              -- If allowed, then it's value will be chosen automatically
              -- by the margin
              -- depending on modus: maximal (\"+inf\") in case, when it's a
              -- requirement by a template, or minimal (\"zero\"), when it's
              -- an assigment to a reciever of representation
              , pcsAllowEmptySDL_parseItByModusMargin      :: Bool
              -- | Is it allowed to give an unreadable value to the field
              -- \"representation detailization level\" ("Text.PCLT.SDL")?
              -- If allowed,
              -- then it's value will be chosen automatically by the margin
              -- depending on modus: maximal (\"+inf\") in case, when it's a
              -- requirement by a template, or minimal (\"zero\"), when it's
              -- an assigment to a reciever of representation
              , pcsAllowUnreadableSDL_parseIdByModusMargin :: Bool
              -- | If @pcsi2text@ ("Text.PCLT.MakeMessage") function fails to
              -- determine an SDL requiered by used template (due to
              -- referential cycle, missing referenced template,
              -- @PCLT_SDL_Errornous@ or other reason) - should this failure
              -- lead to absolute denial to represent problem template, or
              -- should the @pcsi2text@ treat it like \"+inf\"
              -- (@PCLT_SDL InfinitelyBig_SDL@)? The error will be returned
              -- anyway, but question is purely about output representation.
              , pcsAllowFailureToDetermineSDL_parseIdByModusMargin :: Bool
              -- | Sequence of symbols used, when representing a specialcase
              -- of parameter value - @Newline_PV@ ("Text.PCLT.PCSI").
              -- (Different OSes use different newline strings: in Linux it's
              -- \#10, in M\$ - \#13\#10, MacOS - \#13)
              , pcsNewlineLBS :: Lazy.ByteString
              -- | NOT USED, RESERVED FOR FUTURE VERSIONS OF PCLT!
              -- Maximal allowed depth of reparsing.
              , pcsReparsingDepthMax              :: ReparsingDepth -- reserved for new parameter value type, won't influence catalog functioning in this version
              -- | NOT USED, RESERVED FOR FUTURE VERSIONS OF PCLT!
              -- Maximal allowed size of representation resuling from
              -- instaniating a reparsing result.
              , pcsReparseParameterContentMaxSize :: Int64          -- reserved for new parameter value type, won't influence catalog functioning in this version
        } deriving (Show, Typeable)

{- |
   >  PCLT_InnerConfig {
   >            pcsInnerConfigID = 0
   >          , pcsCompositePlaceholderWrapper = B.pack "##|"
   >          , pcsParameterPlaceholderWrapper = B.pack "@@|"
   >          , pcsInsuficientDetLevelPlaceholder = B.pack "#x#"
   >          , pcsMarkingErrorPlaceholderWrapper = B.pack "/!E!\"
   >          , pcsDefaultLanguage                = "eng"
   >          , pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets =
   >                    StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets {
   >                              soStrict_IsIt = True
   >                            , soExcludingInComposites   = []
   >                            , soExcludingComposites     = []
   >                            , soExcludingParameters     = []
   >                            , soExcludingCompComposites = []
   >                            , soExcludingCompParameters = []
   >                    }
   >          , pcsAllowUntemplatedMessages                = True
   >          , pcsAllowUntemplatedLocalizationsOfMessages = True
   >          , pcsShowAdhocParamsInResultOfUntemplated    = True
   >          , pcsInstaniationResultMaxSize               = 10000000
   >          , pcsAllowEmptySDL_parseItByModusMargin      = False
   >          , pcsAllowUnreadableSDL_parseIdByModusMargin = False
   >          , pcsAllowFailureToDetermineSDL_parseIdByModusMargin = False
   >          , pcsNewlineLBS                              = B.pack "\n"
   >    }
-}
defaultPCLTInnerConfig :: PCLT_InnerConfig
defaultPCLTInnerConfig = PCLT_InnerConfig {
                pcsInnerConfigID = 0
              , pcsCompositePlaceholderWrapper = B.pack "##|" -- f.e., "Hello, @@|Name@@|! ##|InvitationComposite##|"
              , pcsParameterPlaceholderWrapper = B.pack "@@|"
              , pcsInsuficientDetLevelPlaceholder = B.pack "#x#"
              , pcsMarkingErrorPlaceholderWrapper = B.pack "/!E!\\"
              , pcsDefaultLanguage                = "eng"
              , pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets =
                        StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets {
                                  soStrict_IsIt = True
                                , soExcludingInComposites   = []
                                , soExcludingComposites     = []
                                , soExcludingParameters     = []
                                , soExcludingCompComposites = []
                                , soExcludingCompParameters = []
                        }
              , pcsAllowUntemplatedMessages                = True
              , pcsAllowUntemplatedLocalizationsOfMessages = True
              , pcsShowAdhocParamsInResultOfUntemplated    = True
              , pcsInstaniationResultMaxSize      = 10000000
              , pcsAllowEmptySDL_parseItByModusMargin      = False
              , pcsAllowUnreadableSDL_parseIdByModusMargin = False
              , pcsAllowFailureToDetermineSDL_parseIdByModusMargin = False
              , pcsNewlineLBS                     = B.pack "\n"
              , pcsReparsingDepthMax              = 100
              , pcsReparseParameterContentMaxSize = 50000 -- in the next version wanna make reparsing tail-recursive to get rid of this workaroundish cfg param
        }
