{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

module Text.PCLT.Parser.AdvancedSepBy where

import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import Data.MyHelpers
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import System.IO.Unsafe
import Text.PCLT.Parser.ParserInternals

-- for cases like "Hi, $$ParamKeyName1$$!\nHow was your day, $$ParamKeyName2$$? ##CompositeName1## ##CompositeName2##"

type SeparationMarkerIdx = Int
data SeparatedSectorMarker =
          Beginning_SSM
        | EOF_SSM
        | InnerMarker_SSM SeparationMarkerIdx
        | Error_SSM StandartMarkingStrategyError -- i know, a bad style to make it dependant of a very specific marking strategy... no time for any better now
       deriving (Eq, Ord)
type MarkedChunkLength = Int64

manyTill_EOForEitherOf :: Parser Char -> [Parser Lazy.ByteString] -> Parser (SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)
manyTill_EOForEitherOf !p !ends_list = Parser $! manyTill_EOForEitherOf' B.empty 0
        where
          alternatives_set = map (\ (end_p, i) -> end_p >> return (InnerMarker_SSM i) ) (zip ends_list [1..])
          ---------------------------
          manyTill_EOForEitherOf' :: Lazy.ByteString -> MarkedChunkLength -> State ParserState (ParserResult (SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength))
          manyTill_EOForEitherOf' !soFar !len = do
                saved <- get
                result <- runParser $ choice alternatives_set
                let returnResult marker = return $ Success (marker, B.pack $ B.unpack $ B.reverse soFar, len) -- any better way to compact many chunks into one?..
                    if_bad = do
                       put saved
                       ps_c <- runParser p
                       case ps_c of
                           Success    c -> manyTill_EOForEitherOf' (B.cons c soFar) (len + 1) -- would use cons' if had time to ensure, that it would keeps tail-recursion
                           IllegalInput -> returnResult $ Error_SSM $ UnallowedCharacter_SMSE (B.head $ pstInput saved)
                           ReachedEOF   -> returnResult     EOF_SSM
                case result of
                    Success a    -> returnResult a
                    IllegalInput -> if_bad
                    ReachedEOF   -> if_bad

type New_Active_Sep_SSM      = SeparatedSectorMarker
type Previous_Active_Sep_SSM = SeparatedSectorMarker
type Current_Sep_SSM         = SeparatedSectorMarker
type Current_Chunk_SSM       = SeparatedSectorMarker
type SectorMarkingStrategy = (Previous_Active_Sep_SSM, Current_Sep_SSM) -> (Current_Chunk_SSM, New_Active_Sep_SSM)

sepBySome :: Parser Char -> SectorMarkingStrategy -> [Parser Lazy.ByteString] -> Parser [(SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)]
sepBySome !p !sectorMarkingStrategy !sep_str_list = _getMarkedChunks1 [] Beginning_SSM
        where
          _getMarkedChunks1 :: [(SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)] -> SeparatedSectorMarker -> Parser [(SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)]
          _getMarkedChunks1 chunks_so_far prev_active_sep_marker =
               do (cur_sep_marker, x, len) <- manyTill_EOForEitherOf p sep_str_list
                  let (cur_chunk_marker, new_active_sep_marker) = sectorMarkingStrategy (prev_active_sep_marker, cur_sep_marker)
                      new_chunks_set = (cur_chunk_marker, x, len) : chunks_so_far
                  case new_active_sep_marker of
                      EOF_SSM -> return $ reverse new_chunks_set
                      _       -> _getMarkedChunks1 new_chunks_set new_active_sep_marker

data StandartMarkingStrategyError =
          InputAfterEOF_SMSE
        | BeginningMNotInTheBeginning_SMSE
        | OverlappingMarkedChunks_SMSE
        | UnsupportedMarkersSequence_SMSE SeparatedSectorMarker SeparatedSectorMarker
        | OpenMarkerAtEOF_SMSE SeparationMarkerIdx
        | UnallowedCharacter_SMSE Char
       deriving (Eq, Ord)

standardMarkingStrategy :: SectorMarkingStrategy
standardMarkingStrategy (prev_active_sep_marker, cur_sep_marker) =
        case (prev_active_sep_marker, cur_sep_marker) of
            (    Beginning_SSM,           EOF_SSM) -> (InnerMarker_SSM 0, EOF_SSM)
            (          EOF_SSM,                 _) -> (Error_SSM InputAfterEOF_SMSE, EOF_SSM)
            (    Beginning_SSM, InnerMarker_SSM i) -> (InnerMarker_SSM 0, InnerMarker_SSM  i)
            (InnerMarker_SSM i,           EOF_SSM) -> case i == 0 of
                                                          True  -> (InnerMarker_SSM 0, EOF_SSM)
                                                          False -> (Error_SSM $ OpenMarkerAtEOF_SMSE i, EOF_SSM)
            (InnerMarker_SSM i,     Beginning_SSM) -> (InnerMarker_SSM i, Error_SSM BeginningMNotInTheBeginning_SMSE)
            (InnerMarker_SSM i, InnerMarker_SSM j) ->
                 case i == j of
                     True  -> (InnerMarker_SSM i, InnerMarker_SSM 0)
                     False -> case i == 0 of
                                  True  -> (InnerMarker_SSM i, InnerMarker_SSM j)
                                  False -> (Error_SSM OverlappingMarkedChunks_SMSE, InnerMarker_SSM j)
            _ -> let err_m = Error_SSM $ UnsupportedMarkersSequence_SMSE prev_active_sep_marker cur_sep_marker
                  in (err_m, err_m)

standardMarkingStrategyFix_StripEmptyChunks :: [(SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)] -> [(SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)]
standardMarkingStrategyFix_StripEmptyChunks = foldr foldr_f []
        where
          foldr_f marked_chunk accum =
                case marked_chunk == (InnerMarker_SSM 0, B.empty, 0) of
                    True  -> accum
                    False -> marked_chunk : accum

type ChunkIndexInList_ = Int
retrieveNonPlainMarkingsMap :: [(SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)] -> M.Map SeparatedSectorMarker [(Lazy.ByteString, ChunkIndexInList_)]
retrieveNonPlainMarkingsMap marked_seq = fst $ foldl foldr_f (M.empty, 0) marked_seq
        where
          foldr_f (accum, ind) marked_chunk =
                case marked_chunk of
                    (InnerMarker_SSM i, str, _) ->
                        case i == 0 of
                            False -> ( M.unionWith
                                                (++)
                                                accum
                                                ( M.singleton (InnerMarker_SSM i) [(str, ind)])
                                     , ind + 1
                                     )
                            True  -> (accum, ind + 1)
                    _ -> (accum, ind + 1)

getListOfMarkings :: M.Map SeparatedSectorMarker [(Lazy.ByteString, ChunkIndexInList_)] -> Int -> [(Lazy.ByteString, ChunkIndexInList_)]
getListOfMarkings m i = case M.lookup (InnerMarker_SSM i) m of
                            Just l  -> l
                            Nothing -> []

retrieveErrorsMarkingsList :: [(SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)] -> [(SeparatedSectorMarker, Lazy.ByteString, ChunkIndexInList_)]
retrieveErrorsMarkingsList marked_seq = fst $ foldl foldr_f ([], 0) marked_seq
        where
          foldr_f (accum, ind) marked_chunk =
                case marked_chunk of
                    (Error_SSM err_msg, str, _) -> ((Error_SSM err_msg, str, ind): accum, ind + 1)
                    _ -> (accum, ind + 1)

{-
-- \n == "\\n"  ==> "\x0d\x0a" or whatever nl is
-- \\ == "\\\\" ==> "\\" = \
translateEscapes :: Lazy.ByteString -> Lazy.ByteString -> (Lazy.ByteString, MarkedChunkLength)
translateEscapes nl s = (B.concat [new_s, B.pack last_state], chars_less)
        where
            nl_len = B.length nl - 2
            (new_s, last_state, chars_less) = B.foldl
                                      (\ (accum, state, cl) ch ->
                                                case state of
                                                    [] -> case ch of
                                                              '\\' -> (           accum, '\\':[], cl)
                                                              _    -> (B.cons' ch accum,      [], cl)
                                                    '\\':[] ->
                                                          case ch of
                                                              '\\' -> (B.cons'  '\\'    accum ,              [], cl + 1)
                                                              'n'  -> (B.concat    [nl, accum],              [], cl + nl_len)
                                      )
                                      (B.empty, [], 0)
                                      s

translateEscapes_inTheListOfMarkings :: Lazy.ByteString -> [(SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)] -> [(SeparatedSectorMarker, Lazy.ByteString, MarkedChunkLength)]
translateEscapes_inTheListOfMarkings nl marked_seq =
        map (\ chu@(m, s, l)->
                        case m of
                            InnerMarker_SSM i -> case i == 0 of
                                                     True  -> let (n_s, l_mod) = translateEscapes nl s in (m, n_s, l - l_mod)
                                                     False -> chu
                            _ -> chu
            )
            marked_seq
-}
------------------------------------------------------------
------------------------------------------------------------

insertInsteadOf_inLBS :: (Lazy.ByteString, Lazy.ByteString) -> Lazy.ByteString -> Lazy.ByteString
insertInsteadOf_inLBS (old_sep, new_sep) lbs =
            case parse (sepBySome anyChar standardMarkingStrategy [stringLBS old_sep]) lbs of
                ( IllegalInput       , _ ) -> error "This should have never happened! Unexpected error in 'insertInsteadOf_inLBS': parse returned unexpected result!"
                ( ReachedEOF         , _ ) -> error "This should have never happened! Unexpected error in 'insertInsteadOf_inLBS': parse returned unexpected result!"
                ( Success chunks_list, _ ) ->
                        B.concat $ intersperse new_sep $ map snd3 chunks_list
------------------------------------------------------------
------------------------------------------------------------

instance Show SeparatedSectorMarker where
        show sms = _prefix ++ _body ++ "."
              where
                _prefix = "Sector separation marker: "
                _body   = case sms of
                              Beginning_SSM -> "input beginning"
                              EOF_SSM       -> "input ending"
                              InnerMarker_SSM sm_idx -> "marker #" ++ show sm_idx
                              Error_SSM smse -> "error '" ++ show smse ++ "'"

instance Show StandartMarkingStrategyError where
        show smse = _prefix ++ _body
           where
              _prefix = "An error occured when parsing a marked text. Marking failed: "
              _body =
                case smse of
                   InputAfterEOF_SMSE -> "input is not allowed after EOF."
                   BeginningMNotInTheBeginning_SMSE -> "beginning is allowed to occur only as the first input marker."
                   OverlappingMarkedChunks_SMSE -> "the marking strategy doesn't allow overlapping marked text chunks."
                   UnsupportedMarkersSequence_SMSE prev_active_sm cur_sm -> "the marking strategy doesn't support markers sequence (active previous separator marker -> current separator marker): " ++ show prev_active_sm ++ " -> " ++ show cur_sm ++ " ."
                   OpenMarkerAtEOF_SMSE sm_idx -> "text ends with unclosed chunk of nonplain (marker index: " ++ show sm_idx ++ ") text."
