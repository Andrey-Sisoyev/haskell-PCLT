{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | Taken from Lucu-0.4 package. The differences from the original:
--
-- (1) this version exports everything,
--
-- (2) added stringLBS,
--
-- (3) - Data.ByteString.Lazy.UTF8.Unified instead or usual lazy
-- ByteString.
-- _________________________
-- Here are the original authors' (PHO) comments:
--
-- Yet another parser combinator. This is mostly a subset of
-- "Text.ParserCombinators.Parsec" but there are some differences:
--
-- * This parser works on 'Data.ByteString.Base.LazyByteString'
--   instead of 'Prelude.String'.
--
-- * Backtracking is the only possible behavior so there is no \"try\"
--   action.
--
-- * On success, the remaining string is returned as well as the
--   parser result.
--
-- * You can choose whether to treat reaching EOF (trying to eat one
--   more letter at the end of string) a fatal error or to treat it a
--   normal failure. If a fatal error occurs, the entire parsing
--   process immediately fails without trying any backtracks. The
--   default behavior is to treat EOF fatal.
--
-- In general, you don't have to use this module directly.

{-# LANGUAGE BangPatterns #-}

module Text.PCLT.Parser.ParserInternals where

import           Control.Monad.State.Strict
import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Foldable as Fold
import           Data.Int
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>))

-- |@'Parser' a@ is obviously a parser which parses and returns @a@.
newtype Parser a = Parser {
      runParser :: State ParserState (ParserResult a)
    }


data ParserState
    = PST {
        pstInput      :: Lazy.ByteString
      , pstIsEOFFatal :: !Bool
      }
    deriving (Eq, Show)


data ParserResult a = Success !a
                    | IllegalInput -- 受理出來ない入力があった
                    | ReachedEOF   -- 限界を越えて讀まうとした
                      deriving (Eq, Show)


--  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
instance Monad Parser where
    p >>= f = Parser $! do saved <- get -- 失敗した時の爲に状態を保存
                           result <- runParser p
                           case result of
                             Success a    -> runParser (f a)
                             IllegalInput -> do put saved -- 状態を復歸
                                                return IllegalInput
                             ReachedEOF   -> do put saved -- 状態を復歸
                                                return ReachedEOF
    return !x = Parser $! return $! Success x
    fail _    = Parser $! return $! IllegalInput

-- |@'failP'@ is just a synonym for @'Prelude.fail'
-- 'Prelude.undefined'@.
failP :: Parser a
failP = fail undefined

-- |@'parse' p bstr@ parses @bstr@ with @p@ and returns @(# result,
-- remaining #)@.
parse :: Parser a -> Lazy.ByteString -> ( ParserResult a, Lazy.ByteString )
parse !p input -- input は lazy である必要有り。
    = let (!result, state') = runState (runParser p) (PST input True)
      in
        ( result, pstInput state' ) -- pstInput state' も lazy である必要有り。

-- |@'parseStr' p str@ packs @str@ and parses it.
parseStr :: Parser a -> String -> ( ParserResult a, Lazy.ByteString )
parseStr !p input -- input は lazy である必要有り。
    = parse p (B.pack input)


anyChar :: Parser Char
anyChar = Parser $!
          do state@(PST input _) <- get
             if B.null input then
                 return ReachedEOF
               else
                 do put $! state { pstInput = B.tail input }
                    return (Success $! B.head input)


eof :: Parser ()
eof = Parser $!
      do PST input _ <- get
         if B.null input then
             return $! Success ()
           else
             return IllegalInput

-- |@'allowEOF' p@ makes @p@ treat reaching EOF a normal failure.
allowEOF :: Parser a -> Parser a
allowEOF !f
    = Parser $! do saved@(PST _ isEOFFatal) <- get
                   put $! saved { pstIsEOFFatal = False }

                   result <- runParser f

                   state <- get
                   put $! state { pstIsEOFFatal = isEOFFatal }

                   return result


satisfy :: (Char -> Bool) -> Parser Char
satisfy !f
    = do c <- anyChar
         if f c then
             return c
           else
             failP


char :: Char -> Parser Char
char !c = satisfy (== c)


string :: String -> Parser String
string !str
    = let bs  = B.pack str
          len = B.length bs
      in
        Parser $!
        do st <- get
           let (bs', rest) = B.splitAt len $ pstInput st
               st'         = st { pstInput = rest }
           if B.length bs' < len then
               return ReachedEOF
             else
               if bs == bs' then
                   do put st'
                      return $ Success str
               else
                   return IllegalInput


infixr 0 <|>

-- |This is the backtracking alternation. There is no non-backtracking
-- equivalent.
(<|>) :: Parser a -> Parser a -> Parser a
(!f) <|> (!g)
    = Parser $! do saved  <- get -- 状態を保存
                   result <- runParser f
                   case result of
                     Success a    -> return $! Success a
                     IllegalInput -> do put saved -- 状態を復歸
                                        runParser g
                     ReachedEOF   -> if pstIsEOFFatal saved then
                                         do put saved
                                            return ReachedEOF
                                     else
                                         do put saved
                                            runParser g


choice :: [Parser a] -> Parser a
choice = foldl (<|>) failP


oneOf :: [Char] -> Parser Char
oneOf = foldl (<|>) failP . map char


notFollowedBy :: Parser a -> Parser ()
notFollowedBy !p
    = Parser $! do saved  <- get -- 状態を保存
                   result <- runParser p
                   case result of
                     Success _    -> do put saved -- 状態を復歸
                                        return IllegalInput
                     IllegalInput -> do put saved -- 状態を復歸
                                        return $! Success ()
                     ReachedEOF   -> do put saved -- 状態を復歸
                                        return $! Success ()


digit :: Parser Char
digit = do c <- anyChar
           if c >= '0' && c <= '9' then
               return c
             else
               failP


hexDigit :: Parser Char
hexDigit = do c <- anyChar
              if (c >= '0' && c <= '9') ||
                 (c >= 'a' && c <= 'f') ||
                 (c >= 'A' && c <= 'F') then
                  return c
                else
                  failP


many :: forall a. Parser a -> Parser [a]
many !p = Parser $!
          do state <- get
             let ( result, state' ) = many' state Seq.empty
             put state'
             return result
    where
      many' :: ParserState -> Seq a -> ( ParserResult [a], ParserState )
      many' !st !soFar
          = case runState (runParser p) st of
              (Success a,  st') -> many' st' (soFar |> a)
              (IllegalInput, _) -> ( Success (Fold.toList soFar), st )
              (ReachedEOF  , _) -> if pstIsEOFFatal st then
                                       ( ReachedEOF, st )
                                   else
                                       ( Success (Fold.toList soFar), st )

manyChar :: Parser Char -> Parser Lazy.ByteString
manyChar !p = Parser $!
              do state <- get
                 case scan' state 0 of
                   Success len
                       -> do let (bs, rest) = B.splitAt len (pstInput state)
                                 state'     = state { pstInput = rest }
                             put state'
                             return $ Success bs
                   ReachedEOF
                       -> if pstIsEOFFatal state then
                              return ReachedEOF
                          else
                              error "internal error"
                   _   -> error "internal error"
    where
      scan' :: ParserState -> Int64 -> ParserResult Int64
      scan' !st !soFar
          = case runState (runParser p) st of
              (Success _   , st') -> scan' st' (soFar + 1)
              (IllegalInput, _  ) -> Success soFar
              (ReachedEOF  , _  ) -> if pstIsEOFFatal st then
                                         ReachedEOF
                                     else
                                         Success soFar


many1 :: Parser a -> Parser [a]
many1 !p = do x  <- p
              xs <- many p
              return (x:xs)


count :: Int -> Parser a -> Parser [a]
count !n !p = Parser $! count' n p Seq.empty

-- This implementation is rather ugly but we need to make it
-- tail-recursive to avoid stack overflow.
count' :: Int -> Parser a -> Seq a -> State ParserState (ParserResult [a])
count' 0  _  !soFar = return $! Success $! Fold.toList soFar
count' !n !p !soFar = do saved  <- get
                         result <- runParser p
                         case result of
                           Success a    -> count' (n-1) p (soFar |> a)
                           IllegalInput -> do put saved
                                              return IllegalInput
                           ReachedEOF   -> do put saved
                                              return ReachedEOF


-- def may be a _|_
option :: a -> Parser a -> Parser a
option def !p = p <|> return def


sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy !p !sep = sepBy1 p sep <|> return []


sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 !p !sep
    = do x  <- p
         xs <- many $! sep >> p
         return (x:xs)


sp :: Parser Char
sp = char ' '


ht :: Parser Char
ht = char '\t'


crlf :: Parser String
crlf = string "\x0d\x0a"

stringLBS :: Lazy.ByteString -> Parser Lazy.ByteString
stringLBS !bs
    = let len = B.length bs
      in
        Parser $!
        do st <- get
           let (bs', rest) = B.splitAt len $ pstInput st
               st'         = st { pstInput = rest }
           case B.length bs' < len of
               True -> return ReachedEOF
               False ->
                   case bs == bs' of
                       True -> do
                           put st'
                           return $ Success bs
                       False -> return IllegalInput