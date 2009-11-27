{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK ignore-exports, prune #-}

-- | This module unifies some parts of @bytestring@ (as is in @0.9.1.5@
-- version) and @utf8-string@ (as is in @0.3.6@ version) packages - it
-- exports "Data.ByteString.Lazy.Char8", "Data.ByteString.Lazy.UTF8" and
-- "System.IO.UTF8", while hiding from the first everything that is
-- to be found in latter two. Ofcourse "System.IO.UTF8" routines
-- are wrapped to work with lazy 'ByteStrings' instead of 'Strings'.
-- This way we get a module an "Data.ByteString.Lazy.Char8" alternative
-- that won't corrupt Unicode symbols, an that will make
-- a proper IO with them.
--
-- This module is intended to be imported qualified, to avoid name
-- clashes with Prelude functions. eg.
--
-- @import qualified Data.ByteString.Lazy.UTF8.Unified as B@
--
-- or even
--
-- > import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
-- > import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
module Data.ByteString.Lazy.UTF8.Unified (
          module Data.ByteString.Lazy.Char8
        , module Data.ByteString.Lazy.UTF8
        , pack
        , unpack
        , Data.ByteString.Lazy.UTF8.Unified.length
        , putStr
        , putStrLn
        , readFile
        , writeFile
        , appendFile
        , interact
        , getContents
        , hGetContents
        ) where

import Control.Monad
import Data.Int
import Data.ByteString.Lazy.UTF8 hiding (length)
import qualified Data.ByteString.Lazy.UTF8 as U (length)
import qualified Data.ByteString.Lazy.UTF8 as Lazy (ByteString)
import Data.ByteString.Lazy.Char8 hiding (
          ByteString
        , pack,unpack
        , uncons, splitAt, take, drop, span, break, putStrLn, putStr, foldl, foldr, length, lines -- change them on Data.ByteString.Lazy.UTF8
        , putStr,putStrLn,readFile,writeFile,appendFile,interact,getContents,hGetContents -- change them on System.IO.UTF8
        , hGet,hGetNonBlocking,hPut -- however, there is no analog for these in UTF8
        )
import Prelude hiding (length, putStr, putStrLn, readFile, writeFile, appendFile, interact, getContents)
import qualified System.IO.UTF8 as UIO
import System.IO (Handle)

pack :: String -> Lazy.ByteString
pack = fromString

unpack :: Lazy.ByteString -> String
unpack = toString

length :: Lazy.ByteString -> Int64
length = fromIntegral . U.length

putStr :: Lazy.ByteString -> IO ()
putStr = UIO.putStr . unpack

putStrLn :: Lazy.ByteString -> IO ()
putStrLn = UIO.putStrLn . unpack

readFile :: FilePath -> IO Lazy.ByteString
readFile fp = pack `liftM` UIO.readFile fp

writeFile :: FilePath -> Lazy.ByteString -> IO ()
writeFile fp lbs = UIO.writeFile fp $ unpack lbs

appendFile :: FilePath -> Lazy.ByteString -> IO ()
appendFile fp lbs = UIO.appendFile fp $ unpack lbs

interact :: (Lazy.ByteString -> Lazy.ByteString) -> IO ()
interact f = UIO.interact (\ s -> unpack $ f $ pack s)

getContents :: IO Lazy.ByteString
getContents = pack `liftM` UIO.getContents

hGetContents :: Handle -> IO Lazy.ByteString
hGetContents h = pack `liftM` UIO.hGetContents h