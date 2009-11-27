{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

module Data.MyHelpers where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString) -- this strange act is for backward compatibility with already written code
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)

import Data.Char
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe

import Prelude hiding (putStrLn)
import System.IO hiding (putStrLn,hPutStr)
import System.IO.Unsafe

import System.IO.UTF8

----------------------------------

type ErrorMessage = String

infixr 1 <<
(<<)   :: Monad m => m b -> m a -> m b
f << x = x >> f

fst3  :: (a,b,c) -> a
snd3  :: (a,b,c) -> b
thrd3 :: (a,b,c) -> c
fst3  (a,_,_) = a
snd3  (_,b,_) = b
thrd3 (_,_,c) = c

apFor2ple :: (a -> a -> a, b -> b -> b) -> (a,b) -> (a,b) -> (a,b)
apFor2ple (af,bf) (a1,b1) (a2,b2) = (af a1 a2, bf b1 b2)

liftTuple :: Monad m => (m a, m b) -> m (a, b)
lift2ple  :: Monad m => (m a, m b) -> m (a, b)
lift3ple  :: Monad m => (m a, m b, m c) -> m (a, b, c)
lift4ple  :: Monad m => (m a, m b, m c, m d) -> m (a, b, c, d)
liftTuple (ma, mb) = do a <- ma
                        b <- mb
                        return (a,b)
lift2ple = liftTuple
lift3ple (ma, mb, mc) = do a <- ma
                           b <- mb
                           c <- mc
                           return (a,b,c)
lift4ple (ma, mb, mc, md) = do a <- ma
                               b <- mb
                               c <- mc
                               d <- md
                               return (a,b,c,d)

ap22ple :: (a, a)       -> (a -> b) -> (b, b)
ap22ple (a1,a2) fa = (fa a1, fa a2)
ap22pleM :: Monad m => (a, a)       -> (a -> m b) -> m (b, b)
ap22pleM ple f = lift2ple $ ap22ple ple f
ap23ple :: (a, a, a)    -> (a -> b) -> (b, b, b)
ap23ple (a1,a2,a3) fa = (fa a1, fa a2, fa a3)
ap23pleM :: Monad m => (a, a, a)    -> (a -> m b) -> m (b, b, b)
ap23pleM ple f = lift3ple $ ap23ple ple f
ap24ple :: (a, a, a, a) -> (a -> b) -> (b, b, b, b)
ap24ple (a1,a2,a3,a4) fa = (fa a1, fa a2, fa a3, fa a3)
ap24pleM :: Monad m => (a, a, a, a) -> (a -> m b) -> m (b, b, b, b)
ap24pleM ple f = lift4ple $ ap24ple ple f

apFrom2ple :: ((a -> b), (a -> c)) -> a -> (b, c)
apFrom2ple (f1,f2) a = (f1 a, f2 a)
apFrom3ple :: ((a -> b), (a -> c), (a -> d)) -> a -> (b, c, d)
apFrom3ple (f1,f2,f3) a = (f1 a, f2 a, f3 a)
apFrom4ple :: ((a -> b), (a -> c), (a -> d), (a -> e)) -> a -> (b, c, d, e)
apFrom4ple (f1,f2,f3,f4) a = (f1 a, f2 a, f3 a, f4 a)

liftEither6 :: Either er a -> Either er b -> Either er c -> Either er d -> Either er e -> Either er f -> Either er (a,b,c,d,e,f)
liftEither6 er_or_a er_or_b er_or_c er_or_d er_or_e er_or_f =
        case er_or_a of
            Left er -> Left er
            Right a ->
               case er_or_b of
                   Left er -> Left er
                   Right b ->
                        case er_or_c of
                            Left er -> Left er
                            Right c ->
                               case er_or_d of
                                   Left er -> Left er
                                   Right d ->
                                        case er_or_e of
                                            Left er -> Left er
                                            Right e ->
                                               case er_or_f of
                                                   Left er -> Left er
                                                   Right f -> Right (a,b,c,d,e,f)

liftEither5 :: Either er a -> Either er b -> Either er c -> Either er d -> Either er e -> Either er (a,b,c,d,e)
liftEither5 er_or_a er_or_b er_or_c er_or_d er_or_e =
        case er_or_a of
            Left er -> Left er
            Right a ->
               case er_or_b of
                   Left er -> Left er
                   Right b ->
                        case er_or_c of
                            Left er -> Left er
                            Right c ->
                               case er_or_d of
                                   Left er -> Left er
                                   Right d ->
                                        case er_or_e of
                                            Left er -> Left er
                                            Right e -> Right (a,b,c,d,e)

liftEither2 :: Either er a -> Either er b -> Either er (a,b)
liftEither2 er_or_a er_or_b  =
        case er_or_a of
            Left er -> Left er
            Right a ->
               case er_or_b of
                   Left er -> Left er
                   Right b -> Right (a,b)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight = not . isLeft

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "Error occurred, when applied fromLeft to Right!"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Error occurred, when applied fromLeft to Right!"

takeFromMap :: Ord k => k -> Map k a -> (Maybe a, Map k a)
takeFromMap k m =
        let mb_a = M.lookup k m
         in ( mb_a
            , case isJust mb_a of
                  True  -> M.delete k m
                  False -> m
            )

partition_2 :: Ord k => (a -> Bool) -> (a -> b, a -> c) -> Map k a -> (Map k b, Map k c)
partition_2 p (true_f, false_f) m =
        M.foldWithKey
                (\ k a (true_map, false_map) ->
                        case p a of
                            True  -> (M.insert k (true_f a) true_map, false_map)
                            False -> (true_map, M.insert k (false_f a) false_map)
                )
                (M.empty, M.empty)
                m

unionWithKey_2 :: Ord k => (k -> (a, a) -> (a, b)) -> (b -> c -> c) -> c -> Map k a -> Map k a -> (Map k a, c)
unionWithKey_2 unifier side_result_accum_f accum_start m1 m2 =
        foldr (\ (k, a1) (m_accum, side_result_accum) ->
                        case takeFromMap k m_accum of
                            (Nothing, _) -> (M.insert k a1 m_accum, side_result_accum)
                            (Just a2, rest_of_accum_map) ->
                                    let (ar, side_result_add) = unifier k (a1, a2)
                                     in (M.insert k ar rest_of_accum_map, side_result_accum_f side_result_add side_result_accum)
              )
              (m2, accum_start)
              (M.toList m1)

unionWithKey_3 :: Ord k => (k -> (Maybe a, Maybe b) -> (c, d)) -> (d -> d -> d) -> d -> Map k a -> Map k b -> (Map k c, d)
unionWithKey_3 unifierF sideResultAccumF accum_start m1 m2 =
        let (m1_result, accum1, m2_cut) = foldr
                      (\ (k, a) (m_accum, side_result_accum, m2_cut) ->
                                let (mb_b, new_m2_cut) = takeFromMap k m2_cut
                                    (c, d) = unifierF k (Just a, mb_b)
                                    new_side_result_accum = sideResultAccumF side_result_accum d
                                 in (M.insert k c m_accum, new_side_result_accum, new_m2_cut)
                      )
                      (M.empty, accum_start, m2)
                      (M.toList m1)
            (m2_result, accum2) = foldr
                      (\ (k, b) (m_accum, side_result_accum) ->
                                let (c, d) = unifierF k (Nothing, Just b)
                                    new_side_result_accum = sideResultAccumF side_result_accum d
                                 in (M.insert k c m_accum, new_side_result_accum)
                      )
                      (m1_result, accum1)
                      (M.toList m2_cut)
         in (m2_result, accum2)

unionWithKey_4 :: Ord k => (k -> (Maybe a, Maybe b) -> (Maybe c, d)) -> (d -> d -> d) -> d -> Map k a -> Map k b -> (Map k c, d)
unionWithKey_4 unifierF sideResultAccumF accum_start m1 m2 =
        let (m1_result, accum1, m2_cut) = foldr
                      (\ (k, a) (m_accum, side_result_accum, m2_cut) ->
                                let (mb_b, new_m2_cut) = takeFromMap k m2_cut
                                    (mb_c, d) = unifierF k (Just a, mb_b)
                                    new_side_result_accum = sideResultAccumF side_result_accum d
                                 in ( case mb_c of
                                          Just c -> M.insert k c m_accum
                                          Nothing -> m_accum
                                    , new_side_result_accum, new_m2_cut
                                    )
                      )
                      (M.empty, accum_start, m2)
                      (M.toList m1)
            (m2_result, accum2) = foldr
                      (\ (k, b) (m_accum, side_result_accum) ->
                                let (mb_c, d) = unifierF k (Nothing, Just b)
                                    new_side_result_accum = sideResultAccumF side_result_accum d
                                 in ( case mb_c of
                                          Just c -> M.insert k c m_accum
                                          Nothing -> m_accum
                                    , new_side_result_accum
                                    )
                      )
                      (m1_result, accum1)
                      (M.toList m2_cut)
         in (m2_result, accum2)

lookupDeleteFromList :: Eq a => a -> [a] -> (Bool, [a])
lookupDeleteFromList e l =
        let mship = elem e l
         in ( mship
            , case mship of
                  True  -> delete e l
                  False -> l
            )

from2DList :: Ord k => [(k, a)] -> M.Map k [a]
from2DList l = foldl (\ accum_map (k, e) -> M.unionWith (++) accum_map (M.singleton k [e])) M.empty l

str2Numeric :: (Num a, Ord a) => String -> Maybe a
str2Numeric s = _str2Numeric s 0
      where
        _str2Numeric []      i              = Just i
        _str2Numeric (sh:st) i | isDigit sh = let nexti = (i*10) + (fromIntegral $ digitToInt sh)
                                               in case i > nexti  of
                                                      False -> _str2Numeric st nexti
                                                      True  -> Nothing
                               | otherwise  = Nothing

cons2' :: Char -> Char -> Lazy.ByteString -> Lazy.ByteString
cons2' c1 c2 s = B.cons' c1 (B.cons' c2 s)

cons3' :: Char -> Char -> Char -> Lazy.ByteString -> Lazy.ByteString
cons3' c1 c2 c3 s = B.cons' c1 (cons2' c2 c3 s)

truncLiterary :: String -> Int -> String
truncLiterary str n = case length str > n of
                          True  -> take (n - 3) str ++ "..."
                          False -> str

truncLiteraryLBS :: Lazy.ByteString -> Int64 -> Lazy.ByteString
truncLiteraryLBS str n =
        case B.length str > n of
            True  -> B.concat [B.take (n - 3) str, B.pack "..."]
            False -> str

-----------------------------
-- few public helpers

dump :: String -> IO ()
dump a = do
        h <- openFile "./dump.out.hs" AppendMode
        hPutStr h a
        hClose h

watch :: Show a => a -> a
watch a = unsafePerformIO $ do
                putStrLn_paged 22 $ show a
                return a

watchCond :: Show a => Bool -> a -> a
watchCond cond = if cond then watch else id

putStrLn_paged :: Int -> String -> IO ()
putStrLn_paged page_size s = f $ lines s
        where
          f lines_list =
                let (to_print, to_next_itera) = splitAt page_size lines_list
                 in do putStrLn (concat $ intersperse "\n" to_print)
                       case null to_next_itera of
                           True  -> return ()
                           False -> f to_next_itera << hGetChar stdin << putStrLn "\n-------Press any key to continue...-------"

traceShowPaged :: Show a => Int -> a -> b -> b
traceShowPaged n a b = unsafePerformIO $ do
        putStrLn_paged n $ show a
        return b

traceCond :: Show a => Bool -> Int -> a -> b -> b
traceCond cond n a b = if cond then traceShowPaged n a b else b
