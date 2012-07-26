{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Data.MarkedArray
( MarkedArray
, new
, elems
, clear
, mapArray
, consumeWith
) where

import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Applicative ((<$>))
import Data.Ix (Ix, range)
-- import qualified Data.Array.ST as A
import qualified Data.Array.IO as A
import qualified Data.Array.MArray as A
-- import qualified Data.Array.Unboxed as A
import Control.Monad (forM_, foldM)

--data MarkedArray s = MarkedArray
--    (A.STUArray s Int Double) (A.STUArray s Int Bool) [Int]

data MarkedArray = MarkedArray
    (A.IOUArray Int Double) (A.IOUArray Int Bool) [Int]

new :: Int -> IO MarkedArray
new n = do
    arr <- A.newListArray bounds [0.0 | i <- range bounds]
    barr <- A.newListArray bounds [False | i <- range bounds]
    return $ MarkedArray arr barr []
    where bounds = (0, n - 1)

elems :: MarkedArray -> IO [(Int, Double)]
elems (MarkedArray arr _ trace) = do
    -- values <- mapM (A.readArray arr) trace
    -- return $ zip trace values
    mapM' (\i -> (i,) <$> A.readArray arr i) trace
  where
    sequence' (mx:xs) = unsafeInterleaveIO $
        combine xs =<< mx
        where combine xs x = return . (x:) =<< sequence' xs
    sequence' [] = return []
    
    mapM' f (x:xs) = unsafeInterleaveIO $ do
        y <- f x
        ys <- mapM' f xs
        return (y : ys)
    mapM' _ [] = return []

clear :: MarkedArray -> IO MarkedArray
clear (MarkedArray arr barr trace) = do
    forM_ trace $
        \i -> A.writeArray barr i False
    return $ MarkedArray arr barr []

mapArray :: (Double -> Double) -> MarkedArray -> IO MarkedArray
mapArray f ma@(MarkedArray arr _ trace) = do
    forM_ trace $
        \i -> do
            v <- A.readArray arr i
            A.writeArray arr i $! f v
    return ma

consumeWith :: (Double -> Double -> Double) -> [(Int, Double)]
            -> MarkedArray -> IO MarkedArray
consumeWith f xs ma@(MarkedArray arr barr trace) = do
    trace' <- foldM (consumeElem f ma) trace xs
    return $ MarkedArray arr barr trace'

consumeElem :: (Double -> Double -> Double) -> MarkedArray
            -> [Int] -> (Int, Double) -> IO [Int]
consumeElem f (MarkedArray arr barr _) (!trace) (i, v) = do
    b <- A.readArray barr i
    if b == False
        then do
            A.writeArray barr i True
            A.writeArray arr i v
            return (i:trace)
        else do
            w <- A.readArray arr i
            A.writeArray arr i $! f w v
            return trace
