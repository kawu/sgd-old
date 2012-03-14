{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses #-}


module SGD.Base
( Grad
, ParamCore (..)
, DataElem (..)
) where


import qualified Data.MarkedArray as MA
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Control.Monad (forM_)
import           GHC.Conc (numCapabilities)
import           Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)


type Grad = MA.MarkedArray
type Fun1 = Double -> Double
type Fun2 = Double -> Double -> Double
type LogDouble = Double


class ParamCore p where
    -- | Map function on parameter set.  This function may be unsafe,
    -- input parameter set will not be used again.
    unsafeMap           :: Fun1 -> p -> IO p
    -- | Insert list of (index, value) pairs into parameter set
    -- using given combining function.  This function may be unsafe,
    -- input parameter set will not be used again.
    unsafeConsume       :: Fun2 -> [(Int, Double)] -> p -> IO p
    -- | Size of the parameter set.
    size                :: p -> Int


class ParamCore p => DataElem p x where
    -- | Gradient computation.  It is ensured, that initially
    -- gradient is filled with 0 values.
    computeGrad         :: p -> [x] -> Grad -> IO Grad
    -- | Accuracy on given data set.  Just for show.
    accuracy            :: p -> [x] -> Double


instance ParamCore (U.Vector Double) where

    size values = U.length values

    unsafeConsume f xs values = do
        vect <- U.unsafeThaw values
        forM_ xs $ \(i, v) -> do
            w <- UM.read vect i
            UM.write vect i $! f w v
        values' <- U.unsafeFreeze vect
        return values'

    unsafeMap f values = do
        vect <- U.unsafeThaw values
        -- | Explicit concurrent computation.
        com <- newEmptyMVar
        forM_ bounds $ \(p, q) -> forkIO $ do  
            forM_ [p .. q - 1] $ \i ->
                UM.write vect i . f =<< UM.read vect i
            putMVar com ()
        sequence [takeMVar com | _ <- bounds]
        values' <- U.unsafeFreeze vect
        return values'
      where
        k       = numCapabilities
        n       = U.length values
        ps      = ((n - 1) `div` k) + 1
        uppers  = [i * ps | i <- [1 .. k - 1]] ++ [n]
        bounds  = zip (0 : uppers) uppers

--     unsafeMap f values = do
--         vect <- U.unsafeThaw values
--         forM_ [0 .. UM.length vect - 1] $
--             \i -> UM.write vect i . f =<< UM.read vect i
--         values' <- U.unsafeFreeze vect
--         return values'
