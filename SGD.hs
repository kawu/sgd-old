module SGD
( sgd
, SgdArgs (..)
, module SGD.Base
) where

import           Prelude hiding (length)
import           Control.Monad (replicateM, foldM, forM_, when)
import           Control.Applicative ((<$>))
import           Data.List (transpose)
import           Data.ListLike (ListLike, length, index, toList)
import           System.Random (randomRIO, setStdGen, mkStdGen)
import           System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
import           GHC.Conc (numCapabilities)
import           Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)

import qualified Data.MarkedArray as MA
import           SGD.Base

data SgdArgs = SgdArgs
    { batchSize :: Int
    , regVar    :: Double
    , iterNum   :: Double
    , scale0    :: Double
    , tau       :: Double }

sgd :: (ListLike v x, DataElem p x) => SgdArgs -> v -> v -> p -> IO p
sgd args trainData evalData params = do
    let step :: Double
        step = fromIntegral (batchSize args)
             / fromIntegral (length trainData)
        scales = map (\done -> (scale0 args * tau args)
                             / (tau args + done)) [0, step ..]
        points = takeWhile (> 0.0) [iterNum args, iterNum args - step ..]

    hSetBuffering stdout NoBuffering
    putStr "Training data size = "
    putStrLn $ show $ length trainData
    when (length evalData > 0) $ do
        putStr "Evaluation data size = "
        putStrLn $ show $ length evalData

    putStr "Model size = "
    ms <- return $ size params
    putStrLn $ show ms
    putStrLn "\n  -- TRAINING --"

    -- buffers for gradients
    gradBufs <- replicateM numCapabilities (MA.new ms)
    setStdGen $ mkStdGen 0
    params' <- foldM
        (sgdStep args trainData evalData step gradBufs)
        params
        (zip points scales)
    putStrLn "\n  -- FINISHED --"

    putStrLn $ ("\naccuracy train = " ++)
             $ show $ accuracy params'
             $ toList trainData
    return params'

putInfo :: (ListLike v x, DataElem p x)
        => SgdArgs -> p -> v -> Double -> IO ()
putInfo args params dataSet point = do
    acc <- return $ case length dataSet of
        0 -> "#"
        _ -> show $ accuracy params $ toList dataSet
    putStrLn $ "\n" ++ "[" ++ (show $ floor $ point) ++ "] "
        ++ "accuracy eval = " ++ acc

sgdStep :: (ListLike v x, DataElem p x)
          => SgdArgs -> v -> v -> Double -> [Grad]
          -> p -> (Double, Double) -> IO p
sgdStep args trainData evalData step gradBufs params (point, scale) = do
    if floor point /= floor (point - step)
        then putInfo args params evalData point
        else putStr "."
    batch <- getBatch trainData (batchSize args)
    updateParams args params gradBufs batch scale $ length trainData

updateParams :: (ListLike v x, DataElem p x)
             => SgdArgs -> p -> [Grad] -> v
             -> Double -> Int -> IO p
updateParams args params grads batch scale trainSize =
    let regularization v = v * regCoef
        regCoef = 1.0 - iVar2 * coef * scale
        coef = (fromIntegral $ batchSize args)
             / (fromIntegral trainSize)
        iVar2 = 1.0 / (regVar args ^ 2)
        parts = partition numCapabilities $ toList batch
    in do
        -- explicit concurrent computation of gradients
        com <- newEmptyMVar
        forM_ (zip grads parts) $ \(grad, part) -> forkIO $ do
            grad' <- computeGrad params part grad
            putMVar com =<< MA.mapArray (*scale) grad'
        grads' <- sequence [takeMVar com | _ <- [1..numCapabilities]]

        params' <- unsafeMap regularization params
        result <- foldM (\params grad ->
            applyGrad grad params) params' grads'
        forM_ grads' MA.clear
        return result

applyGrad :: ParamCore p => Grad -> p -> IO p
applyGrad grad params =
    MA.elems grad >>= \xs -> unsafeConsume (+) xs params

getBatch :: ListLike v x => v -> Int -> IO [x]
getBatch xs n =
    map (index xs) <$> replicateM n (randomRIO (0, length xs - 1))

partition :: Int -> [a] -> [[a]]
partition k = transpose . group k
    where group k [] = []
          group k xs = take k xs
                     : (group k $ drop k xs)
