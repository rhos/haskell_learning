module Main where
import           Data.Array.Unboxed
import           Data.Array.ST
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.ST

main :: IO ()
main = return ()

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0, 9) [(3, True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1, 10) $ zip [1 .. 10] $ cycle [True]

qcArray :: UArray Int Bool
qcArray = array (0, 4) $ [(1, True), (2, True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) $ zip [0 .. 3] $ cycle [0]

updateBiB :: UArray Int Int
updateBiB = beansInBuckets // [(1, 5), (3, 6)]

incBiB :: UArray Int Int
incBiB = accum (+) updateBiB $ zip [0 .. 3] $ cycle [2]

-- listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
-- listToSTUArray vals = do
--   let end = length vals - 1
--   newArray (0,end) 0

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

myData1 :: UArray Int Int
myData1 = listArray (0, 5) [7, 3, 5, 9, 11, 1]

myData2 :: UArray Int Int
myData2 = listArray (0, 5) [8, 4, 6, 10, 12, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. (end - i)] $ \j -> do
      val     <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray j       nextVal
        writeArray stArray (j + 1) val
      return 5
  return stArray

crossOver :: (UArray Int Int ,UArray Int Int) -> Int -> UArray Int Int
crossOver (a1,a2) crossOverPt = runSTUArray $ do
  st1 <- thaw a1
  let end = (snd . bounds) a1
  forM_ [crossOverPt .. end] $ \i -> do
    writeArray st1 i $ a2 ! i
  return st1

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros array = runSTUArray $ do
  starray <- thaw array
  let end = (snd . bounds) array
  let count = 0
  forM_ [0 .. end] $ \i -> do
    val <- readArray starray i
    when (val == 0) $ do
      writeArray starray i (-1)
  return starray