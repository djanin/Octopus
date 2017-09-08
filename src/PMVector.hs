{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : PMVector
Description : Partially mutable vector
Copyright   : (c) David Janin 2017
License     : see the LICENSE file in the distribution
Maintainer  : janin@labri.fr
Stability   : experimental

Partially mutable and storable vectors indexed over [0..s-1] where s is the number of elements in the vector.
So far, elements can only be added at the end and then red at will.

-}

module PMVector (PMV(), newPMV, resetPMV, addPMV, appendPMV,readPMV,updatePMV,dumpPMV,sizePMV,updateAllPMV,unsafeWith)
    where

import qualified Data.Vector.Storable.Mutable as VS
import Data.IORef
import Foreign.Storable
import Foreign.Ptr

import System.IO.Unsafe(unsafePerformIO)

-- * Partially Mutable Storable Vector
    
data PMV v = PMV (IORef Int) (VS.IOVector v)
           
-- | A bound on the size of PMVector
maxSize :: Int
maxSize = 2^15
  -- todo : this means that, with the above value, at most 1048576 vertex or triangles may appear in an given 3D Scene...
  -- More complex scene will require object to be splited in pieces. Observe that calls of headM _ [] says there are
  -- no more connecting points hence an object is "complete" 
          
-- | Gets the number of elements in a 'PMV'
sizePMV :: PMV v -> IO Int
sizePMV (PMV sr _) = readIORef sr

                     
-- | Creates a new empty 'PMV' initialized to undefined with max size (use resetPMV to set size to zero)
newPMV ::  (Storable v) => IO (PMV v)
newPMV =
    do
      size <- newIORef $ maxSize -1
      vector <- VS.new maxSize
      return (PMV size vector)
             
-- | Resets a 'PMV' to zero size discarding its content.
resetPMV :: PMV v -> IO (PMV v)
resetPMV s@(PMV sr _) = do
  writeIORef sr 0
  return s

-- | Appends a list of new elements to a 'PMV' and sends back the list of new element indices.
appendPMV :: (Storable v) => PMV v -> [v] -> IO ([Int])
appendPMV (PMV sr arr) l =
    let n = length l
    in do
      s <- readIORef sr      
      mapM_ (\i -> VS.write arr (s+i) (l!!i)) [0..n-1]
      modifyIORef' sr (n+)  -- strict modification of size
      return $ map (\i -> i + s) [0..n-1]

-- | Adds a new element to a 'PMV' and sends back the new element index.
addPMV :: (Storable v) => PMV v -> v -> IO (Int)
addPMV (PMV sr arr) v =
    do
      s <- readIORef sr
      VS.write arr s v
      modifyIORef' sr (1+)
      return $ s

-- | Reads the ith element of a 'PMV', with i ranging from 0 to s-1.
readPMV :: (Storable v) => PMV v -> Int -> IO v
readPMV (PMV sr arr) i =
    do
      s <- readIORef sr
      case (i < s) of
        True ->  VS.read arr i
        False -> fail $  "readPMV : Out of range index "++show i++ ": out of [0.."++show (s-1) ++ "]"
                 
-- | Updates the ith element of a 'PMV' with i ranging from 0 to s-1.
updatePMV :: (Storable v) => PMV v -> Int -> (v -> v) -> IO ()
updatePMV (PMV sr arr) i f =
    do
      s <- readIORef sr
      case (i < s) of
        True ->  do
          v <- VS.read arr i
          VS.write arr i (f v)
        False -> fail $  "updatePMV : Out of range index "++show i++ ": out of [0.."++show (s-1) ++ "]"

-- | Updates all elements of a 'PMV'
updateAllPMV :: (Storable v) => PMV v -> (v -> v) -> IO ()
updateAllPMV (PMV sr arr) f =
    let updateI arr0 i =
            do
              v <- VS.read arr0 i
              VS.write arr0 i (f v)
    in do
      s <- readIORef sr
      mapM_ (updateI arr) [0..s-1]
    
            
-- * Tests
            
                 
-- | Dumps a 'PMV' into a list

dumpPMV :: (Storable v) => PMV v -> IO [v]
dumpPMV (PMV sr arr)  =
    do
      s <- readIORef sr
      -- outArr <- unsafeFreezeSTArray $ listArray $
      mapM (\i -> VS.read arr i) [0..s-1] 
          
{-
-- Internal : a little test
test :: (Int, Int, [Int], [Int])
test = unsafePerformIO $
    do 
      arr <- newPMV
      s0 <- sizePMV arr
      li <- appendPMV arr [1,2,3,4]
      li' <- appendPMV arr [1,2,3,4]
      s1 <- sizePMV arr
      l <- dumpPMV arr
      return (s0,s1,li++li',l)
  -}  
             
unsafeWith :: Storable a => PMV a -> (Ptr a -> IO b) -> IO b
unsafeWith (PMV _ a) f = VS.unsafeWith a f
