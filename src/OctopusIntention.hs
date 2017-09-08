{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

module OctopusIntention ((<>),(%),
                        Move(),
                        module Color,
                        V4(V4),
                        PenType(..),
                        PenShape(..),
                        Scene(..),
                        Storable(),
                        shift,
                        cyclePen,emptyPen,ncyclePen,ellipsePen,emCyclePen,polyPen,emPolyPen,gearPen,emGearPen,pen,cyclePenShape,
                        loop,emptyScene,
                        computeNextScene,
                        rot_x,rot_y,rot_z,
                        scale_x,scale_y,scale_z,scale,reset
                        )
                        where

import Data.Ratio
    
import Data.Semigroup
import InverseSemigroup
import Affine3D

import MatrixOpt
    
import Foreign.Storable
import Foreign.Ptr (castPtr)    

    
    
import PMVector

import System.IO.Unsafe(unsafePerformIO)
import Graphics.Rendering.OpenGL (GLuint)

import Color

       
       

-- * Octopus (very limited) syntax (could be developped of self analysis and optimization)


-- | The octopus pen shape defines what regular 2D curves should be extruded when moving.
-- Parameters @(pt,n,r,a,c1,c2)@ roughly provide penshape type (@pt@) and, when applicable,  number
-- of vertices (@n@), curve radius (@r@), curve angle (@a@), in degree, possibly negative,
-- and two colors (@c1@ and @c2@), with a semantics that may change with pen shape type.
-- N.B: PenShape with EmpyPen and non zero radius allows for controling the quality of normal
-- computations (see the torus example).
  
newtype PenShape d = PenShape (PenType,Int,d,d,Color d,Color d)

-- | Types of penshapes (the list can be increased, provided the corresponding
-- treatment are added to the geometry shader).
data PenType = EmptyPen | Circle | Polygon | Gear deriving (Show,Eq,Ord,Enum,Bounded)

changeType :: PenType -> PenShape d -> PenShape d             
changeType nt (PenShape (_, n, r, a, c1, c2)) = PenShape (nt, n, r, a, c1, c2)
    
instance forall d. (Storable d,Num d, RealFrac d, Eq d) => Storable (PenShape d) where
    -- todo : storing penshape could be done better
  sizeOf _ = 12 * sizeOf (undefined::d)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::d)
  {-# INLINE alignment #-}
  poke ptr ( PenShape  (pt, n, r, a, V4 r1 g1 d1 a1, V4 r2 g2 d2 a2))
      = do mapM_ (\(n,x) -> pokeElemOff ptr' n x) $ zip [0..11]
                     [fromIntegral (fromEnum pt), -- code of the pen type
                      fromIntegral n, -- the integer is encoded as a float
                      r, a,
                      r1, g1, d1, a1, r2, g2, d2, a2]
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = do
      pt <- peekElemOff ptr' 0 :: IO d
      n <- peekElemOff ptr' 1 :: IO d
      r <- peekElemOff ptr' 2
      a <- peekElemOff ptr' 3
      c1 <- V4 <$> peekElemOff ptr' 4 <*> peekElemOff ptr' 5 <*> peekElemOff ptr' 6 <*> peekElemOff ptr' 7
      c2 <- V4 <$> peekElemOff ptr' 8 <*> peekElemOff ptr' 9 <*> peekElemOff ptr' 10 <*> peekElemOff ptr' 11
      let pti = toEnum(round (pt))
      case pti <= maxBound of
        True -> return $ PenShape (pti, round n, r, a, c1, c2)
        False -> fail "Retreiving pen : not a valid penshape"
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

-- | Empty pen shape
emptyPenShape ::  (Num d) => PenShape d                  
emptyPenShape = PenShape (EmptyPen,0,0,0,0,0)

-- | Cyclic pen shape
cyclePenShape :: (RealFrac d, Storable d) => d -> Color d -> PenShape d
                 --  todo : redundant with cycle pen below... ?
cyclePenShape r c = PenShape (Circle,30,r,360,c,c)
                
-- * Octopus semantics

-- ** Octopus state

-- | Octopus scene
data Scene d = Scene {
      -- | succesive positions of the pen
      penPositionArray :: PMV (Affine3D d),
      -- | succesive shapes of the pen
      penShapeArray :: PMV (PenShape d),
      -- | connected pen segments
      adjacencyArray :: PMV GLuint, -- line adjacency array
      -- | current pen position                
      currentPenPosition :: Affine3D d
    }


-- | A rather adhoc print scene info function 

printSceneInfo ::  (Storable d,Floating d) => Scene d -> IO ()
printSceneInfo s =
    do
      nbp <- sizePMV (penPositionArray s)
      nbs <- sizePMV (penShapeArray s)
      nba <- sizePMV (adjacencyArray s)
      print $ "Scene with:"
      print $ show nbp ++ " positions,"
      print $ show nbs ++ " shapes,"
      print $ show nba ++ " adjacency."

-- | Creating a new scene  (a big initial memory allocation)          
emptyScene :: (Storable d, RealFrac d) => IO (Scene d)
emptyScene = do
  pos <- newPMV 
  sha <- newPMV
  _ <- addPMV pos mempty
  _ <- addPMV sha emptyPenShape
       -- the empty shape is always at position 0 ::: TODO : probably useless !
  seg <- newPMV 
  return $ Scene pos sha seg mempty 
  
-- | Reseting an existing scene (no memory allocation) 
resetScene s = do
  resetPMV (penPositionArray s)
  resetPMV (penShapeArray s)
  _ <- addPMV (penPositionArray s) mempty
  _ <- addPMV (penShapeArray s) emptyPenShape
       -- the empty shape is always at position 0 ::: TODO : probably useless !
  resetPMV (adjacencyArray s)
  return $ s {currentPenPosition = mempty}     

-- * Octopus transition         
         
-- | Octopus Program Semantics
newtype Move d = Move (Scene d -> IO (Scene d))

instance Monoid (Move d) where
    mempty = Move return
    mappend (Move f) (Move g) = Move $ \s -> (f s)>>= g

instance Semigroup (Move d)                                

instance ResetableSemigroup (Move d) where
    reset (Move f) =
        Move $ \s -> do
          let sp = currentPenPosition s
          ns <- f s
          _ <- addPMV (adjacencyArray s) 0         
          return ns {currentPenPosition = sp}
                 

instance  (Floating d, RealFrac d, Storable d) => A3D (Move d) d where
    trans m = Move $ \s -> do
                let ncp = currentPenPosition s <> m
                return (s {currentPenPosition = ncp})

    
computeNextScene :: (RealFrac d, Storable d) => Move d -> Scene d -> IO (Scene d)
computeNextScene (Move f) s = resetScene s >>= f

-- *  3D drawing tools

-- ** Predefined move functions

-- | Converts a pen shape to a move
pen :: (Storable d,RealFrac d,Num d) => PenShape d -> Move d
pen ps = Move $ \s -> do         
  ni <- addPMV (penPositionArray s) (currentPenPosition s)
  _ <- addPMV (penShapeArray s) ps
  _ <- addPMV (adjacencyArray s) $ fromIntegral ni
  return s

-- | The empty pen creates a disconnection between previous and forthcomming drawings                 
emptyPen :: (Storable d, Num d, RealFrac d) => Move d
emptyPen = pen $ PenShape (EmptyPen,0,0,0,0,0)                

-- | The cycle pen is a ready to connect cyclic pen shape
cyclePen :: (RealFrac d, Storable d) => d -> Color d -> Move d 
cyclePen r c = pen $ PenShape (Circle,60,r,360,c,c)

-- | The n cycle pen is a ready to connect cyclic pen shape with specified resolution
ncyclePen :: (RealFrac d, Storable d) => Int -> d -> Color d -> Move d 
ncyclePen n r c = pen $ PenShape (Circle,n,r,360,c,c)
               
-- | The ellipse pen is a ready to connect ellipse pen shape
ellipsePen :: (RealFrac d, Storable d, Floating d) => d -> d -> Color d -> Move d 
ellipsePen r1 r2 c = scale_y (1/r2) <> pen (PenShape (Circle,30,r1,360,c,c))
                   <> scale_y (r2)

-- | The empty cycle pen is a (non drawn) pen shape for smoothness purpose
emCyclePen :: (RealFrac d, Storable d) => d -> Color d -> Move d 
emCyclePen r c = pen $ PenShape (EmptyPen,1,r,360,c,c)

-- | The poly pen creates a ready to connect pologonic (moved up to 3 points if needed) pen shape
polyPen :: (RealFrac d, Storable d) => Int -> d -> Color d -> Move d 
polyPen n r c = pen $ PenShape (Polygon,n,r,360,c,c)

-- | The empty poly pen is a (non drawn) pen shape for smoothness purpose
emPolyPen :: (RealFrac d, Storable d) => Int -> d -> Color d -> Move d 
emPolyPen n r c = pen $ PenShape (Polygon,n,r,360,c,c)
                 
-- | The gear pen is a ready to connect gear pen shape
gearPen :: (RealFrac d, Storable d) => Int -> d -> Color d -> Move d 
gearPen n r c = pen $ PenShape (Gear,n,r,360,c,c)
               
-- | The empty gear pen is a (non drawn) pen shape for smoothness purpose
emGearPen :: (RealFrac d, Storable d) => Int -> d -> Color d -> Move d 
emGearPen n r c = pen $ PenShape (EmptyPen,n,r,360,c,c)

-- | Iterates and loops the given affine move with a given pen shape.
loop :: (Storable d, Floating d, RealFrac d) => Int -> Affine3D d -> PenShape d -> Move d
loop n aff ps = reset $ 
  foldl (<>) (pen (changeType EmptyPen ps)) (map (\_ -> trans aff <> pen ps) [0..n])
        <> trans aff <> pen (changeType EmptyPen ps)





       


