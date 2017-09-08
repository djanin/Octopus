{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ScopedTypeVariables  #-}



module Affine3D (Affine3D (..),normalAffine,getAffine,getShift,A3D(..),(!*!),unit3D) where

import Data.Semigroup    
import InverseSemigroup

-- import Linear.V3
-- import Linear.Matrix hiding ((!*!),(!*))

import MatrixOpt

import Foreign.Storable

import Foreign.Ptr (castPtr, Ptr)    

import Data.IORef                                  
                                  

-- * Affine transformation

data Affine3D d = Affine3D !(M33 d) !(V3 d) deriving (Show)

normalAffine :: Fractional d => Affine3D d -> M33 d
normalAffine (Affine3D m _) = (transpose . inv33) m
getAffine :: Affine3D t -> (M33 t, V3 t)
getAffine (Affine3D m v) =  (m,v)
getShift  (Affine3D _ v) = v 

id33 :: (Num d) => (M33 d)
id33 = (V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1))

unit3D :: (Num d) => (Affine3D d)
unit3D = Affine3D id33 0

compose3D (Affine3D m1 v1) (Affine3D m2 v2) = Affine3D (m1!*!m2) (v1+m1!* v2)          
inverse3D (Affine3D m v) = Affine3D (inv33 m) (-inv33 m !*v)

instance Num d => Num (Affine3D d) where
    (+)  (Affine3D m1 v1) (Affine3D m2 v2) = Affine3D (m1 + m2) (v1+v2) 
    (*)  = compose3D
    fromInteger d = Affine3D (V3 (V3 (fromInteger d) 0 0) (V3 0 (fromInteger d) 0) (V3 0 0 (fromInteger d))) 0
    abs a = a
    signum _ = unit3D
    negate (Affine3D m v) = Affine3D (negate m) (negate v)

instance Fractional d => Fractional (Affine3D d) where
    fromRational d = Affine3D (V3 (V3 (fromRational d) 0 0) (V3 0 (fromRational d) 0) (V3 0 0 (fromRational d))) 0
    recip = inverse3D


instance Num d => Monoid  (Affine3D d) where
    mempty = 1
    mappend = compose3D

instance Num d => Semigroup  (Affine3D d)
                    
instance (Fractional d) => ResetableSemigroup (Affine3D d) where
    reset _ = mempty
instance (Fractional d) => InverseSemigroup (Affine3D d) where
    inverse = inverse3D

              
instance Num d => SemigroupAction (Affine3D d) (V3 d) where
    act (Affine3D m v) p = (m!*p)+v


instance forall d. Storable d => Storable (Affine3D d) where
    -- matrices are stored by column (GPU compatible) 
  sizeOf _ = 12 * sizeOf (undefined::d)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::d)
  {-# INLINE alignment #-}
  poke ptr (Affine3D (V3 (V3 x0 x3 x6) (V3 x1 x4 x7) (V3 x2 x5 x8) ) (V3 x9 x10 x11))
      -- transposed storage
      = do mapM_ (\(n,x) -> pokeElemOff ptr' n x) $ zip [0..11] [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11]
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = do
      -- transposed storage
      m <- do
        l1 <- V3 <$> peekElemOff ptr' 0 <*> peekElemOff ptr' 3 <*> peekElemOff ptr' 6
        l2 <- V3 <$> peekElemOff ptr' 1 <*> peekElemOff ptr' 4 <*> peekElemOff ptr' 7
        l3 <- V3 <$> peekElemOff ptr' 2 <*> peekElemOff ptr' 5 <*> peekElemOff ptr' 8
        return $ V3 l1 l2 l3
      v <- V3 <$> peekElemOff ptr' 9 <*> peekElemOff ptr' 10 <*> peekElemOff ptr' 11
      return $ Affine3D m v
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

    
-- * Affine 3D class

class (Floating d, Monoid t) => A3D t d | t-> d where
    trans :: Affine3D d -> t
    rot_x :: d -> t
    rot_x = trans . a_rot_x
    rot_y :: d -> t
    rot_y = trans . a_rot_y
    rot_z :: d -> t
    rot_z = trans . a_rot_z
    shift :: (d,d,d) -> t
    shift = trans . a_shift
    scale :: d -> t
    scale = trans . a_scale
    scale_x :: d -> t
    scale_x = trans . a_scale_x
    scale_y :: d -> t
    scale_y = trans . a_scale_y
    scale_z :: d -> t
    scale_z = trans . a_scale_z
    turn :: d -> t           
    turn = rot_z
    walk :: d -> t       
    walk d = shift (d,0,0)

instance (Floating d) => A3D (Affine3D d) d where
    trans  = id
             
outputBase :: (Num d) =>
     d -> d -> d -> d -> d -> d -> d -> d -> d -> Affine3D d
outputBase  ax ay az
            bx by bz
            cx cy cz  
  = Affine3D (V3 (V3 ax ay az) (V3 bx by bz)  (V3 cx cy cz)) (V3 0 0 0)
                
a_rot_x a = outputBase
              1   0         0
              0   (cos a)  (-sin a)
              0   (sin a)  (cos a)
a_rot_y a = outputBase
              (cos a)    0 (sin a)
              0          1  0
              (-sin a) 0  (cos a)
a_rot_z a = outputBase
              (cos a) (-sin a) 0
              (sin a) (cos a)    0
              0       0          1
a_shift (a1,a2,a3) = Affine3D id33 (V3 a1 a2 a3)
a_scale a = outputBase
              a  0  0
              0  a  0
              0  0  a
a_scale_x a = outputBase
              a  0  0
              0  1  0
              0  0  1
a_scale_y a = outputBase
              1  0  0
              0  a  0
              0  0  1
a_scale_z a = outputBase
              1  0  0
              0  1  0
              0  0  a

