{-# LANGUAGE FlexibleContexts #-}
module MatrixOpt ((!*!),(!*),V3(V3),V4(V4),M33,transpose,inv33) where

import Linear.V3 
import Linear.V4 

import  Linear.Matrix  hiding ((!*!),(!*),det33,inv33,transpose) 
  


{-# INLINE (!*!) #-}
(!*!) :: Num a => M33 a -> M33 a -> M33 a
(V3 (V3 a11 a12 a13)
    (V3 a21 a22 a23)
    (V3 a31 a32 a33)) !*! (V3 (V3 b11 b12 b13)
                              (V3 b21 b22 b23)
                              (V3 b31 b32 b33))
  = V3 (V3 (a11*b11 + a12*b21 + a13*b31) (a11*b12 + a12*b22 + a13*b32) (a11*b13 + a12*b23 + a13*b33))
       (V3 (a21*b11 + a22*b21 + a23*b31) (a21*b12 + a22*b22 + a23*b32) (a21*b13 + a22*b23 + a23*b33))
       (V3 (a31*b11 + a32*b21 + a33*b31) (a31*b12 + a32*b22 + a33*b32) (a31*b13 + a32*b23 + a33*b33))


{-# INLINE (!*) #-}
(!*) :: Num a => M33 a -> V3 a -> V3 a
(V3 (V3 a b c)
    (V3 d e f)
    (V3 g h i)) !* (V3 x y z)
  = V3 (x*a + y*b + z*c)
       (x*d + y*e + z*f)
       (x*g + y*h + z*i)




transpose :: M33 a -> M33 a
transpose (V3 (V3 a b c)
            (V3 d e f)
            (V3 g h i))
  = V3 (V3 a d g)
       (V3 b e h)
       (V3 c f i)

det33 :: Num a => M33 a -> a
det33 (V3 (V3 a b c)
          (V3 d e f)
          (V3 g h i)) = a*e*i + b*f*g+c*d*h -c*e*g-f*h*a-i*b*d


inv33 :: (Num a, Fractional (M33 a)) => M33 a -> M33 a
inv33 m@(V3 (V3 a b c)
        (V3 d e f)
        (V3 g h i)) = 1/(det33 m) ^*!
  V3 (V3 (e*i - f*h)  (c*h - b*i) (b*f - c*e))
     (V3 (f*g - d*i)  (a*i - c*g) (c*d - a*f))
     (V3 (d*h - e*g)  (b*g - a*h) (a*e - b*d))


(^*!) :: Num a => a -> M33 a -> M33 a
x ^*! (V3 (V3 a b c)
        (V3 d e f)
        (V3 g h i)) = (V3 (V3 (x*a) (x*b) (x*c))
                          (V3 (x*d) (x*e) (x*f))
                          (V3 (x*g) (x*h) (x*i)))
