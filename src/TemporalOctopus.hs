module TemporalOctopus where
import qualified OctopusIntention as OI

import InverseSemigroup
import Data.Semigroup as S
import Data.Foldable (fold)
data AffineTemporalFun d = AffineTemporalFun { getLinear :: d
                                             , getAffine :: d
                                             }
                         deriving Show

applyAffineTemporalFun :: Num d => AffineTemporalFun d -> d -> d
applyAffineTemporalFun a = \d -> d * getLinear a + getAffine a
  
instance Num d => Semigroup (AffineTemporalFun d) where
  a <> b = AffineTemporalFun (getLinear a * getLinear b)
           $ (getLinear a * getAffine b + getAffine a)
instance Num d => Monoid (AffineTemporalFun d) where
  mappend = (<>)
  mempty = AffineTemporalFun 1 0


instance (Num d,Fractional d) => ResetableSemigroup (AffineTemporalFun d) where
  reset _ = mempty
instance (Num d,Fractional d) => InverseSemigroup (AffineTemporalFun d) where
  inverse (AffineTemporalFun l a) = AffineTemporalFun (1/l) (-a/l)


data TemporalMove d = TemporalMove (AffineTemporalFun d) (d -> OI.Move d)

instance Num d => Semigroup (TemporalMove d) where
  (TemporalMove a b) <> (TemporalMove a' b') =
    TemporalMove (a <> a')  (\x -> b x
                                          <>
                                          b' (applyAffineTemporalFun a x))
instance Num d => Monoid (TemporalMove d) where
  mappend = (<>)
  mempty = TemporalMove mempty (const mempty)

instance Num d=>  ResetableSemigroup (TemporalMove d) where
  reset (TemporalMove _ f) = TemporalMove mempty f

instance (Num d,Fractional d) => InverseSemigroup (TemporalMove d) where
  inverse (TemporalMove t f) =
    TemporalMove (inverse t) (inverse . f . (applyAffineTemporalFun (inverse t)))

instance  InverseSemigroup (OI.Move d) where
  inverse = undefined

delay :: Num d => d -> TemporalMove d
delay d = TemporalMove (AffineTemporalFun 1 d) mempty

stretch :: Num d => d -> TemporalMove d
stretch d = TemporalMove (AffineTemporalFun d 0) mempty

start :: (Ord a, Num a) => TemporalMove a -> TemporalMove a
start (TemporalMove a b) = TemporalMove a (\x -> if x >= 0 then b x else mempty) 

stop :: (Ord a, Num a) => TemporalMove a -> TemporalMove a
stop (TemporalMove a b) =  TemporalMove a (\x -> if x < 0 then b x else mempty)

play :: Ord a => a -> a -> TemporalMove a -> TemporalMove a
play t1 t2 (TemporalMove a b) = TemporalMove a (\x -> if x >= t1 && x < t2
                                                      then b x
                                                      else mempty)

incremental_display
  :: (Ord d, Num d) =>
     Integer -> d -> (Integer -> TemporalMove d) -> TemporalMove d
incremental_display size d generate_move =
  foldMap (\i -> delay (fromInteger i * d)
              <> start (generate_move i)) [0..size]
