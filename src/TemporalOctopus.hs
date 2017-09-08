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


data TemporalOctopus d = TemporalOctopus (AffineTemporalFun d) (d -> OI.Move d)

instance Num d => Semigroup (TemporalOctopus d) where
  (TemporalOctopus a b) <> (TemporalOctopus a' b') =
    TemporalOctopus (a <> a')  (\x -> b x
                                          <>
                                          b' (applyAffineTemporalFun a x))
instance Num d => Monoid (TemporalOctopus d) where
  mappend = (<>)
  mempty = TemporalOctopus mempty (const mempty)

instance Num d=>  ResetableSemigroup (TemporalOctopus d) where
  reset (TemporalOctopus _ f) = TemporalOctopus mempty f

makeTemporalOctopus
  :: Num d => (d -> OI.Move d) -> TemporalOctopus d
makeTemporalOctopus s = TemporalOctopus mempty s

delay :: Num d => d -> TemporalOctopus d
delay d = TemporalOctopus (AffineTemporalFun 1 d) mempty

stretch :: Num d => d -> TemporalOctopus d
stretch d = TemporalOctopus (AffineTemporalFun d 0) mempty

start :: (Ord a, Num a) => TemporalOctopus a -> TemporalOctopus a
start (TemporalOctopus a b) = TemporalOctopus a (\x -> if x >= 0 then b x else mempty) 

stop :: (Ord a, Num a) => TemporalOctopus a -> TemporalOctopus a
stop (TemporalOctopus a b) =  TemporalOctopus a (\x -> if x < 0 then b x else mempty)

play :: Ord a => a -> a -> TemporalOctopus a -> TemporalOctopus a
play t1 t2 (TemporalOctopus a b) = TemporalOctopus a (\x -> if x >= t1 && x < t2
                                                      then b x
                                                      else mempty)

incremental_display
  :: (Ord d, Num d) =>
     Integer -> d -> (Integer -> TemporalOctopus d) -> TemporalOctopus d
incremental_display size d generate_move =
  foldMap (\i -> delay (fromInteger i * d)
              <> start (generate_move i)) [0..size]
