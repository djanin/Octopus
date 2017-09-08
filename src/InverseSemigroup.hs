{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DefaultSignatures  #-}

module InverseSemigroup where

import Data.Semigroup 
          
-- * Resetable and Inverse semigroups

{- | Instances of `ResetableSemigroup` should satisfy the following laws

> reset p <> p  == p
> reset p <> reset p == reset p
> reset p <> reset q == reset q <> reset p
> reset q <> p == p => reset p <> reset q = reset p

The first law says that reset p is a left local unit to p. The second and third law say that these left local units are idempotent and commute. The fourth law says that reset p is the least local units of p in the order
defined over local units by @ p `pleq` q when p <> q = p@

Resetable semigroups are known in semigroup theory as left E-semiadequate semigroups, where E refers to the 
set of elements resets (see: A munn type representation for a class of E-semiadequate semigroups, by
J. Fountain, G.M.S. Gomes  and V. Gould)


Resetable semigroups generalizes `InverseSemigroup` with @reset p = p <> inverse p@.

But there may be more effcient implementation such as  @reset p = mempty@ in  `Group`.
-}
class (Semigroup m) => ResetableSemigroup m where
    reset :: m -> m
    default reset :: (InverseSemigroup m) => m -> m
    reset m = m <> inverse m
              
{- | Instances of `InverseSemigroup` should satisfy the following laws

> p <> inverse p <> p == p
> inverse p <> p <> inverse p == inverse p
> p <> inverse p <> q <> inverse q == q <> inverse q <> p <> inverse p

The first two laws axiomatize the notion of semigroup inverse. The third law guarantees that 
indempotent elements commute, a property equivalent to the unicity of inverses.

By default, The reset operation is implemented by

> reset p = p <> inverse p

However, direct implementation may be much more efficient (the above is likely to be exponential).
For instance, over groups, we have reset p = mempty.
-}

class (ResetableSemigroup m) => InverseSemigroup m where
    inverse :: m -> m
    -- | The natural partial order relation defined by
    -- @
    -- norder m n = m == (reset m <> n)
    -- @
    norder :: (Eq m) => m -> m -> Bool
    norder m n = m == (m <> inverse m <> n)

                
-- * Resatable and inverse monoids and groups

{- | The `InverseMonoid` class describes inverse semigroups that are also monoids.
All instances  of `InverseMonoid` satisfies the following law

> inverse mempty == mempty

since it follows from inverse semigroup properties and unicity of inverses.
-}
class  (Monoid m, InverseSemigroup m) => InverseMonoid m
    
{- | The `Group` class describes groups, that is, inverse semigroups
and that are moreover required to satisfy the following group law

> p <> (inverse p) == mempty

-}
class InverseMonoid m => Group m 
    

-- * Inverse semigroup generation


               
{- | The `SemigroupAction` class describes left action of a semigroup over a set. 
Instances of `SemigroupAction` should satisfy the following mixed associativity law

> act (mappend m1 m2)  a == act m1 (act m2 a)

-}
class (Semigroup m) => SemigroupAction m a where
    act :: m -> a -> a

           
{- | The `MonoidAction` class describes left action of a monoid over a set,
a semigroup action that moreover satisfies the following law

> act mempty a == a

-}
class (Monoid m, SemigroupAction m a) => MonoidAction m a
           
{- | The `MorphicAction` class describes left action of a semigroup over a semigroup
that moreover satisfies the following law

> act m (mappend a b) == mappend (act m a) (act m b)

-}
class (Semigroup m, Semigroup a, SemigroupAction m a) => MorphicAction m a

-- * Application to semigroup and monoid construction

data SProduct  m a = SProduct m a deriving (Show)

-- | Semi-direct product of two semigroups
instance (SemigroupAction m a, Semigroup a) => Semigroup (SProduct m a)  where
    (<>) (SProduct m1 a1) (SProduct m2 a2)
        = SProduct (m1 <> m2) (a1 <> act m1 a2)
                            
-- | Semi-direct product of two monoids
instance (MonoidAction m a, Monoid a) => Monoid (SProduct m a) where
    mempty = SProduct mempty mempty
    mappend (SProduct m1 a1) (SProduct m2 a2)
        = SProduct (mappend m1 m2) (mappend a1 (act m1 a2))

-- * Dimension extension
                                   
-- | Intendedly, the data type @Extend d m@ behaves like a pair of type @(d ->_r d,d -> m)@ where
-- @d ->_r d@ is a monoid under the flipped composition defined by @f <> g = g . f@
-- that acts over @d -> m@ with @act f mf = mf . f@.
--
-- Semigroup, Monoid, Functor, Applicative and Monad instances built from
-- the type constructor @Extend@ derives from  this intention.
--
-- With the intuition that d is a time dimension, then Extend f mf at an instant d
-- produce the value (mf d) and change the instant into (f d)
--
-- With the intuition that d is some space dimension, then Extend f mf at position d
-- produce the value (mf d) and moves to the position (f d)

data Extend d m = Extend (d -> d) (d -> m) 
                
-- | since SemigroupAction (d ->_r d) (d -> m)
instance (Semigroup m) => Semigroup (Extend d m) where
    (<>) (Extend f m)  (Extend g n) =
        Extend (g . f) (m <> (n . f))
               
-- | since MonoidAction (d ->_r d) (d -> m)
instance (Monoid m) => Monoid (Extend d m) where
    mempty = Extend id (const mempty)
    mappend (Extend f m)  (Extend g n) =
        Extend (g . f)  (mappend m (n . f))

instance (ResetableSemigroup m) => ResetableSemigroup (Extend d m) where
    reset (Extend f m) = Extend id (\d -> reset(m d))
               
instance Functor (Extend d) where
    fmap f (Extend g m) =
        Extend g (f . m)

instance Applicative (Extend d) where
    pure m = Extend id  (const m)
    (<*>)  (Extend f m) (Extend g n) =
        Extend (g . f) (\d -> (m d) ((n . f) d))
       
    
instance Monad (Extend d) where
    return m = Extend id  (const m)
    (>>=) (Extend f  m) h = Extend g n where
            g d = case h (m d) of
                    Extend gg  _ -> gg (f d) -- could be apply (f <> gg) d
            n d = case  h (m d) of
                    Extend _  nn -> nn (f d) -- could be apply (act f n) d
