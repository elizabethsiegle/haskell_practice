{-# LANGUAGE TypeOperators, TypeInType, GADTs, TypeFamilies,
             UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables,
             TypeApplications, PolyKinds, RankNTypes, DataKinds #-}

module Class where

--Lists of vectors

--As usual, this is a Literate Haskell file, with the obligatory header:

-- # LANGUAGE StandaloneDeriving, TypeInType, TypeOperators, GADTs,
--              TypeFamilies, UndecidableInstances 
-- module VecList where
-- import Data.Kind ( Type )
-- import Prelude hiding ( concat, (++) )
-- data Nat where
--   Zero :: Nat
--   Succ :: Nat -> Nat
-- type family (a :: Nat) + (b :: Nat) :: Nat where
--   Zero   + b = b
--   Succ a + b = Succ (a + b)
-- infixl 6 +
-- type family (a :: Nat) * (b :: Nat) :: Nat where
--   Zero   * b = Zero
--   Succ a * b = b + (a * b)
-- infixl 7 *
-- data Vec :: Nat -> Type -> Type where
--   Nil  :: Vec Zero a
--   (:>) :: a -> Vec n a -> Vec (Succ n) a
-- infixr 5 :>
-- deriving instance Show a => Show (Vec n a)
-- (++) :: Vec n a -> Vec m a -> Vec (n + m) a
-- Nil       ++ ys = ys
-- (x :> xs) ++ ys = x :> (xs ++ ys)
-- infixr 5 ++
-- Consider this standard-library function:

-- concat :: [[a]] -> [a]
-- concat []         = []
-- concat (xs : xss) = xs ++ concat xss
-- This function takes a list of lists and flattens it to just one list. So concat [[1,2], [3], [4, 5, 6]] is [1,2,3,4,5,6].

-- Here’s how we might try to write it over Vecs:

--  concatRect :: Vec m (Vec n a) -> Vec (m * n) a
--  concatRect Nil         = Nil
--  concatRect (xs :> xss) = xs ++ concatRect xss
-- Note that we’re using type-level multiplication now, with *. This type family is defined above, in the introduction. (Its definition requires UndecidableInstances. That flag disables GHC’s very simplistic termination checker for type families. Enabling the extension means that GHC might not terminate when trying to compile your file. But that’s a risk we’ll have to take.)

-- The problem with concatRect is that it’s limiting. It allows us only to concatenate a rectangular arrangement of elements, where each Vec stored within the larger Vec has the same length n. This function is thus inapplicable to our initial example with the numbers 1 through 6.

-- In order to contemplate a more general concat, we need a new structure, for holding lists of Vecs of uneven lengths. But we still want the structure’s type to record their lengths, so that we can sum all the lengths in the type of concat. To do this, we’ll need type-level lists:

-- data VecList :: [Nat] -> Type -> Type where
--   VLNil :: VecList '[] a
--   (:>>) :: Vec n a -> VecList ns a -> VecList (n ': ns) a
-- infixr 5 :>>
-- The VecList type is indexed by a type-level list of type-level Nats. Each element in the list is the index for one of the Vecs stored in the list. (All elements have the same type a.) In the VLNil empty list case, we see that the type-level list of lengths is empty. (GHC requires that the promoted nil constructor and the promoted cons constructor are always written with their preceding ticks, as we see above.) In the :>> case, we get a Vec n a, a VecList ns a (that is, a VecList indexed by the type-level list of type-level Nats, which we’re calling ns), producing a VecList (n ': ns) a. That is, the result type just conses the new n onto the list ns.

-- We can now write the following translation of our original example:

-- stuffs = (1 :> 2 :> Nil) :>> (3 :> Nil) :>> (4 :> 5 :> 6 :> Nil) :>> VLNil
-- GHCi can tell us

-- λ> :t stuffs
-- stuffs
--  :: VecList
--       '['Succ ('Succ 'Zero), 'Succ 'Zero, 'Succ ('Succ ('Succ 'Zero))]
--       Integer
-- That should be no surprise. The index to the type of stuffs is '[2,1,3], corresponding to the lengths of the constituent elements of stuffs.

-- Writing concat is now relatively easy:

-- type family Sum (ns :: [Nat]) :: Nat where
--   Sum '[]       = Zero
--   Sum (n ': ns) = n + Sum ns
--   concat :: VecList ns a -> Vec (Sum ns) a
-- concat VLNil        = Nil
-- concat (xs :>> xss) = xs ++ concat xss
--Note that, in the type of concat, we must use a new type family Sum that sums up all the elements in ns. It would be ill-kinded to say Vec ns a, because Vec is indexed by a Nat, but ns is a [Nat].

import Data.Kind ( Type )
import Prelude hiding ((++), concat, length, gcastWith )
import qualified Prelude
import Data.Type.Equality

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)
type family a - b where
   a   - Zero           = a
   Succ a - Succ b  = a - b
   Zero - _ = Zero 

type family MakeLst (n :: Nat) :: [Nat] where
  MakeLst Zero = '[]
  MakeLst (Succ n) = (Succ n) : MakeLst n

type family x * y where
   Zero   * y = Zero
   Succ x * y = y + (x * y)

data Vec :: Nat -> Type -> Type where
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

type family (a::[Nat]) ++ (b::[Nat]) where
  '[] ++ b = b
  a ++ '[] = a
  (a:as) ++ b = a : as ++ b

type family NatLst (n ::Nat) :: [Nat] where
 NatLst Zero = '[]
 NatLst (Succ n') = NatLst n' ++ '[(Succ n')]

snoc :: Vec n a -> VecList ns a -> VecList (ns ++ '[n]) a
snoc xs VLNil = xs :>> VLNil
snoc xs (ys :>> yss) = ys :>> snoc xs yss
{-
append :: Vec n a -> Vec m a -> Vec (m + n) a
append v1 v2 = v1 ++ v2
-}
data VecList :: [Nat] -> Type -> Type where
  VLNil :: VecList '[] a
  (:>>) :: Vec n a -> VecList ns a
        -> VecList (n : ns) a
infixr 5 :>>

type family Sum ns where
  Sum '[]      = Zero
  Sum (n : ns) = Sum ns + n

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

pluss ::forall x y. SNat y -> SNat x -> (y + Succ x) :~: Succ (y + x)
pluss SZero _ = Refl
pluss y x = case plus_succ @x y of Refl -> Refl

intersperseHelp1 :: forall y x. SNat y -> SNat x -> (Succ y + Succ x) :~: Succ(Succ (y + x))
intersperseHelp1 SZero SZero =  Refl
intersperseHelp1 SZero (SSucc x') = Refl
intersperseHelp1 y@(SSucc y') SZero = case pluss y SZero of Refl -> Refl
intersperseHelp1 y@(SSucc y') x@(SSucc x') = case pluss y x of Refl -> Refl


intersperseHelp2 :: forall n a. a -> SNat n -> Vec n a -> Vec (n + n) a
intersperseHelp2 _ SZero Nil = Nil
intersperseHelp2 sep n@(SSucc n'::SNat n') (x:>xs) = case intersperseHelp1 n' n' of Refl -> sep :> x :> intersperseHelp2 sep n' xs

--The type family + is commutative.
plus_zero :: SNat n -> n + Zero :~: n
plus_zero SZero      = Refl
plus_zero (SSucc n') = case plus_zero n' of Refl -> Refl

plus_succ :: forall m n. SNat n -> (n + Succ m) :~: Succ (n + m)
plus_succ SZero      = Refl
plus_succ (SSucc sn') = case plus_succ @m sn' of Refl -> Refl

plus_comm :: SNat n -> SNat m -> n + m :~: m + n
plus_comm SZero                    sm = case plus_zero sm of Refl -> Refl
plus_comm (SSucc (sn' :: SNat n')) sm
--  = case plus_succ @n' sm of Refl -> case plus_comm sn' sm of Refl -> Refl
  = case plus_comm sn' sm of Refl -> case plus_succ @n' sm of Refl -> Refl

--gcastWith :: Refl a b -> ((a ~ b) => r) -> r
--gcastWith Refl x = x
--The type family + is associative.
--lemma: sum(xs++ys) == sum xs + sum ys given associativity of (+)
--sum (Nil ++ ys)    = sum ys           -- def (++)
--                   = Zero + sum ys       -- identity of addition
--                   = sum Nil ++ sum ys -- def sum

plus_assoc :: forall m n l. SNat m -> SNat n -> SNat l -> (m+n) + l :~: m + (n+l)
plus_assoc SZero n l = Refl
plus_assoc (SSucc m') n l = case plus_assoc m' n l of Refl -> Refl


-- type family * (as defined in the VecList lecture notes) is commutative
mult_zero :: SNat s -> Zero :~: (s * Zero)
mult_zero SZero = Refl
mult_zero (SSucc z') = case mult_zero z' of Refl-> Refl

mult_comm :: forall n m. SNat m-> SNat n -> (m*n) :~: (n*m)
mult_comm SZero n = case mult_zero n of Refl -> Refl
mult_comm m SZero = case mult_zero m of Refl -> Refl

--intersperse
intersperse :: forall n a. a -> SNat n -> Vec n a -> Vec (n + (n - Succ Zero)) a 
intersperse _ SZero Nil = Nil
intersperse s n@(SSucc n') (x:>xs) = case plus_succ @n n of Refl-> x :> intersperseHelp2 s n' xs

initsHelp :: Vec (Succ n) a -> Vec n a
initsHelp (x :> Nil) = Nil
initsHelp (x :> xs@(_ :> _))  = x :> initsHelp xs
--inits (You will need several new type families for this one. Write it on lists first as a warmup.)
inits :: forall m a. Vec m a -> VecList (NatLst m) a
inits Nil = VLNil
inits a@(x :> xs) = snoc a (inits (initsHelp a))
--take two inputs such that the first one works as the standard, and keep subtracting the second one to note the actual number. So here we need to use the (-) type family and the base case is zero zero = []

--tails (Ditto.)
tails :: Vec m a -> VecList (MakeLst m) a
tails Nil =  VLNil
tails x@(y :> ys) = x :>> tails ys
--intercalate (I found this to be pretty hard.)
--subsequences (I have not tried this one myself. It looks hard, but doable.)
--permutations (I have not tried this one myself. It looks hard, but doable.)
--transpose (Thinking about this makes me want to cry. But maybe you will enjoy it as a challenge problem.)