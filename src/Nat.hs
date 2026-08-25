{-# LANGUAGE StrictData #-}

module Nat where

import Prelude hiding (Int, Real)
import Numeric.Natural (Natural)
import Data.Ratio ((%))

type Nat = Natural
type Int = Integer
type Rat = Rational

-- first field is the min error
data Real = RatR !Rat !Rat | LimR !Rat !(Rat -> Rat)

-- | addReal wght a b, 0 < wght < 1, adds a and b, and if both a and b are
-- stored as fuzzy approximators, then approximating (addReal w ...) to within
-- eps aprroximates a within epsa + emme * w and b to within
-- epsb + emme * (1 - w) (its better to add with weights 0 (or 1) when we know
-- that the approximator of the first (or the second) number is a constant
-- function (which we do when we know that its constructor is RatR))
addReal :: Rat -> Real -> Real -> Real
addReal _    (RatR epsa a) (RatR epsb b) = RatR (epsa + epsb) (a + b)
addReal _    (RatR epsa a) (LimR epsb b) =
  LimR (epsa + epsb) (\eps -> a + b (eps - epsa))
addReal _    (LimR epsa a) (RatR epsb b) =
  LimR (epsa + epsb) (\eps -> a (eps - epsb) + b)
addReal wght (LimR epsa a) (LimR epsb b) =
  LimR
  (epsa + epsb)
  (\eps ->
      let emme = eps - epsa - epsb
      in (a $ epsa + emme * wght) + (b $ epsb + emme * (1 - wght)))

mulReal :: Rat -> Real -> Real -> Real
mulReal _ (RatR epsa a) (RatR epsb b) =
  RatR me centre where
  me = centre - bot
  centre = (top + bot) / 2
  ends =
    [ (a + epsa) * (b + epsb)
    , (a + epsa) * (b - epsb)
    , (a - epsa) * (b + epsb)
    , (a - epsa) * (b - epsb)]
  top = maximum ends
  bot = minimum ends
mulReal _ (RatR epsa a) (LimR epsb b) = LimR me centre where
  me = undefined
  centre eps = undefined
mulReal _ _ _ = error "mulReal not implemented"

-- | centre - a[epsa] * b[d] | < eps

instance Num Real where
  a + b = addReal (1 % 2) a b
