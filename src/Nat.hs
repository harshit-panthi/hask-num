{-# LANGUAGE StrictData #-}

module Nat where

import Prelude hiding (Int, Real)
import Numeric.Natural (Natural)
import Data.Ratio ((%))
import Data.List (scanl')

type Nat = Natural
type Int = Integer
type Rat = Rational

fact :: Nat -> Nat
fact n = foldl' (*) 1 [1..n]

-- Reals are represented as Cauchy sequences
data Real = Real !(Nat -> Rat) !(Rat -> Nat)

instance Num Real where
  (Real a conva) + (Real b convb) =
    Real (\n -> (a n) + (b n))
    (\eps -> max (conva $ eps / 2) (convb $ eps / 2))
    
  (Real a conva) * (Real b convb) = Real (\n -> (a n) * (b n)) (\eps -> n' eps) where
    m :: Rat -> Rat
    m eps =
      max
      (foldl' (max) (0 % 1) $ ((1 % 1) + (a $ (conva eps) + 1)) : (map a [0..(conva eps)]))
      (foldl' (max) (0 % 1) $ ((1 % 1) + (b $ (convb eps) + 1)) : (map b [0..(convb eps)]))

    n' :: Rat -> Nat
    n' eps = max (conva $ eps / ((2 % 1) * (m eps))) (convb $ eps / ((2 % 1) * (m eps)))
    
  abs (Real a conva) = Real (abs . a) conva
  
  negate (Real a conva) = Real (negate . a) conva

  signum (Real a _) = Real (signum . a)
    (error "dont use the signum of (Num Real), use sgn instead")

  fromInteger n = Real (\_ -> (n % 1)) (\_ -> 0)

approx :: Rat -> Real -> Rat
approx eps (Real a conva) = a $ conva (eps / 2) 

factorials :: [Integer]
factorials = scanl' (*) 1 [1..]

e_taylor_term :: [Rat]
e_taylor_term = map (\n -> 1 / fromIntegral n) factorials

e_taylor_series_list :: [Rat]
e_taylor_series_list = scanl' (+) 0 (e_taylor_term)

e_taylor_series_seq :: Nat -> Rat
e_taylor_series_seq n = e_taylor_series_list !! fromIntegral n

e :: Real
e = Real e_taylor_series_seq (\eps -> max 1 ((ceiling $ 1 / eps) - 1))

