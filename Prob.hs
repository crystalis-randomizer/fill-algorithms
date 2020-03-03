{-# Language TupleSections #-}

module Prob where

import qualified Data.Map as M
import Data.Ratio

newtype Prob n a = Prob {unProb :: [(n, a)]} deriving (Eq, Show)

instance Functor (Prob n) where
  fmap f p = Prob [(n, f a) | (n, a) <- unProb p]

instance Fractional n => Applicative (Prob n) where
  pure a = Prob [(fromRational (1 % 1), a)]
  pf <*> pa = Prob [(nf * na, f a) | (nf, f) <- unProb pf, (na, a) <- unProb pa]

instance Fractional n => Monad (Prob n) where
  fail _ = Prob []
  pa >>= f = Prob [(na * nb, b) | (na, a) <- unProb pa, (nb, b) <- unProb $ f a]

-- Note: normalize is one way to effectively "prevent backtracking",
-- or else possibly establish a baseline for backtracking?
normalize :: Fractional n => Prob n a -> Prob n a
normalize (Prob []) = Prob []
normalize (Prob xs) = Prob $ map div xs
  where div (n, a) = (n / s, a)
        s = sum (map fst xs)

uniform :: Fractional n => [a] -> Prob n a
uniform [] = Prob []
uniform xs = Prob $ map (p,) xs
  where p = fromRational $ 1 % fromIntegral (length xs)

consolidate :: (Ord a, Num n) => Prob n a -> Prob n a
consolidate (Prob p) = add p M.empty
  where add [] m = Prob $ map swap $ M.toList m
        add ((n, a):xs) m = add xs $ M.insertWith (+) a n m
        swap (a, b) = (b, a)

probToList :: Prob n a -> [a]
probToList (Prob p) = map snd p

filterProb :: (a -> Bool) -> Prob n a -> Prob n a
filterProb f (Prob p) = Prob $ filter (f . snd) p

lossage :: (Num n) => Prob n a -> n
lossage (Prob xs) = 1 - sum (map fst xs)

entropy :: (Real n) => Prob n a -> Double
entropy (Prob xs) = sum $ map (l . toDouble . fst) xs
  where toDouble f = fromIntegral (numerator r) / fromIntegral (denominator r)
          where r = toRational f
        l x = -x * log x / log 2
