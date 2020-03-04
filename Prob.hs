{-# Language TupleSections #-}

module Prob where

import Control.Arrow (first, second)
import Data.Maybe (fromJust, isJust)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Ratio

newtype Prob n a = Prob {unProb :: [(n, a)]} deriving (Eq, Show, Ord)

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

maybeProb :: Prob n (Maybe a) -> Prob n a
maybeProb (Prob p) = Prob $ map (second fromJust) $ filter (isJust . snd) p

lossage :: (Num n) => Prob n a -> n
lossage (Prob xs) = 1 - sum (map fst xs)

entropy :: (Real n) => Prob n a -> Double
entropy (Prob xs) = sum $ map (l . toDouble . fst) xs
  where toDouble f = fromIntegral (numerator r) / fromIntegral (denominator r)
          where r = toRational f
        l x = -x * log x / log 2

toDoubles :: Real n => Prob n a -> Prob Double a
toDoubles (Prob xs) = Prob $ map (first toDouble) xs
  where toDouble f = fromIntegral (numerator r) / fromIntegral (denominator r)
          where r = toRational f

addLoss :: Prob Double String -> Prob Double String
addLoss p | loss < 1e-5 = p
          | otherwise = Prob $ (loss, "(loss)"):unProb p
          where loss = lossage p

table :: (Real n, Show a) => Prob n a -> String
table = table' . fmap show

table' :: Real n => Prob n String -> String
table' p = unlines $ map fmt $ sortBy (comparing (((-1)*) . fst)) $
           unProb $ addLoss $ toDoubles p
  where fmt (n, a) = (padStart 15 ' ' $ simp $ show (100 * n))
                     ++ "%   " ++ a
        simp [] = []
        simp ('.':x:y:xs) = '.':x:y:[]
        simp ('.':x:[]) = '.':x:[]
        simp "." = "."
        simp (x:xs) = x:simp xs
        padStart l c x | l <= length x = x
                       | otherwise = replicate (l - length x) c ++ x

stack :: [String] -> String
stack = unlines . stack' . map lines
  where stack' [] = []
        stack' [x] = x
        stack' (x:xs) = zipWithPad "" combine x $ stack' xs
          where combine s s' = padEnd len ' ' s ++ s'
                len = maximum (map length x) + 1
                padEnd l c x | l <= length x = x
                             | otherwise = x ++ replicate (l - length x) c
        zipWithPad :: a -> (a -> a -> b) -> [a] -> [a] -> [b]
        zipWithPad _ _ [] [] = []
        zipWithPad p f [] (b:bs) = f p b:zipWithPad p f [] bs
        zipWithPad p f (a:as) [] = f a p:zipWithPad p f as []
        zipWithPad p f (a:as) (b:bs) = f a b:zipWithPad p f as bs

chead :: String -> String -> String
chead top table = left ++ top ++ "\n" ++ table
  where len = maximum (map length $ lines table) - length top
        left = replicate (len `div` 2) ' '

rhead :: String -> String -> String
rhead top table = left ++ top ++ "\n" ++ table
  where len = maximum (map length $ lines table) - length top
        left = replicate len ' '
