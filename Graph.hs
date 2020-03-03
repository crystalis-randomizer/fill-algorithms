{-# Language BangPatterns #-}

module Graph where

import Debug.Trace (trace)

import Data.Char (chr, ord)
import Data.Foldable (foldMap)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid (First(..))
import qualified Data.Set as S
import Data.Set (Set)
import Prob

tr :: Show a => String -> a -> a
tr !msg !v = trace ((++ ": " ++ show v) $! msg) $! v

-- DNF (i.e. ["A", "B"] means A or B, ["AB"] means A and B)
newtype Graph = Graph {unGraph :: [[String]]} deriving (Show)

locations :: Graph -> [Int]
locations (Graph g) = [0 .. length g - 1]

items :: Graph -> String
items = map (chr . (ord 'A'+)) . locations

progressionItems :: Graph -> Set Char
progressionItems (Graph g) = foldMap (foldMap S.fromList) g

graph :: [String] -> Graph
graph xs = Graph $ map (\x -> case x of
                                "" -> [""]
                                _ -> wordsBy (=='|') x) xs

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p c =  case dropWhile p c of
                      [] -> []
                      c' -> w : wordsBy p c''
                          where (w, c'') = break p c'

newtype Filling = Filling {unFill :: Map Int Char} deriving (Eq, Ord)

instance Show Filling where
  show (Filling m) | M.null m = "Filling []"
                   | otherwise = "Filling " ++ map ch [0..end]
    where end = maximum $ M.keys m
          ch i = maybe ' ' id $ M.lookup i m

emptyFill :: Filling
emptyFill = Filling $ M.empty

initialFill :: String -> Filling
initialFill elems = Filling $ foldMap check $ zip [0..] elems
  where check (i, ' ') = M.empty
        check (i, c) = M.singleton i c

addFill :: Int -> Char -> Filling -> Filling
addFill i c = Filling . M.insert i c . unFill

trav :: Graph -> Filling -> Set Char -> Set Int
trav (Graph g) (Filling f) ihas = pass ihas S.empty
  where enum :: [(Int, [String])]
        enum = zip [0..] g
        pass :: Set Char -> Set Int -> Set Int
        pass has reachable = case reachable == nextReachable of
                               True -> reachable
                               False -> pass nextHas nextReachable
          where nextReachable :: Set Int
                nextReachable = foldMap checkLoc enum
                checkLoc :: (Int, [String]) -> Set Int
                checkLoc (i, []) = S.empty
                checkLoc (i, (s:ss)) = -- tr ("checkLoc " ++ show (i, s)) $
                                       case all (flip S.member has) s of
                                         True -> S.singleton i
                                         False -> checkLoc (i, ss)
                nextHas :: Set Char
                nextHas = mappend has $ foldMap getItem nextReachable
                getItem :: Int -> Set Char
                getItem i = case M.lookup i f of
                              Just v -> S.singleton v
                              Nothing -> S.empty

fillings :: Int -> [Filling]
fillings n = map initialFill $ fill alphabet
  where alphabet :: Set Char
        alphabet = S.fromList $ map (chr . (ord 'A'+)) [0..n-1]
        fill :: Set Char -> [String]
        fill s | S.null s = [""]
               | otherwise = do x <- S.toList s
                                f <- fill (S.delete x s)
                                return $! x : f

valid :: Graph -> Filling -> Bool
valid g f = S.size (trav g f S.empty) == length (unGraph g)

randomFill :: Fractional n => Graph -> Prob n Filling
randomFill g = uniform $ filter (valid g) $ fillings $ length $ unGraph g

assumedFillInternal :: Fractional n => Graph -> Prob n Char -> Prob n Filling
assumedFillInternal g dist = fill emptyFill dist $ S.fromList $ probToList dist
  where fill f dist has
          | S.null has = uniform [f]
          | otherwise = do i <- dist
                           let remaining = S.delete i has
                               reachable = S.toList $ trav g f remaining
                               fillable = filter unfilled reachable
                                 where unfilled x = not $ M.member x $ unFill f
                           l <- uniform fillable
                           let d2 = normalize $ filterProb (\x -> x /= i) dist
                           fill (addFill l i f) d2 remaining

assumedFill :: Fractional n => Graph -> Prob n Filling
assumedFill g = assumedFillInternal g $ uniform $ items g

assumedFill2 :: Fractional n => Graph -> Prob n Filling
assumedFill2 g = assumedFillInternal g $ uniform $ S.toList $ progressionItems g

position :: Char -> Filling -> Maybe Int
position c (Filling f) = getFirst $ M.foldMapWithKey check f
  where check :: Int -> Char -> First Int
        check k a | a == c = First $ Just k
                  | otherwise = First $ Nothing

itemAt :: Int -> Filling -> Maybe Char
itemAt i (Filling f) = M.lookup i f
