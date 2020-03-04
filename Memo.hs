module Memo where

import Control.Monad.State.Strict
import Data.IORef
-- .Strict ??
import qualified Data.Map as M
import Data.Map (Map)
import System.IO.Unsafe (unsafePerformIO)

-- Ask for a value in the cache. If it's not there, run the state computation
-- and insert the result into the cache.

memoized :: (Ord k) => IORef (Map k v) -> (k -> v) -> k -> v
memoized ref f k = unsafePerformIO $ do
  memo <- readIORef ref
  case M.lookup k memo of
    Just v -> return v
    Nothing -> do
      let v = f k
      writeIORef ref $ M.insert k v memo
      return v

memoize :: (Ord k) => (k -> v) -> k -> v
memoize f = unsafePerformIO $ do
  memo <- newIORef M.empty
  return $ memoized memo f

memoize2 :: (Ord a, Ord b) => (a -> b -> v) -> a -> b -> v
memoize2 = curry . memoize . uncurry

memfix :: (Ord k) => IORef (Map k v) -> ((k -> v) -> k -> v) -> k -> v
memfix memo f = let x = f (memoized memo x) in x

memoizefix :: (Ord k) => ((k -> v) -> k -> v) -> k -> v
memoizefix f = unsafePerformIO $ do
  memo <- newIORef M.empty
  return $ memfix memo f

uncurryrec :: ((a -> b -> v) -> a -> b -> v) -> ((a, b) -> v) -> (a, b) -> v
uncurryrec f g (a, b) = f (curry g) a b

uncurryrec3 :: ((a -> b -> c -> v) -> a -> b -> c -> v)
            -> ((a, b, c) -> v) -> (a, b, c) -> v
uncurryrec3 f g (a, b, c) = f (curry3 g) a b c

curry3 :: ((a, b, c) -> v) -> a -> b -> c -> v
curry3 f a b c = f (a, b, c)

memoizefix2 :: (Ord a, Ord b) => ((a -> b -> v) -> a -> b -> v) -> a -> b -> v
memoizefix2 = curry . memoizefix . uncurryrec

memoizefix3 :: (Ord a, Ord b, Ord c) => ((a -> b -> c -> v) -> a -> b -> c -> v)
            -> a -> b -> c -> v
memoizefix3 = curry3 . memoizefix . uncurryrec3


-- getOrUpdate :: (Ord k) => k -> State (Map k v) v -> State (Map k v) v
-- getOrUpdate k ifEmptyState = do
--   maybeVal <- gets (M.lookup k)
--   case maybeVal of
--     Just v -> return v
--     Nothing -> do
--       ifEmpty <- ifEmptyState
--       modify (M.insert k ifEmpty)
--       return ifEmpty

-- stateMemoFibs :: Int -> State (Map Int Integer) Integer
-- stateMemoFibs 0 = return 0
-- stateMemoFibs 1 = return 1
-- stateMemoFibs n = do
--     -- Try and get the n-2 and n-1 fib from the cache. If they're not there,
--     -- calculate them recursively and update the cache.
--     n2 <- getOrUpdate (n-2) (stateMemoFibs (n-2))
--     n1 <- getOrUpdate (n-1) (stateMemoFibs (n-1))
--     return (n2 + n1)
