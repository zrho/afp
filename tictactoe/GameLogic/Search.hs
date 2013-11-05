module GameLogic.Search
  ( Tree (..)
  , unfoldTree
  , prune
  , takeTree
  , maximize
  , minimize
  , maximizeAB
  , minimizeAB
  , selectMaxAB
  , lowFirst
  , highFirst
  ) where
import Data.List
import Data.Function
import Prelude

-- | Generic tree data structure.
data Tree a = Node a [Tree a] deriving (Eq, Show)

instance Functor Tree where
  fmap f (Node x ns) = Node (f x) $ fmap (fmap f) ns

instance Ord a => Ord (Tree a) where
  compare (Node x _) (Node y _) = compare x y

--------------------------------------------------------------------------------
-- * Basic manipulation

unfoldTree :: (a -> [a]) -> a -> Tree a
unfoldTree f x = Node x $ fmap (unfoldTree f) $ f x

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ns) = Node x $ fmap (prune $ n - 1) ns

takeTree :: Int -> Tree a -> Tree a
takeTree n (Node x ns) = Node x $ take n ns

--------------------------------------------------------------------------------
-- * Alternating Min/Max

--------------------------------------------------------------------------------
-- ** Naive

maximize :: Ord a => Tree a -> a
maximize (Node x []) = x
maximize (Node x ns) = maximum $ fmap minimize ns

minimize :: Ord a => Tree a -> a
minimize (Node x []) = x
minimize (Node x ns) = minimum $ fmap maximize ns

selectMax :: Ord a => Tree a -> Maybe (Tree a)
selectMax (Node x []) = Nothing 
selectMax (Node x ns) = Just $ fst $ maximumBy (compare `on` snd) $ fmap (\n -> (n, minimize n)) ns

--------------------------------------------------------------------------------
-- ** Alpha Beta Pruning

maximizeAB :: Ord a => Tree a -> a
maximizeAB = maximum . maximizeAB'

maximizeAB' :: Ord a => Tree a -> [a]
maximizeAB' (Node x []) = [x]
maximizeAB' (Node x ns) = mapMin $ fmap minimizeAB' ns where
  -- Omit all subtrees in which the minimum is lower or equals than pot
  mapMin (x:xs)   = minimum x : omit (minimum x) xs
  omit pot []     = []
  omit pot (x:xs) | minLeq x pot = omit pot xs
                  | otherwise    = minimum x : omit (minimum x) xs
  minLeq xs pot   = foldr (\x b -> b || x <= pot) False xs

minimizeAB :: Ord a => Tree a -> a
minimizeAB = minimum . minimizeAB'

minimizeAB' :: Ord a => Tree a -> [a]
minimizeAB' (Node x []) = [x]
minimizeAB' (Node x ns) = mapMax $ fmap maximizeAB' ns where
  mapMax (x:xs)   = maximum x : omit (maximum x) xs
  omit pot []     = []
  omit pot (x:xs) | maxGeq x pot = omit pot xs
                  | otherwise    = maximum x : omit (maximum x) xs
  maxGeq xs pot   = foldr (\x b -> b || x >= pot) False xs

selectMaxAB :: Ord a => Tree a -> Maybe (Tree a)
selectMaxAB (Node x []) = Nothing 
selectMaxAB (Node x ns) = Just $ fst $ maximumBy (compare `on` snd) $ fmap (\n -> (n, minimizeAB n)) ns

--------------------------------------------------------------------------------
-- ** Reordering

lowFirst :: Ord a => Tree a -> Tree a
lowFirst (Node x ns) = Node x $ sort $ fmap highFirst ns

highFirst :: Ord a => Tree a -> Tree a
highFirst (Node x ns) = Node x $ sortBy (reverseOrder compare) $ fmap lowFirst ns

reverseOrder :: (a -> a -> Ordering) -> a -> a -> Ordering
reverseOrder f a b = case f a b of
  LT -> GT
  GT -> LT
  EQ -> EQ
