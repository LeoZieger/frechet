{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use min" #-}
{-# HLINT ignore "Use max" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# HLINT ignore "Use second" #-}
{-# LANGUAGE NamedFieldPuns #-}

module Utils 
  ( unwrap
  , wrap
  , single
  , pairWith
  , picks
  , pairs
  , insertBy
  , apply
  , fib
  , qsort
  , select
  , median
  , middle
  , minWith
  , minsWith
  , SymList(..)
  , fromSL
  , nilSL
  , initSL
  , tailSL
  , lastSL
  , headSL
  , snocSL
  , consSL
  , singleSL
  , nullSL
  ) where

unwrap :: [a] -> a
unwrap [a] = a

wrap :: a -> [a]
wrap x = [x]

single :: [a] -> Bool
single [_] = True
single _ = False

pairWith :: (a -> a -> a) -> [a] -> [a]
pairWith _ [] = []
pairWith _f [a] = [a]
pairWith f (a : a' : as) = f a a' : as

-- pick all possible elements from a list and return the element together with the list w/ the element removed
picks :: [a] -> [(a,[a])]
picks [] = []
picks (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (picks xs)

-- like pick, but return pairs
pairs :: [a] -> [(a,a,[a])]
pairs xs = [ (x,y,xs'') | (x,xs') <- picks xs, (y,xs'') <- picks xs' ]

insertBy :: Ord b => (a -> b) -> a -> [a] -> [a]
insertBy _ a [] = [a]
insertBy f x (y:ys) = if f x < f y then x : y : ys else y : insertBy f x ys

apply :: Int -> (a -> a) -> (a -> a)
apply 0 _ = id
apply n f = f . apply (n-1) f

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

partition3 :: Ord a => a -> [a] -> ([a], [a], [a])
partition3 a = foldr op ([],[],[])
  where
    op x (ls,ms,rs) =
      case compare x a of
        LT -> (x:ls,ms,rs)
        EQ -> (ls,x:ms,rs)
        GT -> (ls,ms,x:rs)

group :: Int -> [a] -> [[a]]
group _n [] = []
group n  xs = ys : group n zs
  where 
    (ys,zs) = splitAt n xs

middle :: [a] -> a
middle xs = xs !! (((n + 1) `div` 2) - 1)
  where
    n = length xs

medians :: Ord a => [a] -> [a]
medians = map (middle . sort) . group 5
  where
    sort = qsort

select :: Ord a => Int -> [a] -> a
select k xs
  | k <= m     = select k ls
  | k <= m + n = ms !! (k - m - 1)
  | k >  m + n = select (k - m - n) rs
  where
    (ls,ms,rs) = partition3 (pivot xs) xs
    m = length ls
    n = length ms
    pivot [x] = x
    pivot xs = median (medians xs)
    median xs = select ((length xs + 1) `div` 2) xs

median :: Ord a => [a] -> a
median xs = select ((n + 1) `div` 2) xs
  where
    n = length xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort xs = qsort ls ++ ms ++ qsort rs
  where
    (ls,ms,rs) = partition3 (pivot xs) xs
    pivot [x] = x
    pivot xs = median (medians xs)
    median xs = select ((length xs + 1) `div` 2) xs

-- find minimum according to a projection function
minWith :: Ord b => (a -> b) -> [a] -> a
minWith f = foldr1 (smaller f)
  where
    smaller f x y = if f x < f y then x else y

-- find all minima according to a projection function
minsWith :: Ord b => (a -> b) -> [a] -> [a]
minsWith f = foldr (smaller f) []
  where
    smaller _ x [] = [x]
    smaller f x ys@(y:_) = case compare (f x) (f y) of
      LT -> [x]
      EQ -> x:ys
      GT -> ys

-- symmetric lists
-- access to front and back is amortized constant
-- invariant:
--   - null front  => null back  || single back
--   - null back   => null front || single front
data SymList a
  = MKSymList { sl_front :: [a], sl_back :: [a] }
  deriving (Show,Eq,Ord)

fromSL :: SymList a -> [a]
fromSL sl = sl_front sl ++ reverse (sl_back sl)

nilSL :: SymList a
nilSL = MKSymList [] []

nullSL :: SymList a -> Bool
nullSL sl = null (sl_front sl) && null (sl_back sl)

singleSL :: SymList a -> Bool
singleSL MKSymList { sl_front, sl_back }
  = null sl_front && single sl_back
  || null sl_back && single sl_front

consSL :: a -> SymList a -> SymList a
consSL a sl@MKSymList{ sl_front, sl_back } =
  if null sl_back
    then MKSymList { sl_front = [a], sl_back = sl_front }
    else sl { sl_front = a : sl_front }

snocSL :: a -> SymList a -> SymList a
snocSL a sl@MKSymList{ sl_front, sl_back } =
  if null sl_front
    then MKSymList { sl_back = [a], sl_front = sl_back }
    else sl { sl_back = a : sl_back }

headSL :: SymList a -> a
headSL MKSymList { sl_front, sl_back } =
  if null sl_front
    then head sl_back --sl_back must be a singleton due to invariant
    else head sl_front
lastSL :: SymList a -> a
lastSL MKSymList { sl_front, sl_back } =
  if null sl_back
    then head sl_front --sl_front must be a singleton due to invariant
    else head sl_back

tailSL :: SymList a -> SymList a
tailSL sl@MKSymList { sl_front, sl_back } 
  | null sl_front = nilSL
  | single sl_front = let (bs,fs) = splitAt (length sl_back `div` 2) sl_back
                      in MKSymList { sl_front = reverse fs, sl_back = bs }
  | otherwise = sl { sl_front = tail sl_front }
initSL :: SymList a -> SymList a
initSL sl@MKSymList { sl_front, sl_back } 
  | null sl_back = nilSL
  | single sl_back = let (fs,bs) = splitAt (length sl_front `div` 2) sl_front
                      in MKSymList { sl_front = fs, sl_back = reverse bs }
  | otherwise = sl { sl_back = tail sl_back }

