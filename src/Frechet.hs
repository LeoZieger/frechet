{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frechet where

import Data.Map as M (insert, lookup, empty, Map)
import Data.Array ( Ix(range), Array, (!), array )

type Point = (Float, Float)
type Curve = [Point]
type State = (Point, Point)
type Path = [State]


curveSIX :: Curve
curveSIX = [(11,14), (7,13), (5,11), (4,8), (5,4), (8,3), (11,4), (11,8), (8,9), (5,8)]

curveNINE :: Curve
curveNINE = [(11,9), (8,8), (5,9), (5,13), (8,14), (11,13), (12,9), (11,6), (9,4), (5,3)]

curveTHREE :: Curve
curveTHREE = [(6,13), (10,14), (13,12), (13, 10), (6,9), (13,8), (13,6), (10,4), (6,5)]

curveUNKNOWN :: Curve
curveUNKNOWN = [(4,11), (6,12), (7,13), (9,14), (11,13), (12,11), (11,9), (9,7), (8,4), (5,4)]


curveLong1 :: Curve
curveLong1 = take 600 $ zip [1..] [40..]

curveLong2 :: Curve
curveLong2 = take 600 $ zip [40..] [30..]


data FTree = Node State [FTree]
            deriving (Eq, Show)



dist :: Point -> Point -> Float
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^(2::Int) + (y1-y2)^(2::Int))

mkFTree :: Curve -> Curve -> FTree
mkFTree [] _ = error "Empty Curve 1"
mkFTree _ [] = error "Empty Curve 2"
mkFTree [p1] [q1] = Node (p1, q1) []
mkFTree (p1:p2:ps) [q1] = Node (p1, q1) [mkFTree (p2:ps) [q1]]
mkFTree [p1] (q1:q2:qs) = Node (p1, q1) [mkFTree [p1] (q2:qs)]
mkFTree (p1:p2:ps) (q1:q2:qs) = Node (p1, q1) [l,m,r]
  where
    l = mkFTree (p2:ps) (q1:q2:qs)
    m = mkFTree (p2:ps) (q2:qs)
    r = mkFTree (p1:p2:ps) (q2:qs)


frechetTree :: Curve -> Curve -> Float
frechetTree c1 c2 = minCost $ allPaths (mkFTree c1 c2)
  where
    minCost :: [Path] -> Float
    minCost [] = error "Empty path"
    minCost [p] = pathCost p
    minCost (p:ps) = min (pathCost p) (minCost ps)

    allPaths :: FTree -> [Path]
    allPaths (Node s []) = [[s]]
    allPaths (Node s ts) = map (s:) (concatMap allPaths ts)

    pathCost :: Path -> Float
    pathCost [] = 0
    pathCost ((p,q):xs) = max (dist p q) (pathCost xs)



frechetRec :: Curve -> Curve -> Float
frechetRec [p] [q] = dist p q
frechetRec c1 c2 | length c1 == 1 && length c2 > 1 = max (dist pn qn) (frechetRec c1 qr)
                     | length c1 > 1 && length c2 == 1 = max (dist pn qn) (frechetRec pr c2)
                     | length c1 > 1 && length c2 > 1 = max (dist pn qn) (minimum [frechetRec pr c2,
                                                                                   frechetRec pr qr,
                                                                                   frechetRec c1 qr])
  where
    (pr,pn) = partitionLast c1
    (qr,qn) = partitionLast c2

    partitionLast :: [a] -> ([a],a)
    partitionLast = go []
      where
        go :: [a] -> [a] -> ([a], a)
        go _ [] = undefined
        go acc [x] = (acc, x)
        go acc (x:xs) = go (acc ++ [x]) xs



frechetMap :: Curve -> Curve -> Float
frechetMap c1 c2 = fst $ go M.empty l1 l2
  where
    l1 = length c1 -1
    l2 = length c2 -1

    allDists :: [[Float]]
    allDists = [[dist p q | q <- c2] | p <- c1]

    go :: Map (Int,Int) Float -> Int -> Int -> (Float, Map (Int,Int) Float)
    go m i j
      | i < 0 || j < 0 = error $ show (i,j) ++ " negativ"
      | i == 0 && j == 0 = ((allDists !! i) !! j, m)
      | i >= 1 && j == 0 = case M.lookup (i,j) m of
                              Just x -> (x,m)
                              Nothing -> let (f1,m1) = go m (i-1) j
                                             d = (allDists !! i) !! j
                                             res = max d f1
                                         in (res, M.insert (i,j) res m1)
      | i == 0 && j >= 1 = case M.lookup (i,j) m of
                              Just x -> (x,m)
                              Nothing -> let (f1,m1) = go m i (j-1)
                                             d = (allDists !! i) !! j
                                             res = max d f1
                                         in (res, M.insert (i,j) res m1)
      | otherwise = case M.lookup (i,j) m of
                              Just x -> (x,m)
                              Nothing -> let (f1,m1) = go m (i-1) j
                                             (f2,m2) = go m1 (i-1) (j-1)
                                             (f3,m3) = go m2 i (j-1)
                                             d = (allDists !! i) !! j
                                             res = max d (minimum [f1,f2,f3])
                                         in (res, M.insert (i,j) res m3)



frechetArr :: Curve -> Curve -> Float
frechetArr c1 c2 = frechets ! (l1,l2)
  where
    l1 = length c1 -1
    l2 = length c2 -1

    allDists :: [[Float]]
    allDists = [[dist p q | q <- c2] | p <- c1]

    tabulate :: Ix i => (i -> e) -> (i,i) -> Array i e
    tabulate f bounds = array bounds [ (x, f x) | x <- range bounds ]

    frechets :: Array (Int,Int) Float
    frechets = tabulate f ((0,0), (l1,l2))

    f (i,j)
      | i == 0 && j == 0 = (allDists !! i) !! j
      | i >= 1 && j == 0 = max (frechets ! (i-1,j)) ((allDists !! i) !! j)
      | i == 0 && j >= 0 = max (frechets ! (i,j-1)) ((allDists !! i) !! j)
      | otherwise = max ((allDists !! i) !! j) (minimum [frechets ! (i-1,j),
                                                         frechets ! (i-1,j-1),
                                                         frechets ! (i,j-1)])
