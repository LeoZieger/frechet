{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Frechet where

type Point = (Float, Float)
type Curve = [Point]
type State = (Point, Point)
type Path = [State]


curveONE :: Curve
curveONE = [(4,8), (7,10), (7,3)]

curveSEVEN :: Curve
curveSEVEN = [(5,10), (9,10), (6,3)]

curveFIVE :: Curve
curveFIVE = [(8,10), (4,10), (4,7), (8,7), (8,3), (4,3)]

curveUNKNOWN :: Curve
curveUNKNOWN = [(4,9), (7,10), (9,8), (8,3)]


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
