module GJK where

import Linear

-- | Convex is represented by its supprot function
newtype Convex = Convex { support :: Double -> V2 Double }

-- | Minkowski Difference between 2 Convexes
minkowskiDifference :: Convex -> Convex -> Convex
minkowskiDifference a b = Convex $ (-) <$> support a <*> (support b . (+ pi))

-- | Compute angle of a vector
unAngle :: V2 Double -> Double
unAngle a@(V2 ax ay) =
  let alpha = asin $ ay / norm a
  in if ax < 0
       then pi - alpha
       else alpha

-- | Core Logic: Detect if origin point is contained inside a Convex
originInside :: Convex -> Bool
originInside s =
  if dot a b > 0
    then False
    else doSimplex (support s) a b
  where
    a = support s 0
    b = support s $ unAngle (-a)

-- | Simplex addition till nearZero
doSimplex :: (Double -> V2 Double) -> V2 Double -> V2 Double -> Bool
doSimplex s a b =
  nearZero (b - a) ||
  (if (axb > 0) /= (bxc > 0)
     then doSimplex s b c
     else (if (axc > 0) /= (cxb > 0)
             then doSimplex s a c
             else True))
  where
    c = s $ unAngle (b - a) + (if axb > 0 then pi / 2 else -pi / 2)
    axb = crossZ a b
    axc = crossZ a c
    bxc = crossZ b c
    cxb = -bxc

-- | If two Convex colliside
convexIntersect :: Convex -> Convex -> Bool
convexIntersect a b = originInside $ minkowskiDifference a b
