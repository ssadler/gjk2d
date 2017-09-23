import Data.Function
import Data.List
import GJK
import Linear

main :: IO ()
main = do
  let terr = V2 <$> [0.1,0.2 .. 10] <*> [0.1,0.2 .. 10]
  -- let polygons = (\v1 v2 v3 v4 -> [v1, v2, v3, v4]) <$> terr <*> terr <*> terr <*> terr
  let cirtest =
        (\cir1 cir2 ->
           ((cir1, cir2), convexIntersect (circle cir1) (circle cir2))) <$>
        terr <*>
        terr
  -- traverse (\s -> if snd s then print (fst s) else return ()) cirtest
  print $ length $ filter snd $ cirtest
  -- print $ convexIntersect (circle $ V2 0.2 7.2) (circle $ V2 0.1 9.1)
  print $
    (uncurry (convexIntersect `on` fromPolygon)) <$>
    [(poly1, poly2), (poly1, poly3), (poly2, poly3), (poly1, poly4)]
  print $ convexIntersect circle2 (fromPolygon poly3)
  print $ convexIntersect circle1 (fromPolygon poly3)

fromPolygon :: [V2 Double] -> Convex
fromPolygon vs = Convex $ \d -> maximumBy (compare `on` dot (angle d)) vs

poly1 = [V2 1 1, V2 1 0, V2 0 1, V2 0 0]

poly2 = fmap (+ 0.5) <$> [V2 1 1, V2 1 0, V2 0 1, V2 0 0]

poly3 = (+ 1) <$> [V2 1 1, V2 1 0, V2 0 1, V2 0 0]

poly4 = (+ 1.5) <$> [V2 1 1, V2 1 0, V2 0 1, V2 0 0]

circle1 = Convex $ \d -> angle d

circle2 = Convex $ \d -> angle d + V2 0.5 0

circle pos = Convex $ \d -> angle d + pos
