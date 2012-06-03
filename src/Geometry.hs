module Geometry where

import Data.Vect.Double
import Types

-- Every geometry must be able to tell me the AABB, and whether a point is
-- inside it.
class Geometry a where
  contains :: a -> Point -> Bool
  aabb     :: a -> AABB

instance Geometry Circle where
  contains (Circle centre r) point = len (point &- centre) <= r
  aabb (Circle (Vec2 x y) r) = AABB (x-r) (x+r) (y-r) (y+r)
