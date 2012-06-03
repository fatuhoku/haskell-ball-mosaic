{-# LANGUAGE TemplateHaskell #-}
module Types where

import Graphics.Rendering.OpenGL hiding (Radius)

import qualified Data.Vector as V
import Data.Vect.Double
import Data.Label

-- Geometry
type MouseCoord = (Int,Int)
type StdColor = Color4 GLdouble
type Point  = Vec2
type Centre = Point
type Radius = Double

data Circle = Circle {
  _centre :: Centre,
  _radius :: Radius
  }

data AABB = AABB Double Double Double Double  -- minX minY maxX maxY

-- A list of objects!

-- For simplicity, we allow the geometry to define its position in
-- world-space. The geometry needs only be enriched with a colour for us to
-- render the scene node correctly.
-- OR DO WE?
-- The Scene is a logical structure and grouping of areas in the scene.
-- We wish to see the circles translated, but not rotated nor scaled. If we did,
-- we have to create the scene nodes in quite a different way.
data SceneNode = CircleSn {
  _snCircle :: Circle,
  _snColour :: Color4 GLdouble
  }

data Scene = Scene {
  _sceneNodes :: V.Vector SceneNode
  }

$(mkLabels [''Circle,''SceneNode,''Scene])
