import Prelude hiding (id,(.))

import System.Random
import Control.Category
import Graphics.Rendering.OpenGL hiding (R, get)
import Graphics.UI.GLFW

import qualified Data.Vector as V
import Data.Label
import Data.List
import Data.Vect.Double
import Data.Vect.Double.OpenGL
import Data.IORef
import Control.Concurrent

import Types
import Circle
import Geometry
import Debug.Trace
import Glfw
import Control.Monad

-- HOW TO COMPILE APPLICATION WITH NO TERMINAL
-- URL: http://www.haskell.org/ghc/docs/7.0.2/html/users_guide/terminal-interaction.html
-- Simply pass -optl-mwindows in the link step.
--
-- Create a Glfw window, create a circle, and allow it to be moved around.
-- The area which recognises the mouse should be exactly the area defined by the circle,
-- and the window should close correctly.
--
-- Glfw likes everything done on a single Thread. This experiment shows just
-- what actually is done on a single thread and what isn't.
--
-- The main loop is responsible for
-- a) updating the application constantly, reacting to the user input
--
-- It looks like Glfw really does run its own thread, so all callbacks should
-- methodologically modify MVars to report any changes to the input state to the
-- application.
--
-- The loop must be broken whenever the user chooses to exit the application, by
-- killing the window or otherwise pressing Esc.
winSize = Size 600 600

data MainLoopControl = Break | Continue

-- The states that the callback needs to be aware of are:
--  - loop: controls when the mainloop exits
--  - scene: the state of the scene at the minute
--  - dragInfo: information about dragging.
--
-- Dragging is implement by 'resetting the position of the scene object relative to
-- the mouse'. That is, we remember relative (world-space) distance from the centre of the
-- scene node to the mouse click
data Vars = Vars {
  loop  :: IORef MainLoopControl,
  scene :: IORef Scene,
  drag  :: IORef (Maybe (Int,Vec2))        -- mouse dragging
  }

defaultCircle = Circle (Vec2 0 0) 0.05
defaultSn = CircleSn defaultCircle halfRed

halfRed = Color4 1.0 0.0 0.0 0.5
red = Color4 1.0 0.0 0.0 1.0
green = Color4 0.0 1.0 0.0 1.0

randomColorIO = do
  let randGLdouble = randomIO >>= return . glflt
  r <- randGLdouble
  g <- randGLdouble
  b <- randGLdouble
  a <- randGLdouble
  let a' = a/2
  return $ Color4 r g b a'

-- Application entry point.
-- The withGlfw environment ensures that initialisation and termination of Glfw
-- is done.
-- Similarly, the withGlfwWindow call ensures that creation (opening) and
-- closing of the main window is done properly.
-- 'fail' in either case means initialisation could not be done properly.
-- We ensure that the window
main = withGlfw $ withGlfwWindow "Glfw Experiments" winSize $ do

  -- Mainloop states --
  let scene = Scene V.empty                  -- create an empty scene
  loopCtrlRef <- newIORef Continue           -- Use MVar if it can be modified elsewhere
  sceneRef <- newIORef scene                 -- record the scene in an IORef.
  dragRef <- newIORef Nothing                   -- information about dragging

  let vars = Vars loopCtrlRef sceneRef dragRef

  setupGlfwCallbacks vars                  -- Register all the callback functions
  mainLoop vars


-- Note that the setWindowCloseCallback does not respond to the closing of the
-- window programmatically i.e. via "closeWindow".
--
-- onMousePosition moves the circle that is in hold
setupGlfwCallbacks :: Vars -> IO ()
setupGlfwCallbacks refs = do
  setWindowSizeCallback onWindowResize
  setWindowCloseCallback $ writeIORef (loop refs) Break >> return True
  setKeyCallback onKeyPressed
  setMouseButtonCallback onMouseButton
  setMousePositionCallback onMousePosition
  setMouseWheelCallback onMouseWheel
  where
    onWindowResize w h  = return ()

    -- If we're currently dragging an object, we update its position according
    -- to the place we last saw the drop.
    onMousePosition x y = do
      dragInfo <- readIORef $ drag refs

      case dragInfo of
        Nothing -> return ()
        Just (idx,relMsCoord) -> do
          worldCoords <- getMousePosition >>= return . windowToWorldCoordinates

          let newCoords = worldCoords &+ relMsCoord
          -- putStr $ "onMousePos: Moving circle to " ++ show newCoords ++ "\n"

          modifyIORef (scene refs) $
            modify sceneNodes $
              \sceneNodes -> sceneNodes V.//
                [(idx, set (centre . snCircle) newCoords (sceneNodes V.! idx))]

    -- onMouseButton MouseButton0 pressed = do
    --   pos <- getMousePosition
    --   scn <- readIORef (scene refs)
    --   let worldPos = windowToWorldCoordinates pos
    --   if pressed
    --     then writeIORef (hold refs) $ indexOfObjectAt scn worldPos
    --     else writeIORef (hold refs) Nothing

    onMouseButton MouseButton0 pressed = do
      if pressed
        then beginDragObjectAtCursor
        else endDragObjectAtCursor
      where
        beginDragObjectAtCursor = do
          worldCoords <- getMousePosition >>= return . windowToWorldCoordinates
          scn <- readIORef $ scene refs
          case scn `indexOfObjectAt` worldCoords of
            Nothing -> return ()
            Just idx -> writeIORef (drag refs) $
              let snCentre = get (centre . snCircle) (get sceneNodes scn V.! idx)
              in Just (idx, snCentre &- worldCoords)

        endDragObjectAtCursor = do
          writeIORef (drag refs) Nothing

    changeSnColourAt :: Point -> StdColor -> SceneNode -> SceneNode
    changeSnColourAt pos col sn =
      if get snCircle sn `contains` pos
        then set snColour col sn
        else sn

    onMouseWheel n        = return ()

    onKeyPressed (CharKey 'C') True = do
      worldCoords <- getMousePosition >>= return . windowToWorldCoordinates
      randCol <- randomColorIO
      modifyIORef (scene refs) $
        \scn -> tryAddColoredCircleSn randCol scn (defaultCircle {_centre=worldCoords})
    onKeyPressed KeyEsc True = writeIORef (loop refs) Break
    onKeyPressed _ _ = return ()

    -- Debugging functions --
    printResizeDims w h = putStr $ "GLFW: Resizing window to ("
                                   ++ show w ++ "," ++ show h ++")\n"

    printMousePos x y = putStr $ "GLFW: mouse moved to ("
                             ++ show x ++ "," ++ show y ++")\n"

    printMouseButton msbtn pressed =
      let stat = if pressed then "pressed" else "released"
      in putStr $ "GLFW: Mouse button " ++ show msbtn ++ " was " ++ stat ++ "\n"

-- TODO check for collisions
-- Add the geometric shape of circle into the scene and colour it
tryAddColoredCircleSn :: StdColor -> Scene -> Circle -> Scene
tryAddColoredCircleSn col (Scene sceneNodes) circ = Scene sceneNodes'
  where
    sceneNodes' = sceneNodes V.++ V.singleton (CircleSn circ col)

-- The unprojection here is greatly simplified, because the projection matrix is ortho and the
-- the world coordinates correspond with the NDC in this way.
windowToWorldCoordinates :: (Int,Int) -> Point
windowToWorldCoordinates = invertWindow
  where
    Size width height = winSize
    invertWindow (xw,yw) = Vec2 ((2*xw' / width') - 1) $ negate ((2*yw' / height') - 1)
      where
        (xw',yw') = (fromIntegral xw,fromIntegral yw)
        (width',height') = (fromIntegral width,fromIntegral height)

-- Linear search all of the scene nodes for containment of the point!
indexOfObjectAt :: Scene -> Point -> Maybe Int
indexOfObjectAt (Scene sceneNodes) pt =
  V.findIndex (\(CircleSn circ _) -> circ `contains` pt) sceneNodes


-- The LoopControl variable is a carried state of the mainLoop!
-- The only thing we care is dragging.
-- 1) When the user clicks, we record the mouse position. This is the relative
--    movement's origin. We express whether we are in a drag motion or not by a
--    Maybe DragMotion data type.
-- 2) Every time we move the mouse with the button down, we calculate the relative
--    movement from the relative origin, and update the position the circle
--    accordingly. Positions are in world coordinates.
--    - The positions of the Circles are stored in a simple 'Scene' data
--      structure, which is just a list of objects for now.
--    - Checking this list for collision isn't exactly difficult. we can think
--      of efficient space partitioning later!
--    - Whenever a circle is being moved, be sure to only update that circle!
--      No other circle's positions should be updated.
-- 3) Whenever c is pressed, we create another circle where the mouse is if it
--    doesn't overlap with any other circle.
-- 4) When the user releases, the object should remain still where it is.
--
-- The main loop does the following:
--  - inspect any incoming inputs: mouse clicks and mouse movements.
--  - instantaneous update of the scene (statically insert objects)
--  - delta-tee update of the scene
--  - draw the updated scene
--  - repeat.
mainLoop :: Vars -> IO ()
mainLoop refs = do
  waitEvents                  -- 1) Calls all callbacks
                              -- 2) Callbacks are aware of the state of the scene
  scn <- readIORef (scene refs)
  -- instantaneouslyUpdateScene scn
  -- deltaTeeUpdateScene scn
  renderScene scn
  threadDelay 100                     -- Wait a millisecond
  breakOrCont <- readIORef (loop refs)
  case breakOrCont of
    Continue -> mainLoop refs
    Break -> return ()

-- Renders the scene. Enable calls are made at every loop!
renderScene :: Scene -> IO ()
renderScene (Scene sceneNodes) = do
  clear [ColorBuffer]
  blend              $= Enabled
  blendFunc          $= (SrcAlpha,OneMinusSrcAlpha)
  viewport           $= ((Position 0 0),winSize)
  matrixMode         $= Projection
  loadIdentity
  let asp = aspectRatio winSize
  ortho2D (-1.0) (1.0) (-1.0) (1.0)
  matrixMode         $= Modelview 0
  loadIdentity

  -- Begin render
  V.mapM_ renderSceneNode sceneNodes
  -- renderSceneNode (CircleSn (Circle (Vec2 0 0) 0.5) red)
  -- End render

  swapBuffers
  where
    aspectRatio :: Size -> GLdouble
    aspectRatio (Size w h)
      | h /= 0    = fromIntegral w / fromIntegral h
      | otherwise = error "Zero height"

-- This function knows how to draw any scene node!
renderSceneNode :: SceneNode -> IO ()
renderSceneNode (CircleSn (Circle (Vec2 x y) r) col) = do
  color col
  preservingMatrix $ do
    glTranslate (Vec3 x y 0)
    circle $ realToFrac r

ftrace = flip trace
