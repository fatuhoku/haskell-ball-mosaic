module Glfw (withGlfw,withGlfwWindow) where 

import Graphics.Rendering.OpenGL (Size(..))
import Graphics.UI.GLFW

withGlfw :: IO () -> IO ()
withGlfw action = do
  initSuccess <- initialize 
  if initSuccess
    then return ()
    else fail "createWindow: failed initialising GLFW"
  action
  terminate

-- | withGlfwWindow call is intended to be used inside the withGlfw call.
withGlfwWindow :: String -> Size -> IO () -> IO ()
withGlfwWindow title (Size w h) action = do
  windSuccess <- openWindow $ defaultDisplayOptions
                                { displayOptions_width  = fromIntegral w
                                , displayOptions_height = fromIntegral h
                                }
  if windSuccess
    then return ()
    else fail "createWindow: failed creating GLFW window"
  setWindowTitle title
  -- setWindowPosition 200 200
  action
  closeWindow 
