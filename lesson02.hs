--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.GL
import Graphics.GLU ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )

initGL :: GLFW.Window -> IO ()
initGL win = do
  glShadeModel GL_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL  -- type of depth test
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

resizeScene :: GLFW.FramebufferSizeCallback
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
  glFlush

drawScene :: GLFW.Window -> IO ()
drawScene _ = do
  -- clear the screen and the depth bufer
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view

  glTranslatef (-1.5) 0 (-6.0) --Move left 1.5 Units and into the screen 6.0
  
  -- draw a triangle
  glBegin GL_TRIANGLES
  glVertex3f 0      1  0 -- top
  glVertex3f 1    (-1) 0 -- bottom right
  glVertex3f (-1) (-1) 0 -- bottom left
  glEnd

  glTranslatef 3 0 0  -- move right three units

  glBegin GL_QUADS
  glVertex3f (-1)   1  0 -- top left
  glVertex3f   1    1  0 -- top right
  glVertex3f   1  (-1) 0 -- bottom right
  glVertex3f (-1) (-1) 0 -- bottom left
  glEnd
  
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

main :: IO ()
main = do
     True <- GLFW.init
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     GLFW.defaultWindowHints
     -- open a window
     Just win <- GLFW.createWindow 800 600 "Lesson 2" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback win (Just drawScene)
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just keyPressed)
     GLFW.setWindowCloseCallback win (Just shutdown)
     -- initialize our window.
     initGL win
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       drawScene win
       GLFW.swapBuffers win
