--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

-- import Graphics.UI.GLUT
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
  glDepthFunc GL_LEQUAL -- type of depth test
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

resizeScene :: GLFW.WindowSizeCallback
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
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
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
     GLFW.defaultWindowHints
     -- get a 640 x 480 window
     -- initialize our window.
     Just win <- GLFW.createWindow 640 480 "Lesson 1" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback win (Just drawScene)
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just keyPressed)
     -- register window close handler
     GLFW.setWindowCloseCallback win (Just shutdown)
     initGL win
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       drawScene win
       GLFW.swapBuffers win
