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
import Control.Monad ( forever, forM_ )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import Foreign ( withForeignPtr, plusPtr, alloca, peek )
import qualified Data.ByteString.Internal as BSI
import Util ( Image(..), bitmapLoad )
import Paths_nehe_tuts

boxcol :: [(GLfloat, GLfloat, GLfloat)]
boxcol = [(1, 0, 0), (1, 0.5, 0), (1, 1, 0), 
          (0, 1, 0), (0, 1, 1)]
topcol :: [(GLfloat, GLfloat, GLfloat)]
topcol = [(0.5, 0, 0), (0.5, 0.25, 0), (0.5, 0.5, 0),
          (0, 0.5, 0), (0, 0.5, 0.5)]

buildLists :: IO (GLuint, GLuint)
buildLists = do
  box <- glGenLists 2
  glNewList box GL_COMPILE
  glBegin GL_QUADS
  glTexCoord2f 1   1   >> glVertex3f   (-1) (-1)   (-1)   -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f   1.0  (-1.0) (-1.0) -- Top Left Of The Texture and Quad
  glTexCoord2f 0.0 0.0 >> glVertex3f   1.0  (-1.0)  1.0   -- Bottom Left Of The Texture and Quad
  glTexCoord2f 1.0 0.0 >> glVertex3f (-1.0) (-1.0)  1.0   -- Bottom Right Of The Texture and Quad
  -- Front Face
  glTexCoord2f 0.0 0.0 >> glVertex3f (-1.0) (-1.0)  1.0   -- Bottom Left Of The Texture and Quad
  glTexCoord2f 1.0 0.0 >> glVertex3f   1.0  (-1.0)  1.0   -- Bottom Right Of The Texture and Quad
  glTexCoord2f 1.0 1.0 >> glVertex3f   1.0    1.0   1.0   -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f (-1.0)   1.0   1.0   -- Top Left Of The Texture and Quad
  -- Back Face
  glTexCoord2f 1.0 0.0 >> glVertex3f (-1.0) (-1.0) (-1.0) -- Bottom Right Of The Texture and Quad
  glTexCoord2f 1.0 1.0 >> glVertex3f (-1.0)   1.0  (-1.0) -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f   1.0    1.0  (-1.0) -- Top Left Of The Texture and Quad
  glTexCoord2f 0.0 0.0 >> glVertex3f   1.0  (-1.0) (-1.0) -- Bottom Left Of The Texture and Quad
  -- Right face
  glTexCoord2f 1.0 0.0 >> glVertex3f   1.0  (-1.0) (-1.0) -- Bottom Right Of The Texture and Quad
  glTexCoord2f 1.0 1.0 >> glVertex3f   1.0    1.0  (-1.0) -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f   1.0    1.0    1.0  -- Top Left Of The Texture and Quad
  glTexCoord2f 0.0 0.0 >> glVertex3f   1.0  (-1.0)   1.0  -- Bottom Left Of The Texture and Quad
  -- Left Face
  glTexCoord2f 0.0 0.0 >> glVertex3f (-1.0) (-1.0) (-1.0) -- Bottom Left Of The Texture and Quad
  glTexCoord2f 1.0 0.0 >> glVertex3f (-1.0) (-1.0)   1.0  -- Bottom Right Of The Texture and Quad
  glTexCoord2f 1.0 1.0 >> glVertex3f (-1.0)   1.0    1.0  -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f (-1.0)   1.0  (-1.0) -- Top Left Of The Texture and Quad

  glEndList
  let top = box + 1
  glNewList top GL_COMPILE
  glBegin GL_QUADS
  glTexCoord2f 0 1 >> glVertex3f (-1) 1 (-1)
  glTexCoord2f 0 0 >> glVertex3f (-1) 1   1
  glTexCoord2f 1 0 >> glVertex3f   1  1   1
  glTexCoord2f 1 1 >> glVertex3f   1  1 (-1)
  glEnd
  glEndList
  return (box, top)

initGL :: GLFW.Window -> IO GLuint
initGL win = do
  tex <- loadTextures
  glEnable GL_TEXTURE_2D
  glShadeModel GL_SMOOTH
  glClearColor 0 0 0 0.5
  glClearDepth 1
  glEnable GL_DEPTH_TEST
  glEnable GL_LEQUAL
  glEnable GL_LIGHT0
  glEnable GL_LIGHTING
  glEnable GL_COLOR_MATERIAL
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h
  return tex

loadTextures :: IO GLuint
loadTextures = do
  fp <- getDataFileName "cube.bmp"
  Just (Image w h pd) <- bitmapLoad fp
  putStrLn $ "Image w = " ++ show w
  putStrLn $ "Image h = " ++ show h
  tex <- alloca $ \p -> do
            glGenTextures 1 p
            peek p
  let (ptr, off, _) = BSI.toForeignPtr pd
  _ <- withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
        glNearest  = fromIntegral GL_NEAREST
    -- create linear filtered texture
    glBindTexture GL_TEXTURE_2D tex
    glTexImage2D GL_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 GL_RGB GL_UNSIGNED_BYTE p'
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER glNearest
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER glNearest
  return tex

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

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

drawScene :: GLuint -> IORef GLfloat -> IORef GLfloat
          -> GLuint -> GLuint -> GLFW.Window -> IO ()
drawScene tex xrot yrot box top _ = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT
  glBindTexture GL_TEXTURE_2D tex

  xr <- readIORef xrot
  yr <- readIORef yrot

  forM_ [(x,y) | y <- [1..5], x <- [0..y-1] ] $ \(x,y) -> do
    glLoadIdentity
    let x' = fromIntegral x
        y' = fromIntegral y
        color (r,g,b) = glColor3f r g b
    glTranslatef (1.4+x'*2.8-y'*1.4) (((6-y')*2.4)-7)  (-20)
    glRotatef (45.0-(2.0*y')+xr) 1 0 0
    glRotatef (45-yr) 0 1 0
    color (boxcol !! (y-1))
    glCallList box
    color (topcol !! (y-1))
    glCallList top 
  glFlush

keyPressed :: IORef GLfloat -> IORef GLfloat -> GLFW.KeyCallback
keyPressed _    _    win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed xrot _    _   GLFW.Key'Up     _ GLFW.KeyState'Pressed _ = modifyIORef xrot (subtract 0.8)
keyPressed xrot _    _   GLFW.Key'Down   _ GLFW.KeyState'Pressed _ = modifyIORef xrot (+0.8)
keyPressed _    yrot _   GLFW.Key'Left   _ GLFW.KeyState'Pressed _ = modifyIORef yrot (subtract 0.8)
keyPressed _    yrot _   GLFW.Key'Right  _ GLFW.KeyState'Pressed _ = modifyIORef yrot (+0.8)
keyPressed _    _    _   _               _ _                     _ = return ()

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
     Just win <- GLFW.createWindow 800 600 "Lesson 12" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     xrot <- newIORef 0
     yrot <- newIORef 0
     
     -- initialize our window.
     tex <- initGL win
     (box, top) <- buildLists
     GLFW.setWindowRefreshCallback win $
       Just (drawScene tex xrot yrot box top)
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win $
       Just (keyPressed xrot yrot)
     GLFW.setWindowCloseCallback win (Just shutdown)
     -- GLFW.getWindowRefreshRate >>= print
     forever $ do
       GLFW.pollEvents
       drawScene tex xrot yrot box top win
       GLFW.swapBuffers win
