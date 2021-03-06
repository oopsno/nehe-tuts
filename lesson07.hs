--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.GL
import Graphics.GLU ( gluPerspective, gluBuild2DMipmaps )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Foreign ( withForeignPtr, plusPtr
               , ForeignPtr, newForeignPtr_ )
import Foreign.Storable ( Storable )
import Foreign.Marshal.Array ( newArray, allocaArray, peekArray )
import qualified Data.ByteString.Internal as BSI
import Util ( Image(..), bitmapLoad )
import Paths_nehe_tuts

newArray' :: Storable a => [a] -> IO (ForeignPtr a)
newArray' xs = (newArray xs) >>= newForeignPtr_

glLightfv' :: GLenum -> GLenum -> ForeignPtr GLfloat -> IO ()
glLightfv' l a fp =
  withForeignPtr fp $ glLightfv l a

initGL :: GLFW.Window -> IO [GLuint]
initGL win = do
  glEnable GL_TEXTURE_2D
  glShadeModel GL_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST
  lightAmbient  <- newArray' [0.5, 0.5, 0.5, 1.0] 
  lightDiffuse  <- newArray' [1.0, 1.0, 1.0, 1.0]
  lightPosition <- newArray' [0.0, 0.0, 2.0, 1.0]
  glLightfv' GL_LIGHT1 GL_AMBIENT  lightAmbient
  glLightfv' GL_LIGHT1 GL_DIFFUSE  lightDiffuse
  glLightfv' GL_LIGHT1 GL_POSITION lightPosition
  glEnable GL_LIGHT1
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h
  loadGLTextures

loadGLTextures :: IO [GLuint]
loadGLTextures = do
  fp <- getDataFileName "Crate.bmp"
  Just (Image w h pd) <- bitmapLoad fp
  let numTextures = 3
  texs <- allocaArray numTextures $ \p -> do
            glGenTextures (fromIntegral numTextures) p
            peekArray numTextures p
  let (ptr, off, _) = BSI.toForeignPtr pd
  _ <- withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
        glNearest = fromIntegral GL_NEAREST
        glLinear  = fromIntegral GL_LINEAR
    -- create nearest filtered texture
    glBindTexture GL_TEXTURE_2D (texs!!0)
    glTexImage2D GL_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 GL_RGB GL_UNSIGNED_BYTE p'
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER glNearest
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER glNearest
    -- create linear filtered texture
    glBindTexture GL_TEXTURE_2D (texs!!1)
    glTexImage2D GL_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 GL_RGB GL_UNSIGNED_BYTE p'
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER glLinear
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER glLinear
    -- create mipmap filtered texture
    glBindTexture GL_TEXTURE_2D (texs!!2)
    glTexImage2D GL_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 GL_RGB GL_UNSIGNED_BYTE p'
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER glLinear 
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER
      (fromIntegral GL_LINEAR_MIPMAP_NEAREST)
    gluBuild2DMipmaps GL_TEXTURE_2D 3 (fromIntegral w)
      (fromIntegral h) GL_RGB GL_UNSIGNED_BYTE p'
  return texs

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

drawScene :: [GLuint] -> IORef GLfloat -> IORef GLfloat
          -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat 
          -> IORef Int -> GLFW.Window -> IO ()
drawScene texs xrot yrot xspeed yspeed zdepth filt _ = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view

  glTranslatef 0 0 (-5.0) --Move left 5 Units into the screen

  zd <- readIORef zdepth
  glTranslatef 0 0 zd --Move left 5 Units into the screen

  xr <- readIORef xrot
  yr <- readIORef yrot
  glRotatef xr 1 0 0 -- Rotate the triangle on the Y axis
  glRotatef yr 0 1 0 -- Rotate the triangle on the Y axis
  f <- readIORef filt
  glBindTexture GL_TEXTURE_2D (texs!!f)
  
  glBegin GL_QUADS -- start drawing a polygon (4 sided)
  -- first the front
  glNormal3f     0    0    1
  glTexCoord2f   0    0 
  glVertex3f   (-1) (-1)   1  -- bottom left of quad (Front)
  glTexCoord2f   1    0 
  glVertex3f     1  (-1)   1  -- bottom right of quad (Front)
  glTexCoord2f   1    1 
  glVertex3f     1    1    1  -- top right of quad (Front)
  glTexCoord2f   0    1 
  glVertex3f   (-1)   1    1  -- top left of quad (Front)
  -- now the back
  glNormal3f     0    0  (-1)
  glTexCoord2f   1    0 
  glVertex3f   (-1) (-1) (-1) -- bottom right of quad (Back)
  glTexCoord2f   1    1 
  glVertex3f   (-1)   1  (-1) -- top right of quad (Back)
  glTexCoord2f   0    1 
  glVertex3f     1    1  (-1) -- top left of quad (Back)
  glTexCoord2f   0    0 
  glVertex3f     1  (-1) (-1) -- bottom left of quad (Back)
  -- now the top
  glNormal3f     0    1    0
  glTexCoord2f   0    1
  glVertex3f   (-1)   1  (-1) -- top left of quad (Top)
  glTexCoord2f   0    0  
  glVertex3f   (-1)   1    1  -- bottom left of quad (Top)
  glTexCoord2f   1    0  
  glVertex3f     1    1    1  -- bottom right of quad (Top)
  glTexCoord2f   1    1  
  glVertex3f     1    1  (-1) -- top right of quad (Top)
  -- now the bottom
  glNormal3f     0  (-1)   0
  glTexCoord2f   1    1  
  glVertex3f     1  (-1)   1  -- top right of quad (Bottom)
  glTexCoord2f   0    1  
  glVertex3f   (-1) (-1)   1  -- top left of quad (Bottom)
  glTexCoord2f   0    0  
  glVertex3f   (-1) (-1) (-1) -- bottom left of quad (Bottom)
  glTexCoord2f   1    0  
  glVertex3f     1  (-1) (-1) -- bottom right of quad (Bottom)
  -- now the right
  glNormal3f     1    0    0
  glTexCoord2f   1    0  
  glVertex3f     1  (-1) (-1) -- bottom right of quad (Right)
  glTexCoord2f   1    1  
  glVertex3f     1    1  (-1) -- top right of quad (Right)
  glTexCoord2f   0    1  
  glVertex3f     1    1    1  -- top left of quad (Right)
  glTexCoord2f   0    0
  glVertex3f     1  (-1)   1  -- bottom left of quad (Right)
  -- now the left
  glNormal3f   (-1)   0    1
  glTexCoord2f   0    0  
  glVertex3f   (-1) (-1) (-1) -- bottom left of quad (Left)
  glTexCoord2f   1    0  
  glVertex3f   (-1)   1  (-1) -- top left of quad (Left)
  glTexCoord2f   1    1  
  glVertex3f   (-1)   1    1  -- top right of quad (Left)
  glTexCoord2f   0    1  
  glVertex3f   (-1) (-1)   1  -- bottom right of quad (Left)

  glEnd

  xsp <- readIORef xspeed
  ysp <- readIORef yspeed  
  writeIORef xrot $! xr + xsp
  writeIORef yrot $! yr + ysp
  
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: IORef Bool -> IORef Int -> IORef GLfloat
           -> IORef GLfloat -> IORef GLfloat -> GLFW.KeyCallback
keyPressed _ _ _ _ _ win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed l _ _ _ _ _   GLFW.Key'L  _ GLFW.KeyState'Pressed _ = do
  le <- readIORef l
  if le == True
    then glEnable  GL_LIGHTING
    else glDisable GL_LIGHTING
  writeIORef l $! not le
keyPressed _ filt _ _ _ _ GLFW.Key'F _ GLFW.KeyState'Pressed _ = do
  f <- readIORef filt
  writeIORef filt $! (f + 1) `mod` 3
keyPressed _ _ zdepth _ _ _ GLFW.Key'PageUp _ GLFW.KeyState'Pressed _ = do
  zd <- readIORef zdepth
  writeIORef zdepth $! zd - 0.2
keyPressed _ _ zdepth _ _ _ GLFW.Key'PageDown _ GLFW.KeyState'Pressed _ = do
  zd <- readIORef zdepth
  writeIORef zdepth $! zd + 0.2
keyPressed _ _ _ xspeed _ _ GLFW.Key'Up _ GLFW.KeyState'Pressed _ = do
  xs <- readIORef xspeed
  writeIORef xspeed $! xs - 0.1
keyPressed _ _ _ xspeed _ _ GLFW.Key'Down _ GLFW.KeyState'Pressed _ = do
  xs <- readIORef xspeed
  writeIORef xspeed $! xs + 0.1
keyPressed _ _ _ _ yspeed _ GLFW.Key'Right _ GLFW.KeyState'Pressed _ = do
  xs <- readIORef yspeed
  writeIORef yspeed $! xs + 0.1
keyPressed _ _ _ _ yspeed _ GLFW.Key'Left _ GLFW.KeyState'Pressed _ = do
  ys <- readIORef yspeed
  writeIORef yspeed $! ys - 0.1
keyPressed _ _ _ _ _ _ _ _ _ _ = return ()

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
     Just win <- GLFW.createWindow 800 600 "Lesson 7" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     lighting <- newIORef True
     xrot     <- newIORef (0::GLfloat)
     yrot     <- newIORef (0::GLfloat)
     xspeed   <- newIORef (0::GLfloat)
     yspeed   <- newIORef (0::GLfloat)
     zdepth   <- newIORef (-5.0 :: GLfloat)
     filt     <- newIORef (0::Int)
     -- initialize our window.
     texs <- initGL win
     GLFW.setWindowRefreshCallback win $
       Just (drawScene texs xrot yrot xspeed yspeed zdepth filt)
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just (keyPressed lighting filt zdepth xspeed yspeed))
     GLFW.setWindowCloseCallback win (Just shutdown)
     forever $ do
       GLFW.pollEvents
       drawScene texs xrot yrot xspeed yspeed zdepth filt win
       GLFW.swapBuffers win
