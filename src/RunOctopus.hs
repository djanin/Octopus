{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
 
module RunOctopus where

-- General Haskell modules
import           Control.Applicative
import Foreign.Storable (Storable,sizeOf)
import Foreign.Ptr (nullPtr)
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.RWS.Strict  (RWST, ask, asks, runRWST, get, liftIO, modify, put)


import Halive.Utils

-- Import all OpenGL libraries qualified, for pedagogical reasons
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GL.Functions
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Linear as L
import qualified Data.Vector.Storable as V
import  Data.Monoid
import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)

import System.Exit
import System.IO

    
    
import LoadShader
    
import Data.Int    

import Graphics.GL.Functions (glDrawArrays, glDrawElements)
import Graphics.GL.Tokens
    
-- Local modules
import qualified UtilGLFW as W
import UtilGLFW (IOState,Event(..),EventHandler(..),Environment(..),initEnvironment, State(..), initState,
                showModifierKeys, processEvent,
                getJoystickDirections,getCursorKeyDirections, getSpeedKey)
import qualified UtilJuicy as T

import PMVector as PMV
import qualified Data.Vector.Storable.Mutable as VS
import Octopus
import qualified TemporalOctopus as TO
import qualified Affine3D
import Affine3D (A3D(..))
import Data.IORef
import  GHC.Float
    
                                        
runOctopus :: TO.TemporalOctopus Float -> IO ()
runOctopus (TO.TemporalOctopus _ s) = do
  -- TODO : the next two lines must not be inverted (seg. fault otherwise) but no explicit dependency prevents it.
  -- This is BAAAAD !
  env <- reacquire 0 initEnvironment
  res <- reacquire 1 initResources
  -- end TODO
  iostate <-  reacquire 2 . newIORef =<< initState (envWindow env) 
  -- iostate <-  newIORef =<< initState (envWindow env) 
  scene <- emptyScene
  mainLoop (draw res scene s) env iostate
  cleanup env

cleanup :: Environment -> IO ()
cleanup env = do
    GLFW.destroyWindow (envWindow env)
    GLFW.terminate
    exitSuccess

mainLoop :: (Environment -> State -> IO ())  -> Environment -> IOState  -> IO ()
mainLoop drawAct env iostate = do
    close <- GLFW.windowShouldClose (envWindow env)
    state <- readIORef iostate
    unless close
       $ do
         drawAct env state
         GLFW.swapBuffers (envWindow env)
         GL.flush  -- someone indirectly recommended it
                   -- and it eventually goes almost twice faster !!
         GLFW.pollEvents

         ((),nstate,()) <- runRWST processEvents  env state 
         writeIORef iostate nstate
         mainLoop drawAct env iostate

processEvents :: EventHandler ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()
    processTime
    processDevices

processTime ::  EventHandler ()
processTime = do
  env   <- ask
  state <- get
  -- Time handling
  t' <- liftIO $ maybe 0 id <$> GLFW.getTime 
  let t = double2Float t'
  t0 <- liftIO $ readIORef (envTimeStamp env)
  liftIO $  writeIORef (envTimeStamp env) t
  -- print refresh rate in fps
  case (round (t0) < round (t)) of
    True -> do
      liftIO $ print (round $ 1/(t-t0))
    False -> return()

  -- Movement
  let delta = (stateSpeed state*(t-t0))
  put $ state
            {
              stateOrigin = stateOrigin state
                              + (L.V3 0 0 delta)
            }

  -- liftIO $ print $ stateOrigin state         
                

processDevices ::  EventHandler ()            
processDevices = do
  env   <- ask
  
  -- Speed update
  state <- get
  f <- liftIO $ getSpeedKey (envWindow env)
  put $ state    
          { stateSpeed = f + stateSpeed state
            }
--  liftIO $ print  $ stateSpeed state      
  -- Mouse dragging
  state <- get
  if stateDragging state
  then do
    let sodx  = stateDragStartX      state
        sody  = stateDragStartY      state
        sodxa = stateDragStartXAngle state
        sodya = stateDragStartYAngle state
    (x, y) <- liftIO $ GLFW.getCursorPos (envWindow env)
    let myrot = (x - sodx) / 2
        mxrot = (y - sody) / 2
    put $ state
            { stateXAngle = sodxa + mxrot
            , stateYAngle = sodya + myrot
            }
    -- liftIO $ print (sodxa + mxrot,sodya + myrot)

  -- Joystick or arrow keys
  else do
    (kxrot, kyrot) <- liftIO $ getCursorKeyDirections (envWindow env)
    (jxrot, jyrot, jzrot) <- liftIO $ getJoystickDirections GLFW.Joystick'1
    let m1 =  L.fromQuaternion $ L.axisAngle  (L.V3 (-1) 0 0)  (realToFrac $  (kxrot - 2 * jxrot)/100)
        m2 =  L.fromQuaternion $ L.axisAngle  (L.V3 0 0 1)  (realToFrac $  (kyrot + 2 * jyrot)/100)
        m3 =  L.fromQuaternion $ L.axisAngle  (L.V3 0 1 0)  (realToFrac $  (jzrot)/100)
             
    put $ state
       {
         stateBasis =   (m1 L.!*! m2 L.!*! m3) L.!*! stateBasis state
       , stateOrigin =  (m1 L.!*! m2 L.!*! m3) L.!* stateOrigin state
                       
       }
                 
--------------------------------------------------------------------------------

transformM :: State -> Int -> Int -> (L.M44 GL.GLfloat,L.M44 GL.GLfloat,L.M44 GL.GLfloat)
              
transformM state width height = (projection,  view, model) where
  -- angle      = realToFrac t * pi
  -- anim       = L.mkTransformation (L.axisAngle (L.V3 0 0 0) angle) L.zero
  -- model      = L.mkTransformationMat L.identity $ L.V3 0 0 (-4)
  model      = L.mkTransformationMat (stateBasis state) (stateOrigin state)
  view       = U.camMatrix cam
  -- cam        = U.tilt (-30) . U.dolly (L.V3 0 2 0) $ U.fpsCamera
  cam        = U.tiltRad (realToFrac $ stateXAngle state/100) .
               U.rollRad (realToFrac $ stateZAngle state/100) .
               U.panRad (realToFrac $ stateYAngle state/100).
               id $ U.fpsCamera
  projection = U.projectionMatrix (pi/4) aspect
               (realToFrac $ stateZDistClosest state)
               (realToFrac $ stateZDistFarthest state)  -- last args: near far 
  aspect     = fromIntegral width / fromIntegral height

--------------------------------------------------------------------------------


data Resources = Resources {
    -- main program environment
      mainProgram :: U.ShaderProgram
    , mainVao :: GL.VertexArrayObject                 
    , positionBuffer :: GL.BufferObject
    , penShapeBuffer :: GL.BufferObject
    , adjacencyBuffer :: GL.BufferObject
    -- , textures :: [GL.TextureObject]
    , cubeMapTexture :: GL.TextureObject
                        
    -- skybox program environment
    , skyBoxProgram :: U.ShaderProgram
    , skyVao :: GL.VertexArrayObject                 
    , skyMapTexture :: GL.TextureObject
    , skyPosition :: GL.BufferObject
    }
               
initResources ::  IO Resources
initResources = do
    -- Antialising spec
    GL.multisample $= GL.Enabled
    GLFW.windowHint $ GLFW.WindowHint'Samples 16
    GL.cullFace $= Just GL.Front
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    let -- to be used in final version ... easier debug with execution time loading 
        -- shaderList = [(GL.VertexShader, $(embedFile "src/Shaders/shader.v.glsl"))
        --              ,(GL.GeometryShader, $(embedFile "src/Shaders/shader.g.glsl"))
        --              ,(GL.FragmentShader, $(embedFile "src/Shaders/shader.f.glsl"))]
        shaderList = [(GL.VertexShader,("src/Shaders/shader.v.glsl"))
                      ,(GL.GeometryShader, ("src/Shaders/shader.g.glsl"))
                      ,(GL.FragmentShader, ("src/Shaders/shader.f.glsl"))]
        shaderSkyboxList = [(GL.VertexShader,("src/Shaders/shader.skybox.v.glsl"))
                      ,(GL.FragmentShader,("src/Shaders/shader.skybox.f.glsl"))]
        -- textureList = ["src/Shaders/hello.tga"]
        cubeMapList = ["src/Shaders/cubeMap/ft.tga",
                       "src/Shaders/cubeMap/bk.tga",
                       "src/Shaders/cubeMap/up.tga",
                       "src/Shaders/cubeMap/dn.tga",
                       "src/Shaders/cubeMap/rt.tga",
                       "src/Shaders/cubeMap/lf.tga"]
    -- debug
    () <- loadAndTestShader -- useless for computation but great help for glsl compiler error !
    () <- loadAndTestSkyShader  -- useless for computation but great help for glsl compiler error !
    -- end debug

    -- main shaders program 
    mvao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just mvao
    mpos <- U.fromSource GL.ArrayBuffer ([]::[L.M43 GL.GLfloat])
    mshap <- U.fromSource GL.ArrayBuffer ([]::[L.M34 GL.GLfloat])
    madj <-  U.fromSource GL.ElementArrayBuffer ([]::[L.V3 GL.GLuint])
    -- mtex <-  mapM T.load2DTexture textureList
    mcube <- T.loadCubeMap cubeMapList
             
    -- skybox shaders program 
    svao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just svao
    scube <- T.loadCubeMap cubeMapList
    spos <- U.fromSource GL.ArrayBuffer  ([]::[L.V3 GL.GLfloat])

    mprog <- U.loadShaderProgram shaderList
             -- U.loadShaderProgramWithBS shaderList (const $ return ()) -- in final version
    sprog <- U.loadShaderProgram shaderSkyboxList        
    
                
    bindRessource $ Resources
               mprog mvao mpos mshap madj mcube
               sprog svao scube spos

loadAndTestShader :: IO()
loadAndTestShader = do
    let shaderList =
            [ShaderInfo GL.VertexShader (FileSource "src/Shaders/shader.v.glsl")
            ,ShaderInfo GL.GeometryShader (FileSource "src/Shaders/shader.g.glsl")
            ,ShaderInfo GL.FragmentShader (FileSource "src/Shaders/shader.f.glsl")]
    prog <- loadShaders shaderList
    return $ seq prog ()

           
loadAndTestSkyShader :: IO()
loadAndTestSkyShader = do
    let skyShaderList =
            [ShaderInfo GL.VertexShader (FileSource "src/Shaders/shader.skybox.v.glsl")
            ,ShaderInfo GL.FragmentShader (FileSource "src/Shaders/shader.skybox.f.glsl")]
    prog <- loadShaders skyShaderList
    return $ seq prog ()
           
                     
                     
--------------------------------------------------------------------------------

bindRessource :: Resources -> IO Resources
bindRessource r = do
    -- Bind resources to their respective shader programs and their variables

    -- MAIN PROGRAM
    GL.bindVertexArrayObject $= Just (mainVao r)
    GL.currentProgram $= (Just . U.program . mainProgram $ r)
    
    let (U.ShaderProgram a u p) = mainProgram r

    -- debug
    print a
    print u
    print p
    -- end debug
    
    -- pen position attributes
    U.enableAttrib (mainProgram r) "m1"  
    U.enableAttrib (mainProgram r) "m2"
    U.enableAttrib (mainProgram r) "m3"
    U.enableAttrib (mainProgram r) "pen"
    GL.bindBuffer GL.ArrayBuffer $= Just (positionBuffer r)
    U.setAttrib (mainProgram r) "m1"
      GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float ((9+3)*4) (U.offsetPtr 0)
    U.setAttrib (mainProgram r) "m2"
      GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float ((9+3)*4) (U.offsetPtr $ 3*4)
    U.setAttrib (mainProgram r) "m3"
      GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float ((9+3)*4) (U.offsetPtr $ 6*4)
      -- Shift
    U.setAttrib (mainProgram r) "pen"
      GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float ((9+3)*4) (U.offsetPtr $ 9*4)
     
    -- pen shape attributes
    U.enableAttrib (mainProgram r) "info"  
    U.enableAttrib (mainProgram r) "c1"
    U.enableAttrib (mainProgram r) "c2"   
    GL.bindBuffer GL.ArrayBuffer $= Just (penShapeBuffer r)
    U.setAttrib (mainProgram r) "info"
      GL.ToFloat $ GL.VertexArrayDescriptor 4 GL.Float ((9+3)*4) (U.offsetPtr 0)
    U.setAttrib (mainProgram r) "c1"
      GL.ToFloat $ GL.VertexArrayDescriptor 4 GL.Float ((9+3)*4) (U.offsetPtr $ 4*4)
    U.setAttrib (mainProgram r) "c2"
      GL.ToFloat $ GL.VertexArrayDescriptor 4 GL.Float ((9+3)*4) (U.offsetPtr $ 8*4)
        
    -- Init and load texture     
    -- texture setup
    {-    
    GL.activeTexture $= GL.TextureUnit 1
    let (t1:_) = textures r
    GL.textureBinding GL.Texture2D $= Just t1
    let tu1= U.getUniform (mainProgram r) "myTexture"
    GL.uniform tu1 $= GL.Index1 (1::GL.GLint)
    -}  
    -- cubemap setup  
    GL.activeTexture $= GL.TextureUnit 0
    let cm = cubeMapTexture r
    GL.textureBinding GL.TextureCubeMap $= Just cm

    let cmu= U.getUniform (mainProgram r) "cubemap"
    GL.uniform cmu $= GL.Index1 (0::GL.GLint)

    -- SKYBOX PROGRAM

    GL.bindVertexArrayObject $= Just (skyVao r)
    GL.currentProgram $= (Just . U.program . skyBoxProgram $ r)

    GL.activeTexture $= GL.TextureUnit 0
      -- the "cubemap" of mainProgram is the "skymap" of skyBoxProgram
    let cmu= U.getUniform (skyBoxProgram r) "skymap"
    GL.uniform cmu $= GL.Index1 (0::GL.GLint) 
  
    let (U.ShaderProgram a u p) = skyBoxProgram r
    -- debug
    print a
    print u
    print p
    -- end debug
      
    --- skybox position
    U.enableAttrib (skyBoxProgram r) "skypos"  
    GL.bindBuffer GL.ArrayBuffer $= Just (skyPosition r)
    U.setAttrib (skyBoxProgram r) "skypos"
      GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float (3*4) (U.offsetPtr 0)
     
    return r

skyTetra :: GL.GLfloat -> L.V3 GL.GLfloat -> [L.V3 GL.GLfloat]
        -- a big tetraedron drawable by triangle strip
skyTetra w (L.V3 x y z) =
    let x1 = L.V3 (x+w) (y+w) (z+w)
        x2 = L.V3 (x-w) (y+w) (z-w)
        x3 = L.V3 (x-w) (y-w) (z+w)
        x4 = L.V3 (x+w) (y-w) (z-w)
    in [x2, x1, x3, x4, x2, x1]

--------------------------------------------------------------------------------

draw ::  Resources -> Scene Float -> (Float -> Move Float) -> Environment -> State -> IO ()
draw res scene f env state = do
    let win = envWindow env            
                  
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.depthFunc $= Just GL.Less
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    -- GL.cullFace $= Just GL.Front
  
    -- Resizing the viewport?
    (width, height) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
      
    let (p,v,m) = transformM state width height
        mvp = p L.!*! v L.!*! m
        vp = p L.!*! v       
 
    -- draw main scene
    GL.currentProgram $= (Just . U.program . mainProgram $ res)
    GL.bindVertexArrayObject $= Just (mainVao res)
    GL.activeTexture $= GL.TextureUnit 0
      
    t <- liftIO $ readIORef (envTimeStamp env)

    
    newScene <- computeNextScene (f t) scene
    updateResourcesWithScene res newScene
                             
    U.asUniform mvp $ U.getUniform (mainProgram res) "mvp"
    nt <- sizePMV (adjacencyArray newScene)
    GL.bindBuffer GL.ElementArrayBuffer $= Just (adjacencyBuffer res)
    glDrawElements GL_LINE_STRIP_ADJACENCY (fromIntegral $ nt) GL_UNSIGNED_INT nullPtr

   -- draw the skybox
    GL.currentProgram $= (Just . U.program . skyBoxProgram $ res)
    GL.bindVertexArrayObject $= Just (skyVao res)
--    GL.activeTexture $= GL.TextureUnit 1
    GL.activeTexture $= GL.TextureUnit 0
    GL.depthFunc $= Just GL.Lequal
    GL.cullFace $= Just GL.Back  
    
    let ml = L.mkTransformationMat (stateBasis state) 0
    U.asUniform (vp L.!*!ml)  $ U.getUniform (skyBoxProgram res) "mvp"
    GL.bindBuffer GL.ArrayBuffer $= Just (skyPosition res)
          
    -- load the skybox
    let points = skyTetra 1000  0 -- fmap (stateBasis state L.!*) $ 
        len = length points         
        numBytes = fromIntegral $ len * sizeOf (undefined::L.V3 GL.GLfloat)
        v = V.fromList points
    V.unsafeWith v $ \ptr ->
                  GL.bufferData  GL.ArrayBuffer $= (numBytes, ptr, GL.DynamicDraw) 
    GL.drawArrays GL.TriangleStrip 0  (fromIntegral $ len)
    GL.depthFunc $= Just GL.Less
    GL.cullFace $= Just GL.Front
                  
--------------------------------------------------------------------------------
                    
updateResourcesWithScene :: Resources -> Scene Float -> IO ()
updateResourcesWithScene r scene =
  do updatePosition r (penPositionArray scene)
     updatePenShape r (penShapeArray scene)
     updateElements r (adjacencyArray scene)
     -- printSceneInfo scene
     

_updateThing :: forall a t. Storable a =>
                     GL.BufferTarget
                     -> t
                     -> (t -> GL.BufferObject)
                     -> PMV.PMV a
                     -> IO ()
_updateThing target r f v = do
  GL.bindBuffer target $= Just (f r)
  len <- sizePMV v
  let numBytes = fromIntegral $ len * sizeOf (undefined::a)
  PMV.unsafeWith v $ \ptr ->
    GL.bufferData target $= (numBytes, ptr, GL.DynamicDraw)
      
updatePosition :: Resources -> PMV.PMV (Affine3D.Affine3D Float) -> IO ()
updatePosition r v = _updateThing GL.ArrayBuffer r positionBuffer v
                     
updatePenShape :: Resources -> PMV.PMV (PenShape Float) -> IO ()
updatePenShape r v = _updateThing GL.ArrayBuffer r penShapeBuffer v

updateElements :: Resources -> PMV.PMV (GL.GLuint) -> IO ()
updateElements r v = _updateThing GL.ElementArrayBuffer r adjacencyBuffer v
               

               
                        
      
