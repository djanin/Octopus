module UtilGLFW where

import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
    
import System.Exit
import System.IO
import Data.IORef
    

import System.IO.Unsafe(unsafeInterleaveIO)

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List                 (intercalate)
import Data.Maybe                (catMaybes)
import Text.PrettyPrint
    
    
import qualified Linear as L
    
simpleErrorCallback :: GLFW.Error -> String -> IO ()
simpleErrorCallback e s =
    putStrLn $ unwords [show e, show s]

-- errorCallback :: GLFW.ErrorCallback
-- errorCallback _ = hPutStrLn stderr

-- keyCallback :: GLFW.KeyCallback
-- keyCallback window key _ action _ =
--    when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
--         GLFW.setWindowShouldClose window True

initialize :: String -> IO (GLFW.Window,TQueue Event)
initialize title = do
    
--        envEventsChan    :: TQueue Event
--    , envWindow        :: !GLFW.Window

  GLFW.setErrorCallback $ Just simpleErrorCallback
  successfulInit <- GLFW.init
  -- if init failed, we exit the program
  if not successfulInit then exitFailure else do
      GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext False --True
      GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
          -- necessary for seting up macOSX opengl environment
      GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile  GLFW.OpenGLProfile'Core
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2

    
      mw <- GLFW.createWindow 640 480 title Nothing Nothing
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
                eventsChan <- newTQueueIO :: IO (TQueue Event)
  
                GLFW.makeContextCurrent mw
                GLFW.setErrorCallback                  $ Just $ errorCallback           eventsChan
                GLFW.setWindowPosCallback       window $ Just $ windowPosCallback       eventsChan
                GLFW.setWindowSizeCallback      window $ Just $ windowSizeCallback      eventsChan
                GLFW.setWindowCloseCallback     window $ Just $ windowCloseCallback     eventsChan
                GLFW.setWindowRefreshCallback   window $ Just $ windowRefreshCallback   eventsChan
                GLFW.setWindowFocusCallback     window $ Just $ windowFocusCallback     eventsChan
                GLFW.setWindowIconifyCallback   window $ Just $ windowIconifyCallback   eventsChan
                GLFW.setFramebufferSizeCallback window $ Just $ framebufferSizeCallback eventsChan
                GLFW.setMouseButtonCallback     window $ Just $ mouseButtonCallback     eventsChan
                GLFW.setCursorPosCallback       window $ Just $ cursorPosCallback       eventsChan
                GLFW.setCursorEnterCallback     window $ Just $ cursorEnterCallback     eventsChan
                GLFW.setScrollCallback          window $ Just $ scrollCallback          eventsChan
                GLFW.setKeyCallback             window $ Just $ keyCallback             eventsChan
                GLFW.setCharCallback            window $ Just $ charCallback            eventsChan

                GLFW.swapInterval 1
                    -- GLFW.setKeyCallback window (Just keyCallback)
                return (window,eventsChan)


--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

data Environment = Environment {
      envWindow        :: !GLFW.Window
    , envEventsChan    :: TQueue Event
    , envTimeStamp     :: IORef Float                
    }

initEnvironment ::  IO Environment
initEnvironment = do
  (win,chan) <- initialize "Octopus"
  timest <- newIORef 0                  
  return $ Environment win chan timest

--------------------------------------------------------------------------------

type IOState = IORef State

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateXAngle          :: !Double
    , stateYAngle          :: !Double
    , stateZAngle          :: !Double
    , stateZDist           :: !Double
    , stateZDistClosest    :: !Double
    , stateZDistFarthest   :: !Double
    , stateMouseDown       :: !Bool
    , stateDragging        :: !Bool
    , stateDragStartX      :: !Double
    , stateDragStartY      :: !Double
    , stateDragStartXAngle :: !Double
    , stateDragStartYAngle :: !Double
    -- game like state values 
    , stateSpeed           :: !GL.GLfloat
    , stateBasis           :: !(L.M33 GL.GLfloat)
    , stateOrigin          :: !(L.V3 GL.GLfloat)
    }

initState :: GLFW.Window -> IO State
initState win = do
    (fbWidth, fbHeight) <- GLFW.getFramebufferSize win
    let zDistClosest  = 0.1
        zDistFarthest = 10000 -- zDistClosest + 400
        zDist         = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
    return $ State
           { stateWindowWidth     = fbWidth
           , stateWindowHeight    = fbHeight
           , stateXAngle          = 0
           , stateYAngle          = 0
           , stateZAngle          = 0
           , stateZDist           = zDist
           , stateZDistClosest  = zDistClosest
           , stateZDistFarthest = zDistFarthest
           , stateMouseDown       = False
           , stateDragging        = False
           , stateDragStartX      = 0
           , stateDragStartY      = 0
           , stateDragStartXAngle = 0
           , stateDragStartYAngle = 0
           , stateSpeed           = 0
           , stateBasis           = L.identity
           , stateOrigin          = 0
           }

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

type EventHandler = RWST Environment () State IO

                  

processEvent :: Event -> EventHandler ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowPos _ x y) ->
          printEvent "window pos" [show x, show y]

      (EventWindowSize _ width height) ->
          printEvent "window size" [show width, show height]

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventWindowRefresh _) ->
          printEvent "window refresh" []

      (EventWindowFocus _ fs) ->
          printEvent "window focus" [show fs]

      (EventWindowIconify _ is) ->
          printEvent "window iconify" [show is]

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventMouseButton _ mb mbs mk) -> do
          printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
          when (mb == GLFW.MouseButton'1) $ do
              let pressed = mbs == GLFW.MouseButtonState'Pressed
              modify $ \s -> s
                { stateMouseDown = pressed
                }
              unless pressed $
                modify $ \s -> s
                  { stateDragging = False
                  }

      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "cursor pos" [show x', show y']
          state <- get
          when (stateMouseDown state && not (stateDragging state)) $
            put $ state
              { stateDragging        = True
              , stateDragStartX      = x
              , stateDragStartY      = y
              , stateDragStartXAngle = stateXAngle state
              , stateDragStartYAngle = stateYAngle state
              }

      (EventCursorEnter _ cs) ->
          printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']
          modify $ \s -> s
            { stateZDist =
                let zDist' = stateZDist s + realToFrac (negate $ y / 2)
                in curb (stateZDistClosest s) (stateZDistFarthest s) zDist'
            }
          adjustWindow

      (EventKey win k scancode ks mk) -> do
          printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True
              -- ?: print instructions
              when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
                liftIO printInstructions
              -- i: print GLFW information
              when (k == GLFW.Key'I) $
                liftIO $ printInformation win
      (EventChar _ c) ->
          printEvent "char" [show c]

adjustWindow :: EventHandler ()
adjustWindow = do
    state <- get
    let width  = stateWindowWidth  state
        height = stateWindowHeight state
        zDist  = stateZDist        state

    let pos   = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
        h     = fromIntegral height / fromIntegral width :: Double
        znear = 1           :: Double
        zfar  = 40          :: Double
        xmax  = znear * 0.5 :: Double
    liftIO $ do
        GL.viewport   GL.$= (pos, size)
        GL.matrixMode GL.$= GL.Projection
        GL.loadIdentity
        GL.frustum (realToFrac $ -xmax)
                   (realToFrac    xmax)
                   (realToFrac $ -xmax * realToFrac h)
                   (realToFrac $  xmax * realToFrac h)
                   (realToFrac    znear)
                   (realToFrac    zfar)
        GL.matrixMode GL.$= GL.Modelview 0
        GL.loadIdentity
        GL.translate (GL.Vector3 0 0 (negate $ realToFrac zDist) :: GL.Vector3 GL.GLfloat)
      
--------------------------------------------------------------------------------
printInstructions :: IO ()
printInstructions =
    putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "'?': Print these instructions"                                $+$
        text "'i': Print GLFW information"                                  $+$
        text ""                                                             $+$
        text "* Mouse cursor, keyboard cursor keys, and/or joystick"        $+$
        text "  control rotation."                                          $+$
        text "* Mouse scroll wheel controls distance from scene."           $+$
        text "------------------------------------------------------------"
      )

printInformation :: GLFW.Window -> IO ()
printInformation win = do
    version       <- GLFW.getVersion
    versionString <- GLFW.getVersionString
    monitorInfos  <- runMaybeT getMonitorInfos
    joystickNames <- getJoystickNames
    clientAPI     <- GLFW.getWindowClientAPI              win
    cv0           <- GLFW.getWindowContextVersionMajor    win
    cv1           <- GLFW.getWindowContextVersionMinor    win
    cv2           <- GLFW.getWindowContextVersionRevision win
    robustness    <- GLFW.getWindowContextRobustness      win
    forwardCompat <- GLFW.getWindowOpenGLForwardCompat    win
    debug         <- GLFW.getWindowOpenGLDebugContext     win
    profile       <- GLFW.getWindowOpenGLProfile          win

    putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "GLFW C library:" $+$
        nest 4 (
          text "Version:"        <+> renderVersion version $+$
          text "Version string:" <+> renderVersionString versionString
        ) $+$
        text "Monitors:" $+$
        nest 4 (
          renderMonitorInfos monitorInfos
        ) $+$
        text "Joysticks:" $+$
        nest 4 (
          renderJoystickNames joystickNames
        ) $+$
        text "OpenGL context:" $+$
        nest 4 (
          text "Client API:"            <+> renderClientAPI clientAPI $+$
          text "Version:"               <+> renderContextVersion cv0 cv1 cv2 $+$
          text "Robustness:"            <+> renderContextRobustness robustness $+$
          text "Forward compatibility:" <+> renderForwardCompat forwardCompat $+$
          text "Debug:"                 <+> renderDebug debug $+$
          text "Profile:"               <+> renderProfile profile
        ) $+$
        text "------------------------------------------------------------"
      )
  where
    renderVersion (GLFW.Version v0 v1 v2) =
        text $ intercalate "." $ map show [v0, v1, v2]

    renderVersionString =
        text . show

    renderMonitorInfos =
        maybe (text "(error)") (vcat . map renderMonitorInfo)

    renderMonitorInfo (name, (x,y), (w,h), vms) =
        text (show name) $+$
        nest 4 (
          location <+> size $+$
          fsep (map renderVideoMode vms)
        )
      where
        location = int x <> text "," <> int y
        size     = int w <> text "x" <> int h <> text "mm"

    renderVideoMode (GLFW.VideoMode w h r g b rr) =
        brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz  = int rr <> text "Hz"

    renderJoystickNames pairs =
        vcat $ map (\(js, name) -> text (show js) <+> text (show name)) pairs

    renderContextVersion v0 v1 v2 =
        hcat [int v0, text ".", int v1, text ".", int v2]

    renderClientAPI         = text . show
    renderContextRobustness = text . show
    renderForwardCompat     = text . show
    renderDebug             = text . show
    renderProfile           = text . show

type MonitorInfo = (String, (Int,Int), (Int,Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos =
    getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors

    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
        name <- getMonitorName mon
        vms  <- getVideoModes mon
        MaybeT $ do
            pos  <- liftIO $ GLFW.getMonitorPos mon
            size <- liftIO $ GLFW.getMonitorPhysicalSize mon
            return $ Just (name, pos, size, vms)

    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon

    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames :: IO [(GLFW.Joystick, String)]
getJoystickNames =
    catMaybes `fmap` mapM getJoystick joysticks
  where
    getJoystick js =
        fmap (maybe Nothing (\name -> Just (js, name)))
             (GLFW.getJoystickName js)

--------------------------------------------------------------------------------
                              
printEvent :: String -> [String] -> EventHandler ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x

--------------------------------------------------------------------------------

joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
    x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
    x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
    y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
    y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
    let x0n = if x0 then (-1) else 0
        x1n = if x1 then   1  else 0
        y0n = if y0 then (-1) else 0
        y1n = if y1 then   1  else 0
    return (x0n + x1n, y0n + y1n)

getSpeedKey :: GLFW.Window -> IO (GL.GLfloat)
getSpeedKey win = do
    x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'S
    x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'X
    let x0f = if x0 then 0.1 else 0
        x1f = if x1 then 0.1 else 0
    return (x0f-x1f)
  

getJoystickDirections :: GLFW.Joystick -> IO (Double, Double,Double)
getJoystickDirections js = do
    maxes <- GLFW.getJoystickAxes js
    return $ case maxes of
      (Just (x:y:z:_)) -> (-y, x,z)
      (Just [x,y]) -> (-y, x,0)
      _              -> ( 0, 0,0)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False
      
