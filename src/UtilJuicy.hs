{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, 
             ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module UtilJuicy where

import Graphics.Rendering.OpenGL as GL
import System.Exit (exitWith, ExitCode(..))
    
import Codec.Picture

import Data.Vector.Storable ((!),unsafeWith)

load2DTexture :: FilePath -> IO TextureObject
load2DTexture filename = do
  -- tex <- loadTexture $ texInfo width height TexRGB dat
  [tex] <- genObjectNames 1
  textureBinding Texture2D $= Just tex
  loadImage2DData   filename Texture2D              
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
  return tex
                                        

loadImage2DData fileName faceType =
    let sTexImage2D w h t1 t2 p =
            texImage2D t1 NoProxy 0 RGBA'
               (TextureSize2D (fromIntegral w) (fromIntegral h)) 0  
               (PixelData RGB t2 p)
               
    in do
      image <- readImage fileName
      case image of 
        (Left s) ->
            do
              print s
              exitWith (ExitFailure 1)
        (Right d) ->
            case d of
              (ImageRGB8 (Image width height dat)) ->
                  do
                    unsafeWith dat $ sTexImage2D width height faceType
                                   GL.UnsignedByte
              _ ->
                  do
                    print "Wrong image type: RGB8 format needed"
                    exitWith (ExitFailure 1)

                                  

    

loadCubeMap :: [FilePath] -> IO TextureObject
loadCubeMap l@[_,_,_,_,_,_] =
    let faceType = [TextureCubeMapPositiveX,
                    TextureCubeMapNegativeX,
                    TextureCubeMapPositiveY,
                    TextureCubeMapNegativeY,
                    TextureCubeMapPositiveZ,
                    TextureCubeMapNegativeZ]
        loadFace i = do
          loadImage2DData (l!!i) (faceType!!i)
          textureFilter   TextureCubeMap   $= ((Linear', Nothing), Linear')
          textureWrapMode TextureCubeMap S $= (Mirrored, ClampToEdge)
          textureWrapMode TextureCubeMap T $= (Mirrored, ClampToEdge)
          textureWrapMode TextureCubeMap R $= (Mirrored, ClampToEdge)                          
    in do
      tex <- genObjectName
      textureBinding TextureCubeMap $= Just tex
      mapM_ loadFace [0..5]
      return tex
loadCubeMap _ = error "Usage : LoadCubeMap [f1,f2,f3,f4,f5,f6] with cube map texture files"                          
  
