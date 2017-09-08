module DrawingExamples where

import Octopus


-- ** Examples of open surfaces

segment d c = pen $ PenShape (Polygon,1,d,180,c,c)
emSegment d = pen $ PenShape (EmptyPen,1,d,180,0,0)

plan d1 d2 c  = reset(shift (0,-d1/2,0) <> rot_y(pi) <> emSegment d1 <>
                      segment d1 c <> shift (0,0,d2) <>
                      segment d1 c <> emSegment d1)
                          
           
-- ** Examples of closed surfaces

-- | Sphere with specified radius and color
sphere :: (Floating d, RealFrac d, Storable d) => d -> Color d  -> Move d
          -- to do : sphere should be put as a headshape so that smoothing is performed on the GPU
          -- with no waiste of pen positions...
sphere r c =
    let density = 30 ::Integer --- better with 60, but anyway costly in pen's position
        angles = map (\i -> pi*(fromInteger i)/(fromInteger density)) [0..density-1]
        point a =  shift (0,0,-r*cos a) <>
                  cyclePen (r*sin a) c <>
                   (shift (0,0,r*cos a))
        pend = shift (0,0,r) <>            
               cyclePen 0 c <>
               shift (0,0,-r)    
    in reset(emptyPen <> 
             -- emCyclePen point (-1) <>
             foldl (<>) mempty (map point angles) <> pend <>
                        emptyPen)

-- | Torus with specified radiused and color
torus :: (Floating d, RealFrac d, Storable d) => d -> d -> Color d  -> Move d
          -- to do : torus could be put as a headshape so that smoothing is performed on the GPU
          -- with no waiste of pen positions...
torus r1 r2 c =
    let density = 60 in 
    reset (rot_x (pi/2) <> shift (r1,0,0) <>
    loop  (fromInteger density) 
              (shift (-r1,0,0) <> rot_y (-2*pi/(fromInteger density)) <> shift (r1,0,0))
              (cyclePenShape r2 c) <>
    shift (-r1,0,0)) 
 

-- | Disk of specified radiuses, thickness and colors (sharp edge)
disk :: (Floating d, RealFrac d, Storable d) => d -> d -> d -> Color d -> Color d  -> Move d
disk r1 r2 d c1 c2 = shift (0,0,d/2)  <>
           emptyPen <> cyclePen r2 c2 <> shift (0,0,-d) <> cyclePen r2 c2 <>
           cyclePen r2 c2 <> cyclePen r1 c1 <>
           cyclePen r1 c1 <> shift (0,0,d) <> cyclePen r1 c1  <>
           cyclePen r1 c1 <> cyclePen r2 c2 <>
           cyclePen r2 c2 <> shift (0,0,-d) <> cyclePen r2 c2 <>
           cyclePen r2 c2 <> cyclePen r1 c1 <> emptyPen <> shift (0,0,d/2)

-- | Sharp edge disk of specified radiuses, thickness and color ("smooth" edge)
sDisk :: (Floating d, RealFrac d, Storable d) => d -> d -> d -> Color d -> Color d  -> Move d
sDisk r1 r2  d  c1 c2 = shift (0,0,-d/2)  <> emptyPen <>
           cyclePen r2 c2 <> cyclePen r1 c1 <>
           shift (0,0,d) <> cyclePen r1 c1  <>
           cyclePen r2 c2 <> shift (0,0,-d) <> cyclePen r2 c2
             <> emptyPen <> shift (0,0,d/2)
                    
-- | Gear of specified radiuses, teeth number, thickness and color 
gear :: (Floating d, RealFrac d, Storable d) => Int -> d -> d -> d -> Color d -> Move d
gear' n r1 r2 d c  =
    shift (0,0,-d/2)  <> emptyPen <>
           cyclePen r2 c <> gearPen n r1 c <>
           gearPen n r1 c <> shift (0,0,d) <> gearPen n r1 c <>
           gearPen n r1 c <> cyclePen r2 c <>
           cyclePen r2 c <> shift (0,0,-d) <> cyclePen r2 c 
            <> emptyPen <> shift (0,0,d/2)

               
gear  n r1 _ d c =
    let r2 = r1 * (1 - 6/fromIntegral n)
        nr1 = r1/10
        nr2 = r2/11
        nr = (nr1+nr2)/2
    in reset(shift (0,0,-d/2)  <> cylinder d (r1/10)  c)
       <> rgear   n r1 r2 d c
       <> foldl (<>) mempty
              (map (\_ -> (rot_z(2*pi/5) <> reset(shift (0,nr,0) <> scale_x (1/3) <> rot_x(-pi/2) <> cylinder (r2-nr) (d/2) c))) [1..5])    

-- | Gear of specified radiuses, teeth number, thickness and color with better resolution 
rgear :: (Floating d, RealFrac d, Storable d) => Int -> d -> d -> d -> Color d -> Move d
rgear n r1 r2 d c  =
    let nb = 5  -- number of teeth per pieces
        k = fromInteger (div (toInteger (n + nb-1)) (toInteger nb)) -- number of pieces of gears
        alpha = fromIntegral nb / fromIntegral n     
        angle = 360*alpha
        pgear = pen $ PenShape (Gear, nb,r1,angle,c,c)
        pcycle = pen $ PenShape (Circle,fromInteger $ div 30 k,r2,angle,c,c)
        piece =   shift (0,0,-d/2)  <> emptyPen <>
           pcycle <> pgear <>
           pgear <> shift (0,0,d) <> pgear <>
           pgear <> pcycle <>
           pcycle <> shift (0,0,-d) <> pcycle 
            <> emptyPen <> shift (0,0,d/2)
      
    in foldl (<>) mempty (map (\_ -> piece <> rot_z(2*pi*alpha)) [0..k])

gearPair :: (Floating d, RealFrac d, Storable d) => Integer -> Integer  -> d -> d ->  Color  d-> Color d -> d -> Move d
gearPair num den  r' w c1 c2 t =
    let r = 5*r'
        ratio = num%den
        phase = case (mod num 2 == 0) of
                  True -> pi/(fromIntegral den)
                  False -> 0
        r1 = r*fromRational (num%(num+den))
        r2 = r*fromRational (den%(num+den))
    in
      shift (0,0,2*w)
      <> reset (rot_z (2*pi*t) <> gear (fromInteger num) r1 r1 w c1)
      <> shift (-r,0,0)
      <> reset (rot_z (2*pi*(-fromRational ratio)*t+phase) <> gear (fromInteger den) r2 r2 w c2)
      <> shift (0,0,2*w)
    

gearMultiplier :: (Floating d, RealFrac d, Storable d) => Integer -> Integer -> d -> d -> Color d -> Color d
               -> d -> Move d
gearMultiplier n1 n2 r w c1 c2 t
    = let ratio = fromRational ((7+7*n1)%(7+7*n2))
          c3 = (c1 + c2)/2
      in gearPair 7 (7*n1) r w c1 c3 t
         <> reset (shift (0,0,-2.5*w) <> scale_z (5*w) <> cylinder 1 (r/8)  c3)
                      <> rot_z (pi)
      <> gearPair 7 (7*n2) r w c3 c2 (-t/fromInteger (n1))
         
       
-- | Cone with specified height, base radius and colors
cone :: (Floating d, RealFrac d, Storable d) => d -> d -> Color d -> Move d
cone  h r c =
    let slice = 6
    in  emptyPen <>
    -- connecting base
    cyclePen 0 c <> cyclePen r c <>
             
    foldr (<>) (mempty) (fmap (\n -> cyclePen (r*fromInteger (slice - n)/fromInteger slice) c
                                     <> shift (0,0,h/fromInteger slice)) [0..slice-1])
    <> polyPen 1 0 c
    <> emptyPen      
{-    -- disconnecting from previous
    emptyPen <>
    -- connecting base
    cyclePen 0 c <> cyclePen r c <>
    -- connecting cone (repeating head for sharp edge)
    cyclePen r c <> shift (0,0,h) <>
    polyPen 100 0 c <>
    -- disconnecting from next
    emptyPen
-}
-- | Cylinder with specified height, radius and color
cylinder :: (Floating d, RealFrac d, Storable d) => d -> d -> Color d -> Move d
cylinder  h r c =
    -- disconnecting from previous
    emptyPen <>
    -- connecting base
    cyclePen 0 c <> cyclePen r c <>
    -- connecting cone (repeating head for sharp edge)
    cyclePen r c <> shift (0,0,h) <> cyclePen r c <>
    cyclePen r c <> cyclePen 0 c <> 
    -- disconnecting from next
    emptyPen
    
-- | Cube
cube r c =
    emptyPen <>
    polyPen 4 0 c <> polyPen 4 r c <> polyPen 4 r c <> shift (0,0,r) <> 
    polyPen 4 r c <> polyPen 4 r c <> polyPen 4 0 c <> emptyPen 
    
    
-- | Polyhedron
poly n r c =
    emptyPen <>
    polyPen n 0 c <> polyPen n r c <>
    polyPen n r c <> shift (0,0,(sqrt(3)*r*sin(pi/fromIntegral n))) <> polyPen n 0 c <> emptyPen
    
-- | Tetrahedron
tetra r c =
    shift (0,0,-r/2) <> emptyPen <>
    polyPen 4 0 c <> polyPen 4 r c <>
    polyPen 4 r c <> shift (0,0,(r)) <> polyPen 4 0 c <> emptyPen

-- | Sort of a chichi (pastry available on fair camp)
chichi :: (Floating d, RealFrac d, Storable d) => Int -> d -> d -> Color d -> Move d
chichi n r l c =
    let res = 10
        m = round(l*res/r+1)
    in
    emptyPen <>
    polyPen n 0 c <> polyPen n r c <>
    foldl (<>) mempty (map (\_ -> polyPen n r c <>
                                  rot_z (pi/3/res) <>
                                  shift (0,0,(r/res))) [0..m]) <>
    polyPen n r c <> polyPen n r c <> polyPen n 0 c <>
    emptyPen

arrow d c = reset(rot_y(pi/2) <> cyclePen (d/20) c  <>
                  cyclePen (d/20) c  <> shift (0,0,4*d/5) <> cyclePen (d/20) c  <>
                  cyclePen (d/20) c  <>cone (d/5) (d/10) c)

axis :: (Floating d, RealFrac d, Storable d) => Move d            
axis = arrow 1 (V4 1 0 0 1) <> reset(rot_z (pi/2) <> arrow 1 (V4 0 1 0 1)) <> reset(rot_y (-pi/2) <>  arrow 1 (V4 0 0 1 1))            
    
-- * Our running benchmark

      
-- | A branch 
branch  :: (Floating d, RealFrac d, Storable d) => d  -> Move d
branch  t =
   let c n = V4 (1-sin (n/20)) (1-cos (n/23)) (1-sin(n/33+pi/4)) 1
       segment n p =  shift (0,0,4+(2*n/10)) 
                   <>  rot_y ((1+3/(n+1))/7*sin (pi*(20*t+n*(sin (t/3)))/20))
                           <> rot_x ((1+2/(n+1))/7*cos (pi*(30*t+n*(sin(t/4)))/30)) <> ncyclePen 3 1 (c (n+10*t))
                      <> scale (0.96)
   in  reset $ emptyPen <> scale (1/3) <> foldl (<>) mempty  (map (\n -> segment (fromInteger n) 5) [0..22])
           <> scale 4 <> sphere 1 (c t)

-- | A flower
flower :: (Storable d, RealFrac d, Integral a, Floating d) =>
                d -> a -> Move d
flower t' p' = let t = 2*t'
                   p = 2*p'
               in (rot_x (t/7) <> rot_y (t/9) <> rot_z(t/13))
                     <> reset (scale 3 <> pasteque (t/2) <> 
                     foldr (<>) emptyPen  (map (\n -> branch (t +(fromIntegral n)/(fromIntegral p)/3)
                                                   <> rot_y (2*pi/(fromIntegral p)) ) [0..p-1]))
        where pasteque t = sphere 2 (V4 1 (sin t) (cos t) 1)

-- | Anothe flower, bigger, better movement
xTorP ::  (Storable d, RealFrac d, Floating d) =>
                Integer -> Integer -> d -> Move d
xTorP s n t  =
    let f = 1.2
        c = (V4 f f f 1) *( V4 ((100+(cos (t/2))*5*fromInteger s)/255)
                      ((100-(sin (t/3))*3*fromInteger s)/255)
                      ((50+(cos (t/5))*6*fromInteger n)/255) 1)
        tile =
            let zangle = pi*(1/2+sin (2*pi*(t/3+(fromInteger n)/20-(fromInteger s)/5)/10))/14
                sz = (4*fromInteger s + cos (fromInteger s * t/3))/6
            in shift (0.2,0,sz) <> scale (0.99)
               <> rot_x (pi/15*sin (t-(fromInteger s)/3))
               <> rot_y  (-zangle)
    in ncyclePen 3 3 c
       -- polyPen 3 3 c
           <> tile

-- | The dancing flower movie 
flowerP :: (Storable d, RealFrac d, Floating d) =>  d -> Move d
flowerP t' =
    let t = 0.8*t'
        tail n =  reset(rot_y (pi) <>
                        foldl (<>) mempty
                                  (map (\s -> xTorP s n t) [0..40]) <>
                        -- polyPen 3 3 (V4 1 1 0 1)
                        ncyclePen 3 3 (V4 1 1 0 1)       
                                    <> sphere 6 (V4 1 1 0 1))
        tc = t+60
    in  shift (-3*(2+sin(tc/25)),-3*(2+sin(tc/15)) ,2*(3+sin (tc/19))) <>
        rot_z (2*pi*sin (tc/30)) <> rot_x (2*pi*sin (tc/20)) <> rot_y (2*pi*sin (tc/40)) <>
        scale ((2+cos (t/20))/4) <>
        scale (1/10) <>
        reset (scale_x (3/4) <> scale (18) <> sphere  1 (V4 1 1 0 1)) <> shift (0,0,-15) <>
        foldl (<>) mempty (map (\n -> tail n <> shift (0,3,0) <> rot_x (2*pi/30)) [0..29])
                           
