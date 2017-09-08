module Examples where

import OctopusIntention
import TemporalOctopus
import DrawingExamples

-- | Running test                           
                     
testScene :: TemporalOctopus Float
testScene = makeTemporalOctopus $ \t -> scale 3 <> shift (0,0,-9) <>

  
           -- shift (0,-20*(-0.5+cos(t/6)),-40*(0.5+sin(t/5))) <>
              reset(shift (0,0,-4) <> rot_x (t/2) <> rot_y (t/3) <>rot_z(t/5) <> -- scale_y (1/3) <>
                    cone 2 1 gold
                    -- poly 3 3 (V4 1 1 1 1)
                    -- cylinder 3 3 (V4 0.1 0.1 0.1 1)
                    -- torus 3 0.5 (V4 0.1 0.1 0.1 1)
                    -- sphere 2  (V4 0.1 0.1 0.1 1)
                   ) <>
              reset( (shift (40,0,0)) <>  sphere 1 (V4 1 0 0 1))
              <> reset( (shift (-10,13,0)) <> flowerP t)
              <> reset( (shift (0,-10,40)) <> sphere 3 (V4 0 1 1 1))
              <> reset( (shift (-30,20,0)) <> sphere 2 (V4 0 0 1 1))
              <> 
              -- scale_x (1/2) <>
             rot_y(t/4) <> rot_z(t/5) <> sphere 0.8 (V4 1 1 1 1) <> 
             

              reset(scale (1/3) <> shift (-2,-1,0) -- <> rot_x(t/3) <> rot_y(t/5)
                              <> gearMultiplier 6 10  1 0.5 silver gold t) <>
                  
                   
 
              shift (0,0,-5) <>
--              reset( shift (5,9,-5) <>  scale (1/13) <>  rot_x (t/2) <> rot_y (t/3) <>rot_z(t/5) <> flower (t/9+50) 10) <>
              shift (0,0,-10) <>
--              reset(shift (30,-95,-55) <> rot_x (t/7) <> rot_y (t/3) <>rot_z(t/5) <>  poly 3 80 (V4 0 0 1 1)) <>
              shift (-1,2,0) <> rot_x (t/7) <> rot_y (t/3) <>rot_z(t/5) <>
                          sDisk 6 5 0.1 (V4 1 0 0 1) (V4 1 1 0 1) <>
                          disk 4 3 0.1 (V4 0 1 0 1) (V4 0 1 1 1) <>
                          sphere 2 (V4 1 0 0 1) <>
              -- reset(shift (0,0,-10) <> scale (1/10) <> flower  (t/6) 15) <>
               
              shift (0,0,15) 
               <>
              shift (0,0,-7) <> (scale (1/26)) <>  (rot_x (t/3) <> rot_y (t/4+15) <> rot_z(t/13)) <> 
              reset ( (rot_x (t/2) <> rot_y (t/4) <> rot_z(t/5))
                            <> cone 24 12 (V4 1 0 1 1)) -- gear 23 17 12 3 (V4 1 1 1 1)
                    -- <> sphere 13 (V4 1 1 1 1)
                    
              <>               shift (-90,10,-5) <>

 reset( scale 2 <>               scale_z (1/8) <>
              disk 13 9 1 (V4 0 1 1 1) (V4 0 1 0 1) <>
              shift (0,0,-20) <>     
              disk 19 15 1 (V4 1 0 0 1) (V4 0 0 1 1) <>
              shift (0,0,-20) <>     
              disk 22 21 1 (V4 1 0 0 1) (V4 1 1 0 1) <>
              -- torus 18 1 (V4 0 1 1 1) <> 
              -- torus 24 4 (V4 0 0 1 1) <>
              scale_z (8) <>
              shift (0,0,-30) <> chichi 5 6 30 (V4 1 1 0 1) <>
              foldMap (\n -> rot_z(t*2*(sin (3*fromIntegral n))) <> poly n 9 (V4 1 0 0 1)) [3..12]) <>
                         (rot_x (t/7) <> rot_y (t/9) <> rot_z(t/13))

       <> emptyPen <>  (shift (0,-16,-16)) <>
       rot_x (t/4) <> rot_y (t/2+15) <> rot_z(t/3)
              <> scale 1.5 <> shift (0,30,30) <> cyclePen 0 (V4 1 0 0 1) <>  (shift (0,0,1))
       <> cyclePen 3 (V4 0 1 0 1) <>  (shift (0,0,1))
       <> cyclePen 3 (V4 0 1 0 1) <> cyclePen 0 (V4 1 0 0 1)
              <>  (shift (0,0,2))
          
       <> rot_x t <> sphere 5 (V4 1 1 0 1)  <> rot_x (-t) <>  (shift (0,0,2))
       -- <> reset( (scale (1/2)) <>  (shift (70,30,-60)) <> flower (t/9+20) 10)
                                                 
       <> cyclePen 0 (V4 1 0 0 1) <> cyclePen 3 (V4 1 1 1 1) <>  shift (0,0,1)
       <> cyclePen 3 (V4 0 0 1 1) 
       <> shift (0,0,1)
       <> cyclePen 0 (V4 1 1 1 1)
       <> emptyPen
