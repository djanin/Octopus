module Color where

import MatrixOpt(V4(V4))

type Color d = V4 d
    -- To do : a better color type with predefined ones and nice merges...


silver, gold, copper, steel, bronze, iron :: Fractional d => Color d
silver = makeColor 220 210 167 255
gold = makeColor 215 175 0 255
copper =  makeColor 179 103 0 255
steel = makeColor  165 165 165 255
bronze = makeColor 97 78 26 255
iron = makeColor 127 127 127 255

makeColor r v b a =
    1/255*V4 r v b a
