module Models where

import Math.Vector3 (Vec3, vec3, add, scale, getX, getY, getZ)
import Math.Matrix4
import Graphics.WebGL (Triangle, mapTriangle)
import Http
import Array (initialize, toList)


type Vertex = { color:Vec3, position:Vec3 }


bucket : [Triangle Vertex]
bucket = cylinder


cat : [Triangle Vertex]
cat = cube


{- High level shapes -}
cube : [Triangle Vertex]
cube =
  let rft = vec3  1  1  1   -- right, front, top
      lft = vec3 -1  1  1   -- left,  front, top
      lbt = vec3 -1 -1  1
      rbt = vec3  1 -1  1
      rbb = vec3  1 -1 -1
      rfb = vec3  1  1 -1
      lfb = vec3 -1  1 -1
      lbb = vec3 -1 -1 -1
  in
      concat [ face green  rft rfb rbb rbt   -- right
             , face blue   rft rfb lfb lft   -- front
             , face yellow rft lft lbt rbt   -- top
             , face red    rfb lfb lbb rbb   -- bottom
             , face purple lft lfb lbb lbt   -- left
             , face orange rbt rbb lbb lbt   -- back
             ]

face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Triangle Vertex]
face color a b c d = let f = withColor color
                     in  [ (f a, f b, f c)
                         , (f c, f d, f a) ]

cylinderResolution = 10
cylinderRadius = 1

cylinder : [Triangle Vertex]
cylinder = let circ = pointCircle cylinderRadius cylinderResolution
               dir = (vec3 0 0 1)
               circ2 = move dir circ
           in
               colorTriangles red (circCap circ) ++
               colorTriangles blue (extrude dir circ) ++
               colorTriangles green (circCap circ2)



{- Triangle generating functions -}
circCap : [Vec3] -> [Triangle Vec3]
circCap points = let center = centerPoint points
                 in zip3 points (rotate points) (repeat (length points) center)

extrude : Vec3 -> [Vec3] -> [Triangle Vec3]
extrude moveVector a = let b = move moveVector a
                       in (zip3 a b (rotate a)) ++ (zip3 (rotate a) b (rotate b))



{- Color manipulation functions -}
colorTriangles : Color -> [Triangle Vec3] -> [Triangle Vertex]
colorTriangles color tris = map (mapTriangle (withColor color)) tris

colorToVec : Color -> Vec3
colorToVec color =
    let c = toRgb color
    in  vec3 (toFloat c.red / 255) (toFloat c.green / 255) (toFloat c.blue / 255)


withColor : Color -> Vec3 -> Vertex
withColor color = Vertex (colorToVec color)



{- Vector3 manipulation and creation functions -}
move : Vec3 -> [Vec3] -> [Vec3]
move moveVector points = map (add moveVector) points

pointCircle : Float -> Int -> [Vec3]
pointCircle radius segments = toList (initialize segments
  (\i -> scale radius (makePointOnCircle (toFloat i / toFloat segments))))

makePointOnCircle : Float -> Vec3
makePointOnCircle p = vec3 (cos (turns p)) (sin (turns p)) 0

centerPoint : [Vec3] -> Vec3
centerPoint inputs = vec3 (avg (map getX inputs)) (avg (map getY inputs)) (avg (map getZ inputs))



{- Generic functions -}
rotate : [a] -> [a]
rotate a = (tail a) ++ [head a]

avg : [Float] -> Float
avg xs = sum xs / (toFloat (length xs))

zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 a b c = zipWith (\(a,b) c -> (a,b,c)) (zip a b) c

