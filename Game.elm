module Game where

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

-- Create a cube in which each vertex has a position and color

type Vertex = { color:Vec3, position:Vec3 }

face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Triangle Vertex]
face color a b c d =
  let toV3 color =
        let c = toRgb color
        in  vec3 (toFloat c.red / 255) (toFloat c.green / 255) (toFloat c.blue / 255)
      p = Vertex (toV3 color)
  in
      [ (p a, p b, p c), (p c, p d, p a) ]

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

-- Create the scene

main : Signal Element
main = webgl (700,700) <~ lift scene angle

angle : Signal Float
angle = foldp (\dt theta -> theta + dt / 5000) 0 (fps 120)

scene : Float -> [Entity]
scene angle =
    [ cubeEntity angle 0, cubeEntity (angle * 2) 3 ]

cubeEntity : Float -> Float -> Entity
cubeEntity angle position = entity vertexShader fragmentShader cube (uniforms angle position)

uniforms : Float -> Float -> { rotation:Mat4, perspective:Mat4, camera:Mat4, shade:Float }
uniforms t p =
    { rotation = mul (makeRotate (3*t) (vec3 0 1 0)) (makeRotate (2*t) (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 15) (vec3 p 0 1) (vec3 0 1 0)
    , shade = 0.8
    }

-- Shaders

vertexShader : Shader { attr | position:Vec3, color:Vec3 }
                      { unif | rotation:Mat4, perspective:Mat4, camera:Mat4 }
                      { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * rotation * vec4(position, 1.0);
    vcolor = color;
}

|]

fragmentShader : Shader {} { u | shade:Float } { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]