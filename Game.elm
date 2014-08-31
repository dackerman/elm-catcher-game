
module Game where

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)
import Mouse
import Window

import Logic (gameState, Thing, GameState)
import Models

-- Create the scene

main : Signal Element
main = lift2 page gameState (webgl <~ squareWindowDimensions ~ lift3 scene angle Mouse.position gameState)

page : GameState -> Element -> Element
page state glScene = flow right [glScene, asText state]

squareWindowDimensions : Signal (Int,Int)
squareWindowDimensions = lift (\(x,y) -> let m = (min x y)
                                         in (m,m)) Window.dimensions

angle : Signal Float
angle = foldp (\dt theta -> theta + dt / 5000) 0 (fps 30)

scene : Float -> (Int,Int) -> GameState -> [Entity]
scene angle mousePosition state =
    [ makePlayerEntity angle mousePosition ] ++ (map makeFallingCube state.objs)


makeFallingCube : Thing -> Entity
makeFallingCube thing = cubeEntity 30 (thing.x, thing.y) Models.cat

makePlayerEntity : Float -> (Int,Int) -> Entity
makePlayerEntity angle mousePosition = cubeEntity angle (mouseTo3D mousePosition) Models.bucket


cubeEntity : Float -> (Float,Float) -> [Triangle Models.Vertex] -> Entity
cubeEntity angle position model = entity vertexShader fragmentShader model (uniforms angle position)

uniforms : Float -> (Float,Float) -> { rotation:Mat4, perspective:Mat4, camera:Mat4, shade:Float }
uniforms t (x,y) =
    { rotation = mul (makeRotate (3*t) (vec3 0 1 0)) (makeRotate (2*t) (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 15) (vec3 x y 1) (vec3 0 1 0)
    , shade = 0.8
    }


to3D : Int -> Float
to3D c = (toFloat c - 300) / 50.0

mouseTo3D : (Int,Int) -> (Float,Float)
mouseTo3D (x,y) = (0 - to3D x, to3D y)

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