
module Game where

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)
import Mouse
import Window

import Logic (gameState, PhysicsObject, GameState)
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

type Model = { position: Vec3, rotation: Mat4, mesh: [Triangle Models.Vertex]}

makeFallingCube : PhysicsObject -> Entity
makeFallingCube thing = makeEntity
  { position = vec3 thing.x thing.y 1
  , rotation = identity
  , mesh = Models.cat }

makePlayerEntity : Float -> (Int,Int) -> Entity
makePlayerEntity angle mousePosition = makeEntity
  { position = (mouseTo3D mousePosition)
  , rotation = makeRotate 15 (vec3 1 0 0)
  , mesh = Models.bucket }


makeEntity : Model -> Entity
makeEntity model = entity vertexShader fragmentShader model.mesh (uniforms model.position model.rotation)

uniforms : Vec3 -> Mat4 -> { rotation:Mat4, perspective:Mat4, camera:Mat4, shade:Float }
uniforms position rotation =
    { rotation = rotation
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 15) position (vec3 0 1 0)
    , shade = 0.8
    }


to3D : Int -> Float
to3D c = (toFloat c - 300) / 50.0

mouseTo3D : (Int,Int) -> Vec3
mouseTo3D (x,y) = vec3 (0 - to3D x) (to3D y) 1

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