
module Game where

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)
import Mouse
import Window
import Text (leftAligned, monospace, toText, asText, plainText)

import Logic (gameState, PhysicsObject, GameState)
import Models

-- Create the scene

main : Signal Element
main = lift3 page gameState accumScores (webgl <~ squareWindowDimensions ~ lift scene gameState)

page : GameState -> [Int] -> Element -> Element
page state scores glScene = flow down
  [ leftAligned <| monospace <| toText <| "Score: " ++ (show state.score)
  , flow right [glScene{-, flow down (map (plainText . show) scores)-}]
  ]


accumScores : Signal [Int]
accumScores = foldp (\a b -> b ++ [a]) [] (dropRepeats (lift .score gameState))


squareWindowDimensions : Signal (Int,Int)
squareWindowDimensions = lift (\(x,y) -> let m = (min x y)
                                         in (m,m)) Window.dimensions

angle : Signal Float
angle = foldp (\dt theta -> theta + dt / 5000) 0 (fps 30)

scene : GameState -> [Entity]
scene state =
    [ makePlayerEntity state.player ] ++ (map makeFallingCube state.objs)

type Model = { position: Vec3, rotation: Mat4, mesh: [Triangle Models.Vertex]}

makeFallingCube : PhysicsObject a -> Entity
makeFallingCube physics = makeEntity
  { position = physics.pos
  , rotation = physics.rot
  , mesh = Models.cat }

makePlayerEntity : PhysicsObject a -> Entity
makePlayerEntity physics = makeEntity
  { position = physics.pos
  , rotation = physics.rot
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