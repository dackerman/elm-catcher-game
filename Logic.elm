module Logic (PhysicsObject, GameState, gameState) where

import Time (Time, every, second, fps)
import Random
import Mouse
import Debug

{- Constants -}
framesPerSecondToUpdateGravity = 30

{- Types -}
type GameState = { objs : [PhysicsObject], player : PhysicsObject }
type PhysicsObject = { x : Float, y : Float, vx : Float, vy : Float }

type Action = (GameState -> GameState)

doNothingAction : Signal Action
doNothingAction = constant id

initialGameState : GameState
initialGameState = { objs = [], player = defaultPhysicsObject }

defaultPhysicsObject : PhysicsObject
defaultPhysicsObject = { x = 0, y = 0, vx = 0, vy = 0 }

{- Exported game state: this represents the state of the game at any moment in time -}
gameState : Signal GameState
gameState = foldp applyActions initialGameState (merges
  [ createBlocksActions
  , fpsActions
  , playerActions
  ])

applyActions : Action -> GameState -> GameState
applyActions action state = action state

fpsActions : Signal Action
fpsActions = foldl (lift2 (.)) doNothingAction
  [ killActions
  , gravityActions
  ]


{- Streams -}
{- Updates every N times per second, good for simulation -}
timeStream : Signal Time
timeStream = fps framesPerSecondToUpdateGravity

{- Updates when the mouse is clicked -}
clickStream : Signal ()
clickStream = Mouse.clicks

{- Updates when the mouse is moved -}
mousePositionStream : Signal (Float,Float)
mousePositionStream = lift mouseTo3D Mouse.position


playerActions : Signal Action
playerActions = lift updatePlayerPosition mousePositionStream

updatePlayerPosition : (Float,Float) -> GameState -> GameState
updatePlayerPosition position state = { state | player <- (atPosition position)}

atPosition : (Float,Float) -> PhysicsObject
atPosition (x,y) = { x = x, y = y, vx = 0, vy = 0 }

to3D : Int -> Float
to3D c = (toFloat c - 300) / 50.0

mouseTo3D : (Int,Int) -> (Float,Float)
mouseTo3D (x,y) = (0 - to3D x, to3D y)

{-
 - Actions that update the state of the simulation
 -}

{- Apply gravity to all the created blocks N times per second -}
gravityActions : Signal Action
gravityActions = lift applyGravityToBlocks timeStream

applyGravityToBlocks : Time -> GameState -> GameState
applyGravityToBlocks t state = { state | objs <- map (applyGravity t) state.objs }

applyGravity : Time -> PhysicsObject -> PhysicsObject
applyGravity ms thing = let t = ms / 1000.0
                        in { thing | y <- thing.y + thing.vy + t,
                                     vy <- thing.vy + t }


{- For blocks that have fallen below the kill zone, remove them -}
killActions : Signal Action
killActions = lift killThingsThatAreTooFarDown timeStream

killThingsThatAreTooFarDown : Time -> GameState -> GameState
killThingsThatAreTooFarDown _ state = { state | objs <- filter (above -30) state.objs }

above : Float -> PhysicsObject -> Bool
above threshold thing = -thing.y > threshold


{- Create Blocks when the user Clicks their mouse anywhere on the page -}
createBlocksActions : Signal Action
createBlocksActions = lift appendNewThing (Random.float clickStream)

appendNewThing : Float -> GameState -> GameState
appendNewThing x state = { state | objs <- state.objs ++ [newRandomThing x] }

newRandomThing : Float -> PhysicsObject
newRandomThing x = { x = x * 10 - 5, y = -5, vx = 0, vy = 0 }

