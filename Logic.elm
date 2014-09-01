module Logic (PhysicsObject, GameState, gameState) where

import Time (Time, every, second, fps)
import Random
import Mouse
import Debug
import Keyboard

{- Constants -}
framesPerSecondToUpdateGravity = 30

{- Types -}
type GameState = 
  { objs : [PhysicsObject]
  , player : PhysicsObject
  , score : Int }
type PhysicsObject = { x : Float, y : Float, vx : Float, vy : Float }

type Action = (GameState -> GameState)

doNothingAction : Signal Action
doNothingAction = constant id

initialGameState : GameState
initialGameState = { objs = [], player = defaultPhysicsObject , score = 0}

defaultPhysicsObject : PhysicsObject
defaultPhysicsObject = { x = 0, y = 0, vx = 0, vy = 0 }

{- Exported game state: this represents the state of the game at any moment in time -}
gameState : Signal GameState
gameState = foldp applyActions initialGameState (merges
  [ lift appendNewThing (Random.float clickStream)
  , fpsActions
  , lift updatePlayerPosition mousePositionStream
  ])

applyActions : Action -> GameState -> GameState
applyActions action state = action state

fpsActions : Signal Action
fpsActions = foldl (lift2 (.)) doNothingAction
  [ lift killThingsThatAreTooFarDown timeStream
  , lift applyGravityToBlocks timeStream
  , lift killBlocksThatHitThePlayer timeStream
  ]


{- Streams -}
{- Updates every N times per second, good for simulation -}
timeStream : Signal Time
{-timeStream = lift (\{y} -> 0.01 * (toFloat y) * second) Keyboard.arrows-}
timeStream = fps framesPerSecondToUpdateGravity

{- Updates when the mouse is clicked -}
clickStream : Signal ()
clickStream = Mouse.clicks

{- Updates when the mouse is moved -}
mousePositionStream : Signal (Float,Float)
mousePositionStream = lift mouseTo3D Mouse.position

{-
 - Actions that update the state of the simulation
 -}

{- Actions affecting the player -}
updatePlayerPosition : (Float,Float) -> GameState -> GameState
updatePlayerPosition position state = { state | player <- (atPosition position)}

atPosition : (Float,Float) -> PhysicsObject
atPosition (x,y) = { x = x, y = y, vx = 0, vy = 0 }

to3D : Int -> Float
to3D c = (toFloat c - 300) / 50.0

mouseTo3D : (Int,Int) -> (Float,Float)
mouseTo3D (x,y) = (0 - to3D x, to3D y)

{- Apply gravity to all the created blocks N times per second -}
applyGravityToBlocks : Time -> GameState -> GameState
applyGravityToBlocks t state = { state | objs <- map (applyGravity t) state.objs }

applyGravity : Time -> PhysicsObject -> PhysicsObject
applyGravity ms thing = let t = ms / 1000.0
                        in { thing | y <- thing.y + thing.vy + t,
                                     vy <- thing.vy + t }


{- For blocks that have fallen below the kill zone, remove them -}
killThingsThatAreTooFarDown : Time -> GameState -> GameState
killThingsThatAreTooFarDown _ state = state
  |> (changeScoreWhen (below -30) -5)
  |> (killWhen (below -30))

{- Remove blocks that the player hits -}
killBlocksThatHitThePlayer : Time -> GameState -> GameState
killBlocksThatHitThePlayer _ state = state
  |> (changeScoreWhen blockHitsThePlayer 10)
  |> (killWhen blockHitsThePlayer)

blockHitsThePlayer : GameState -> PhysicsObject -> Bool
blockHitsThePlayer {player} {x,y} =
    (withinHitbox player.y y) && (withinHitbox player.x x)

withinHitbox : Float -> Float -> Bool
withinHitbox a b = (a + 1 > b) && (a - 1 < b)

{- Create Blocks when the user Clicks their mouse anywhere on the page -}
appendNewThing : Float -> GameState -> GameState
appendNewThing x state = { state | objs <- state.objs ++ [newRandomThing x] }

newRandomThing : Float -> PhysicsObject
newRandomThing x = { x = x * 10 - 5, y = -5, vx = 0, vy = 0 }

{- Helper functions for manipulating game state -}
changeScoreWhen : (GameState -> PhysicsObject -> Bool) -> Int -> GameState -> GameState
changeScoreWhen predicate amount state = { state
  | score <- state.score + (amount * (length (filter (predicate state) state.objs))) }

killWhen : (GameState -> PhysicsObject -> Bool) -> GameState -> GameState
killWhen predicate state = { state
  | objs <- filter (neg (predicate state)) state.objs } 

below : Float -> GameState -> PhysicsObject -> Bool
below threshold _ block = -block.y < threshold


neg : (a -> Bool) -> a -> Bool
neg f a = not (f a)
