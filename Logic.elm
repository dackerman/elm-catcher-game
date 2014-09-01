module Logic (PhysicsObject, GameState, gameState) where

import Time (Time, every, second, fps)
import Random
import Mouse
import Debug
import Keyboard
import Math.Vector3 (Vec3, vec3, getX, getY, setX, setY)
import Math.Matrix4 (Mat4, identity, makeRotate)

{- Constants -}
framesPerSecondToUpdateGravity = 30

{- Types -}
type GameState = 
  { objs : [PhysicsObject]
  , player : PhysicsObject
  , score : Int }
type PhysicsObject = { pos : Vec3, vel : Vec3, rot : Mat4 }

type Action = (GameState -> GameState)

doNothingAction : Signal Action
doNothingAction = constant id

initialGameState : GameState
initialGameState =
  { objs = []
  , player = { defaultPhysicsObject | rot <- makeRotate 30 (vec3 1 0 0) }
  , score = 0}

defaultPhysicsObject : PhysicsObject
defaultPhysicsObject = { pos = vzero, vel = vzero, rot = identity }

vzero = vec3 0 0 0



{- Exported game state: this represents the state of the game at any moment in time -}
gameState : Signal GameState
gameState = foldp applyActions initialGameState (merges
  [ lift appendNewThing (Random.float (fps 5))
  , allInStream
    [ killThingsThatAreTooFarDown
    , applyGravityToBlocks
    , killBlocksThatHitThePlayer
    ] timeStream
  , lift updatePlayerPosition mousePositionStream
  ])

applyActions : Action -> GameState -> GameState
applyActions action state = action state

allInStream : [(a -> Action)] -> (Signal a) -> (Signal Action)
allInStream actions stream = foldl (lift2 (.)) doNothingAction
  (map (\a -> lift a stream) actions)


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
updatePlayerPosition position state = { state | player <- (atPosition position state.player)}

atPosition : (Float,Float) -> PhysicsObject -> PhysicsObject
atPosition (x,y) player = { player | pos <- vec3 x y 1, vel <- vzero }

to3D : Int -> Float
to3D c = (toFloat c - 300) / 50.0

mouseTo3D : (Int,Int) -> (Float,Float)
mouseTo3D (x,y) = (0 - to3D x, to3D y)

{- Apply gravity to all the created blocks N times per second -}
applyGravityToBlocks : Time -> GameState -> GameState
applyGravityToBlocks t state = { state | objs <- map (applyGravity t) state.objs }

applyGravity : Time -> PhysicsObject -> PhysicsObject
applyGravity ms obj = let t = ms / 1000.0
                          pos = obj.pos
                          vel = obj.vel
                          y = (getY pos)
                          vy = (getY vel)
                        in { obj | pos <- setY (y + vy + t) pos,
                                   vel <- setY (vy + t) vel }


{- For blocks that have fallen below the kill zone, remove them -}
killThingsThatAreTooFarDown : Time -> GameState -> GameState
killThingsThatAreTooFarDown _ state = state
  |> (changeScoreWhen (below -30) -50)
  |> (killWhen (below -30))

{- Remove blocks that the player hits -}
killBlocksThatHitThePlayer : Time -> GameState -> GameState
killBlocksThatHitThePlayer _ state = state
  |> (changeScoreWhen blockHitsThePlayer 10)
  |> (killWhen blockHitsThePlayer)

blockHitsThePlayer : GameState -> PhysicsObject -> Bool
blockHitsThePlayer {player} {pos} =
    (withinHitbox (getY player.pos) (getY pos))
    && (withinHitbox (getX player.pos) (getX pos))

withinHitbox : Float -> Float -> Bool
withinHitbox a b = (a + 1 > b) && (a - 1 < b)

{- Create Blocks when the user Clicks their mouse anywhere on the page -}
appendNewThing : Float -> GameState -> GameState
appendNewThing x state = { state | objs <- state.objs ++ [newRandomThing x] }

newRandomThing : Float -> PhysicsObject
newRandomThing x =
  { pos = vec3 (x * 10 - 5) (-5) 1
  , vel = vzero
  , rot = makeRotate x (vec3 0 1 0) }

{- Helper functions for manipulating game state -}
changeScoreWhen : (GameState -> PhysicsObject -> Bool) -> Int -> GameState -> GameState
changeScoreWhen predicate amount state = { state
  | score <- state.score + (amount * (length (filter (predicate state) state.objs))) }

killWhen : (GameState -> PhysicsObject -> Bool) -> GameState -> GameState
killWhen predicate state = { state
  | objs <- filter (neg (predicate state)) state.objs } 

below : Float -> GameState -> PhysicsObject -> Bool
below threshold _ block = -(getY block.pos) < threshold


neg : (a -> Bool) -> a -> Bool
neg f a = not (f a)

