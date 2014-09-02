module Logic (PhysicsObject, GameState, gameState) where

import Time (Time, every, second, fps)
import Random
import Mouse
import Debug
import Keyboard
import Math.Vector3 (Vec3, vec3, getX, getY, setX, setY)
import Math.Matrix4 (Mat4, identity, makeRotate, rotate)

{- Constants -}
framesPerSecondToUpdateGravity = 30

{- Types -}
type GameState = 
  { objs : [PhysicsObject (BornOn {})]
  , player : PhysicsObject {}
  , score : Int }
type PhysicsObject a = { a | pos : Vec3, vel : Vec3, rot : Mat4 }
type BornOn a = { a | bornOn : Time }

type Action = (GameState -> GameState)

doNothingAction : Signal Action
doNothingAction = constant id

initialGameState : GameState
initialGameState =
  { objs = []
  , player = { defaultPhysicsObject | rot <- makeRotate 30 (vec3 1 0 0) }
  , score = 0}

defaultPhysicsObject : PhysicsObject {}
defaultPhysicsObject = { pos = vzero, vel = vzero, rot = identity }

vzero = vec3 0 0 0



{- Exported game state: this represents the state of the game at any moment in time -}
gameState : Signal GameState
gameState = foldp applyActions initialGameState (merges
  [ lift2 appendNewThing (Random.float (fps 5)) (sampleOn (fps 5) timeStream)
  , lift applyPhysicsToBlocks frameTimeStream
  , allInStream
    [ killThingsThatAreTooFarDown
    , killBlocksThatHitThePlayer
    ] timeStream
  , lift updatePlayerPosition mousePositionStream
  ])

applyActions : Action -> GameState -> GameState
applyActions action state = action state

allInStream : [(a -> Action)] -> (Signal a) -> (Signal Action)
allInStream actions stream = foldl (lift2 (.)) doNothingAction
  (map (\a -> lift a stream) actions)

frameTimeStream : Signal Time
frameTimeStream = fps framesPerSecondToUpdateGravity

timeStream : Signal Time
timeStream = every (second / framesPerSecondToUpdateGravity)

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

atPosition : (Float,Float) -> PhysicsObject a -> PhysicsObject a
atPosition (x,y) player = { player | pos <- vec3 x y 1, vel <- vzero }

to3D : Int -> Float
to3D c = (toFloat c - 300) / 50.0

mouseTo3D : (Int,Int) -> (Float,Float)
mouseTo3D (x,y) = (0 - to3D x, to3D y)

{- Apply gravity to all the created blocks N times per second -}
applyPhysicsToBlocks : Time -> GameState -> GameState
applyPhysicsToBlocks t state = { state | objs <- map ((applyGravity t) . (applyRotation t)) state.objs }

applyGravity : Time -> PhysicsObject a -> PhysicsObject a
applyGravity ms obj = let t = ms / 1000.0
                          pos = obj.pos
                          vel = obj.vel
                          y = (getY pos)
                          vy = (getY vel)
                        in { obj | pos <- setY (y + vy + t) pos,
                                   vel <- setY (vy + t) vel }

applyRotation : Time -> PhysicsObject a -> PhysicsObject a
applyRotation ms obj = { obj | rot <- rotate (ms/400) (vec3 1 1 0) obj.rot }

{- For blocks that have fallen below the kill zone, remove them -}
killThingsThatAreTooFarDown : Time -> GameState -> GameState
killThingsThatAreTooFarDown _ state = state
  |> (changeScoreWhen (below -5) -50)
  |> (killWhen (below -5))

{- Remove blocks that the player hits -}
killBlocksThatHitThePlayer : Time -> GameState -> GameState
killBlocksThatHitThePlayer time state = state
  |> (scoreWhenBlockHitsPlayer time)
  |> (killWhen (toP blockHitsThePlayer))

scoreWhenBlockHitsPlayer time state =
  let player = state.player
      pos1 = state.player.pos
      scorer obj = 10 * (btoi (blockHitsThePlayer pos1 obj.pos)) * (1000/(time - obj.bornOn))
  in { state | score <- state.score + round (sum (map scorer state.objs)) }

btoi b = if b == True then 1 else 0

toP : (Vec3 -> Vec3 -> Bool) -> GameState -> (PhysicsObject (BornOn {})) -> Bool
toP f state obj = f state.player.pos obj.pos

blockHitsThePlayer : Vec3 -> Vec3 -> Bool
blockHitsThePlayer pos1 pos2 =
    (withinHitbox (getY pos1) (getY pos2))
    && (withinHitbox (getX pos1) (getX pos2))

withinHitbox : Float -> Float -> Bool
withinHitbox a b = (a + 1 > b) && (a - 1 < b)

{- Create Blocks when the user Clicks their mouse anywhere on the page -}
appendNewThing : Float -> Time -> GameState -> GameState
appendNewThing x time state = { state
  | objs <- state.objs ++ [newRandomThing x time] }

newRandomThing : Float -> Time -> PhysicsObject (BornOn {})
newRandomThing x time =
  { pos = vec3 (x * 10 - 5) (-5) 1
  , vel = vzero
  , rot = makeRotate x (vec3 1 1 0)
  , bornOn = time }

{- Helper functions for manipulating game state -}
changeScoreWhen : (GameState -> PhysicsObject (BornOn {}) -> Bool) -> Int -> GameState -> GameState
changeScoreWhen predicate amount state = { state
  | score <- state.score + (amount * (length (filter (predicate state) state.objs))) }

killWhen : (GameState -> (PhysicsObject (BornOn {})) -> Bool) -> GameState -> GameState
killWhen predicate state = { state
  | objs <- filter (neg (predicate state)) state.objs } 

below : Float -> GameState -> (PhysicsObject a) -> Bool
below threshold _ block = -(getY block.pos) < threshold


neg : (a -> Bool) -> a -> Bool
neg f a = not (f a)

