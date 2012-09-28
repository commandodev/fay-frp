{-# LANGUAGE NoImplicitPrelude #-}

module Tests where

import           Language.Fay.FFI
import           Language.Fay.FRP
import           Language.Fay.JQuery
import           Language.Fay.Prelude

main :: Fay ()

main = ready $ do
  log "Hi"


type Pos       = (Double, Double)
type Size      = (Double, Double)
type PlayerPos = Pos

type BallPos  = Pos
type Velocity = (Double, Double)
type Rect = (Pos, Pos)

-- Ball bounce events for horizontal and vertical bounce
data BallBounce = HBounce | VBounce
data Keyboard = Up | Down | Same


batSpeed = 5
batSize  = (10,40)
startPos = 200

ballInitPos = (400,200)
ballSize    = (8,8)
ballInitVel = (0 - 6, 0 - 6)

topWall    = 10
bottomWall = 590


-- Multiple a vector by a scalar
vecMul :: Double -> (Double, Double) -> (Double, Double)
vecMul c (x,y) = (x*c,y*c)

-- Add two vectors
vecAdd :: (Double, Double) -> (Double, Double) -> (Double, Double)
vecAdd (a,b) (c,d) = (a+c,b+d)

-- Adjust velocity based on a bounce event
bounce :: Velocity -> BallBounce -> Velocity
bounce (dx,dy) b = case b of
    HBounce -> (0-dx,dy)
    VBounce -> (dx,0-dy)

mkRect :: Pos -> Size -> Rect
mkRect (x,y) (w,h) = ((x-w',y-h'),(w,h)) where
    w' = w `div` 2
    h' = h `div` 2

-- Has the ball hit the paddle
batCollision :: (PlayerPos, BallPos) -> Bool
batCollision ((px,py),(bx,by)) = abs (px-bx) < w' && abs (py-by) < h' where
    w' = (bw + pw) `div` 2
    h' = (bh + ph) `div` 2
    (bw,bh) = ballSize
    (pw,ph) = batSize

-- Has the ball hit the wall
wallCollision :: BallPos -> Bool
wallCollision (_,y) = y < topWall || y > bottomWall

-- Game logic
wallBounce :: Coroutine BallPos (Event BallBounce)
wallBounce = watch wallCollision >>> constE VBounce

batBounce :: Coroutine (PlayerPos, BallPos) (Event BallBounce)
batBounce = watch batCollision >>> constE HBounce


ballPos :: Coroutine PlayerPos BallPos
ballPos = loopC $ arr (\(ppos, bpos) -> ((ppos, bpos), bpos))
    >>> batBounce *** wallBounce
    >>> zipE
    >>> scanE bounce ballInitVel
    >>> scan vecAdd ballInitPos
    >>> withPrevious ballInitPos


-- Inputs
playerPos :: Coroutine Keyboard PlayerPos
playerPos = playerSpeed >>> integrate startPos >>> arr (\y -> (10, y))

playerSpeed :: Coroutine Keyboard Double
playerSpeed = arr keyboardDir where
    keyboardDir Up = 0-batSpeed
    keyboardDir Down = batSpeed
    keyboardDir Same = 0


restartWhen :: Coroutine a b -> Coroutine (a, Event e) b
restartWhen co = Coroutine $ step co where
    step c (i, ev) = (o, Coroutine cont) where
        (o, c') = runC c i
        cont
            | null ev   = step c'
            | otherwise = step co


resettingBallPos :: Coroutine PlayerPos BallPos
resettingBallPos = loop $ restartWhen ballPos >>> idC &&& watch outOfBounds
    where outOfBounds (x,_) = x < 0 || x > 800


game :: Coroutine Keyboard [Rect]
game = playerPos >>> idC &&& resettingBallPos
       >>> (arrC $ \(plPos, blPos) -> [mkRect plPos batSize, mkRect blPos ballSize])

-- Not in Prelude
abs :: Double -> Double
abs = ffi "Math.abs(%1)"

div :: Double -> Double -> Double
div = ffi "Math.floor(%1/%2)"

log :: Foreign a => a -> Fay ()
log = ffi "console.log(%1)"

logS :: String -> Fay ()
logS = ffi "console.log(%1)"
