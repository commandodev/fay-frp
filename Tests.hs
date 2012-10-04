{-# LANGUAGE NoImplicitPrelude #-}

module Tests where

import           Language.Fay.FFI
import           Language.Fay.FRP
import           Language.Fay.JQuery
import           Language.Fay.Prelude

void :: Fay a -> Fay ()
void m = m >> return ()

main :: Fay ()
main = ready $ do
  logS "test"
  let fst4 = (takeN 4 [Up, Up, Same, Down] game)
  let repeat x = x:repeat x
  let strPos = ballPos >>> arr (log . show)
  let tups = takeN 5 [1..] (withPrevious 0 >>> (arr (uncurry showTup)))
  --forM_ (takeN 5 [1..] (arr (uncurry showTup) >>> idC &&& delay 1)) $ uncurry void
  let facs = takeN 5 [1..5] fac
  logDs facs
  logD $ test 3
  logS "Done"
  forM_ (evalList strPos [(10, 200), (10, 201), (10, 300), (10, 200)]) void
  where
    fac :: Coroutine Double Double
    fac = loopC (arr (uncurry mult) >>> idC &&& delay 1)

showTup :: Double -> Double -> Fay ()
showTup = ffi "console.log(%1, %2)"

plus a b = a + b
mult a b = a * b

loopPlus :: Coroutine Double Double
loopPlus = loopC $ arr (\(a, b) -> (a `plus` b, a))

test :: Double -> Double
test a = let (n, co) = runC loopPlus a in fst $ runC co a

logD :: Double -> Fay ()
logD = ffi "console.log(%1)"

logDs :: [Double] -> Fay ()
logDs = ffi "console.log(%1)"

type Pos       = (Double, Double)
type Size      = (Double, Double)
type PlayerPos = Pos

type BallPos  = Pos
type Velocity = (Double, Double)
data Rect = Rect (Pos, Pos)
  deriving Show

-- Ball bounce events for horizontal and vertical bounce
data BallBounce = HBounce | VBounce | NoBounce
  deriving Show
data Keyboard = Up | Down | Same

instance Foreign Rect

batSpeed = 5
batSize  = (10,40)
startPos = 200

ballInitPos = (400,200)
ballSize    = (8,8)
ballInitVel = (0-6, 0-6)

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
    NoBounce-> (dx, dy)

mkRect :: Pos -> Size -> Rect
mkRect (x,y) (w,h) = Rect ((x-w',y-h'),(w,h)) where
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

-- traceC :: Show a => Coroutine a a
-- traceC = arr (\x ->
--                trace (show x) x)

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
    step c (i, ev) =  (o, Coroutine cont) where
        (o, c') = runC c i
        cont = if null ev
                  then step c'
                  else step co

ballPos :: Coroutine PlayerPos BallPos
ballPos = loopC $ arr (\(ppos, bpos) -> ((ppos, bpos), bpos))
    >>> batBounce *** wallBounce
    >>> zipE
    >>> scanE bounce ballInitVel
    >>> scan vecAdd ballInitPos
    >>> withPrevious ballInitPos

resettingBallPos :: Coroutine PlayerPos BallPos
resettingBallPos = loopC $ restartWhen ballPos >>> idC &&& watch outOfBounds
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

log2 :: String-> String -> Fay ()
log2 = ffi "console.log(%1, %2)"

showRect:: Rect -> String
showRect (Rect (p1, p2)) ="(" ++ (show p1) ++ ", " ++ show p2 ++ ")"

logS :: String -> Fay ()
logS = ffi "console.log(%1)"

logF :: Foreign a => a -> Fay ()
logF = ffi "console.log(_(%1))"
