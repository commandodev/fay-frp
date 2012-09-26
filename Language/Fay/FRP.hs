module Language.Fay.FRP where

import           Prelude

type Event a = [a]

data Stream b a = Stream (b -> (a, Stream b a))

data Coroutine i o = Coroutine { runC :: i -> (o, Coroutine i o) }

-- instance Functor (Coroutine i) where
fmap = fmapC

fmapC :: (a -> o) -> Coroutine i a -> Coroutine i o
fmapC f co = Coroutine $ \i ->
        let (o, coNext) = runC co i
        in (f o, fmapC f coNext)

-- instance Applicative (Coroutine i) where
pure = pureC
(<*>) = appC

pureC :: o -> Coroutine i o
pureC x = Coroutine $ const (x, pure x)

appC :: Coroutine i (a -> o) -> Coroutine i a -> Coroutine i o
cof `appC` cox = Coroutine $ \i ->
        let (f, cof') = runC cof i
            (x, cox') = runC cox i
        in (f x, cof' `appC` cox')


-- instance Category Coroutine where
    -- id = idC

    -- (.) = (<.>)

idC :: Coroutine o o
idC = Coroutine $ \i -> (i, idC)

(<<<) :: Coroutine b o -> Coroutine i b -> Coroutine i o
cof <<< cog = Coroutine $ \i ->
        let (x, cog') = runC cog i
            (y, cof') = runC cof x
        in (y, cof' <<< cog')

(>>>) = flip (<<<)

-- instance Arrow Coroutine where
arr = arrC
first = firstC

arrC :: (i -> o) -> Coroutine i o
arrC f = Coroutine $ \i -> (f i, arr f)

firstC :: Coroutine i t -> Coroutine (i, t1) (t, t1)
firstC co = Coroutine $ \(a,b) ->
        let (c, co') = runC co a
        in ((c,b), firstC co')

-- instance ArrowLoop Coroutine
loop = loopC

loopC :: Coroutine (b,d) (c,d) -> Coroutine b c
loopC co = Coroutine $ \b ->
        let ((c,d),co') = runC co (b,d)
        in (c, loopC co')

evalList :: Coroutine i o -> [i] -> [o]
evalList _  []     = []
evalList co (x:xs) = o:evalList co' xs
    where (o, co') = runC co x

intsFrom :: Integer -> Coroutine () Integer
intsFrom n = Coroutine $ \_ -> (n, intsFrom (n+1))

scan :: (a -> b -> a) -> a -> Coroutine b a
scan f i = Coroutine $ step i where
    step a b = let a' = f a b in (a', scan f a')

accumSum :: Coroutine Double Double
accumSum = scan (+) 0


-- | Map events into different kinds of events
mapE :: (e -> e') -> Coroutine (Event e) (Event e')
mapE = arr . map

-- | Filter events based on a predicate function
filterE :: (e -> Bool) -> Coroutine (Event e) (Event e)
filterE = arr . filter

-- | Replace every event occurence with a fixed event
constE :: e -> Coroutine (Event e') (Event e)
constE = mapE . const

-- | Merge two time varying values using a combining function
zipWithC :: (a -> b -> c) -> Coroutine (a, b) c
zipWithC = arr . uncurry

-- | Merge two event streams together
zipE :: Coroutine (Event e, Event e) (Event e)
zipE = zipWithC (++)

scanE :: (a -> e -> a) -> a -> Coroutine (Event e) a
scanE f i = Coroutine $ step i where
    step a e = let a' = foldl f a e in (a', scanE f a')

-- | Split a value into (current value, previous value) using the given
--   initial value as the previous value during first call.
withPrevious :: a -> Coroutine a (a, a)
withPrevious prev = Coroutine $ \i -> ((i, prev), step i) where
    step old = Coroutine $ \i -> ((i, old), step i)

-- | Delay the value by a single time-step, using the given initial value for
--   the first call.
delay :: a -> Coroutine a a
delay a = withPrevious a >>> arr snd

-- | Integrate a numerical value over time
integrate :: Num a => a -> Coroutine a a
integrate = scan (+)

-- | Derivate a numerical value over time (i.e. return the delta between current
--   and previous time-step.
derivate :: Num a => Coroutine a a
derivate = withPrevious 0 >>> zipWithC (-)

-- | Trigger an event whenever the value satisfies the given predicate function
watch :: (a -> Bool) -> Coroutine a (Event a)
watch f = Coroutine $ \i ->
    if f i
        then ([i], watch f)
        else ([], watch f)
