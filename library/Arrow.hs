module Arrow
where

import Arrow.Prelude


concatArr :: (ArrowApply arr, ArrowPlus arr) => arr a b -> arr [a] b
concatArr arrow =
  arr (\ case
    head : tail -> ((constArr head >>> arrow) <+> (constArr tail >>> concatArr arrow), ())
    _ -> (zeroArrow, ())
  ) >>>
  app

constArr :: Arrow arr => b -> arr a b
constArr a = arr (const a)

{-
The implementation is taken from this StackOverflow answer:
https://stackoverflow.com/a/13669572/485115
-}
traverseArr :: ArrowChoice a => a b c -> a [b] [c]
traverseArr arrow =
  arr (\ case
    head : tail -> Right (head, tail)
    _ -> Left ()
  ) >>> (
    constArr [] |||
    (arrow *** traverseArr arrow >>> arr (uncurry (:)))
  )

traverseArr_ :: ArrowChoice a => a b () -> a [b] ()
traverseArr_ f = proc list -> case list of
  x : xs -> do
    f -< x
    traverseArr_ f -< xs
  _ -> returnA -< ()
