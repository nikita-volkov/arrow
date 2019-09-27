module Arrow
where

import Arrow.Prelude


concattingArr :: (ArrowApply arr, ArrowPlus arr) => arr a b -> arr [a] b
concattingArr arrow =
  arr (\ case
    head : tail -> ((constArr head >>> arrow) <+> (constArr tail >>> concattingArr arrow), ())
    _ -> (zeroArrow, ())
  ) >>>
  app

constArr :: Arrow arr => b -> arr a b
constArr a = arr (const a)

traversingArr :: ArrowChoice a => a b c -> a [b] [c]
traversingArr f = proc list -> case list of
  x : xs -> do
    y <- f -< x
    ys <- traversingArr f -< xs
    returnA -< y : ys
  _ -> returnA -< []

traversingArr_ :: ArrowChoice a => a b () -> a [b] ()
traversingArr_ f = proc list -> case list of
  x : xs -> do
    f -< x
    traversingArr_ f -< xs
  _ -> returnA -< ()
