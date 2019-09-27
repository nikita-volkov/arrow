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
