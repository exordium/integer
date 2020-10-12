{-# language NoImplicitPrelude #-}
module Prelude (module X, module Prelude) where
import Prim as X hiding (Int,B,IO)
import Stock.B as X
import Stock.Ord as X (Ordering)
import Stock.Int as X
import Stock.Word as X
import Stock.IO as X

import GHC.Integer as X
import GHC.Integer.GMP.Internals as X

type I = Integer
