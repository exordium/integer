{-# language NoImplicitPrelude #-}
{-# language MagicHash #-}
module Prelude (module X, module Prelude) where
import "prim" B as X (B#, B)
import "prim" I as X (I)
import "prim" U as X (U)
import F32 as X (F32)
import F64 as X (F64)
import "prim" IO as X (type (☸))
import Stock.Ord as X (Ordering)
import Stock.Word as X (Word)
import Stock.Int as X (Int)
import Stock.IO as X (IO)

import GHC.Integer as X
import GHC.Integer.GMP.Internals as X

type Z = Integer
