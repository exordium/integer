module Big.N.Bit where
import Big.N


shiftL#, shiftR#, clear, complement, set ∷ I → N → N
shiftL# i x = shiftLBigNat x i
shiftR# i x = shiftRBigNat x i
clear i x = clearBitBigNat x i
complement i x = complementBitBigNat x i
set i x = setBitBigNat x i

test ∷ I → N → B
test i x = testBitBigNat x i

(∧),(∨),(⊕), and, or, xor ∷ N → N → N
(∧) = andBigNat; and = andBigNat
(∨) = orBigNat; or = orBigNat
(⊕) = xorBigNat; xor = xorBigNat

popCnt ∷ N → I
popCnt = popCountBigNat
-- | Specialised version of
--
-- > bit = shiftL# (fromU64 1##)
--
-- avoiding a few redundant allocations
bit ∷ I → N
bit = bitBigNat
