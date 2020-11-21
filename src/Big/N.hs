{-# language MagicHash #-}
module Big.N where
import qualified Array
import qualified Array.Byte
import qualified Ref.Byte as Byte

type N = BigNat
pattern BN# ∷ Array.Byte → N
pattern BN# bs = Prelude.BN# bs
valid' ∷ N → B#
valid' = isValidBigNat#
type Limb = U
type Size = I
size ∷ N → Size
size = sizeofBigNat#
zero, one, null ∷ N
zero = zeroBigNat
one = oneBigNat
-- | Special 0-sized bigNat returned in case of arithmetic underflow
--
-- This is currently only returned by the following operations:
--
--  - '(-)'
--  - 'minusBigNatWord'
--
-- Other operations such as 'quotBigNat' may return 'nullBigNat' as
-- well as a dummy/place-holder value instead of 'undefined' since we
-- can't throw exceptions. But that behaviour should not be relied
-- upon.
--
-- NB: @valid' null@ is false
null = nullBigNat


-- | Construct 'Big.N' from existing 'Array.Byte' containing /n/
-- 'GmpLimb's in least-significant-first order.
--
-- If possible 'Array.Byte', will be used directly (i.e. shared
-- /without/ cloning the 'Array.Byte' into a newly allocated one)
--
-- Note: size parameter (times @sizeof(GmpLimb)@) must be less or
-- equal to its 'Array.Byte.size'.
fromBA ∷ Array.Byte → Size → N
fromBA = byteArrayToBigNat#

-- | 1-limb
fromU ∷ U → N
fromU = wordToBigNat
toU ∷ N → U
toU = bigNatToWord

-- | 2 limbs. The first argument is the most-significant limb.
fromU_2 ∷ U → U → N
fromU_2 = wordToBigNat2

toI ∷ N → I
toI = bigNatToInt

index ∷ N → Size → Limb
index = indexBigNat#

(+), (-), (*), add, sub, mul ∷ N → N → N
(+) = plusBigNat; add y x = x + y
(-) = minusBigNat; sub y x = x - y
(*) = timesBigNat; mul y x = x * y
addW, subW, mulW ∷ Limb → N → N
addW y x = plusBigNatWord x y
-- | 'null' on underflow
subW y x = minusBigNatWord x y
mulW y x = timesBigNatWord x y

sqr ∷ N → N
sqr = sqrBigNat

-- | quotRem n 0 = ( null , null )
quotRem ∷ N → N → (# N , N #)
quotRem y x = quotRemBigNat x y
-- | quotRem n 0 is undefined
quotRemW ∷ Limb → N → (# N , Limb #)
quotRemW y x = quotRemBigNatWord x y

(//),(%%), quot, rem ∷ N → N → N
(//) = quotBigNat; quot y x = x // y
(%%) = remBigNat; rem y x = x %% y
quotW ∷ Limb → N → N
quotW y x = quotBigNatWord x y
remW ∷ Limb → N → U
remW y x = remBigNatWord x y

gcd ∷ N → N → N
gcd = gcdBigNat
gcdW ∷ N → U → U
gcdW = gcdBigNatWord

powMod# ∷ N -- ^ base @b@
        → N -- ^ exponent @e@
        → N -- ^ modulo @m@
        → N 
powMod# = powModBigNat

powModW# ∷ N -- ^ base @b@
         → N -- ^ exponent @e@
         → Limb -- ^ modulo @m@
         → Limb
powModW# = powModBigNatWord


-- * Import/Export
--
sizeInBase ∷ N → I → U
sizeInBase = sizeInBaseBigNat
exportToRef ∷ N → Byte.Ref → I → IO Word
exportToRef = exportBigNatToAddr
exportToBuffer ∷ N → Array.Byte.M (☸) → U → I → IO Word
exportToBuffer = exportBigNatToMutableByteArray
importFromRef ∷ Byte.Ref → U → I → IO N
importFromRef = importBigNatFromAddr
importFromBuffer ∷ Byte.Ref → U → I → IO N
importFromBuffer = importBigNatFromAddr
