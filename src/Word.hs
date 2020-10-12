module Word where
import qualified Prim as P
import qualified Ref
import qualified Array.Byte

gcd ∷ U64 → U64 → U64
gcd = gcdWord

-- | b ^ (e % m)
powMod# ∷ U64 {- ^ base -} → U64 {- ^ exponent -} → U64 {- ^ modulo -} → U64 
powMod# = powModWord

-- | The inverse of @/x/@ modulo @/m/@.
-- If the inverse exists, the return value @/y/@ will satisfy @0 < /y/ <
-- abs(/m/)@, otherwise the result is @0@.
recipMod ∷ U64 {- ^ modulus -} → U64 → U64
recipMod m x = recipModWord x m


sizeInBase ∷ U64 → I64 → U64
sizeInBase = sizeInBaseWord# 

exportToRef ∷ Word → Ref.Byte → I64 → IO Word
exportToRef = exportWordToAddr

-- | Dump 'U64' to mutable byte-array in base-256
-- representation.
--
-- Use \"@'sizeInBase' /i/ 256#@\" to compute the exact number of
-- bytes written in advance for @/i/ /= 0@. In case of @/i/ == 0@,
-- 'exportToBuffer' will write and report zero bytes
-- written, whereas 'sizeInBase' report one byte.
--
-- It's recommended to avoid calling 'exportToBuffer' for small
-- integers as this function would currently convert those to big
-- integers in msbf to call @mpz_export()@.
exportToBuffer ∷ Word → Array.Byte.M (☸) → U64 {- ^ offset -} → P.B {- ^ msfb -} → IO Word {- ^ # bytes written -}
exportToBuffer = exportWordToMutableByteArray
