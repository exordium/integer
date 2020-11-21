module Word where
import qualified Ref.Byte as Byte
import qualified Array.Byte

gcd ∷ U → U → U
gcd = gcdWord

-- | b ^ (e % m)
powMod# ∷ U {- ^ base -} → U {- ^ exponent -} → U {- ^ modulo -} → U 
powMod# = powModWord

-- | The inverse of @/x/@ modulo @/m/@.
-- If the inverse exists, the return value @/y/@ will satisfy @0 < /y/ <
-- abs(/m/)@, otherwise the result is @0@.
recipMod ∷ U {- ^ modulus -} → U → U
recipMod m x = recipModWord x m


sizeInBase ∷ U → I → U
sizeInBase = sizeInBaseWord# 

exportToRef ∷ Word → Byte.Ref → I → IO Word
exportToRef = exportWordToAddr

-- | Dump 'U' to mutable byte-array in base-256
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
exportToBuffer ∷ Word → Array.Byte.M (☸) → U {- ^ offset -} → B# {- ^ msfb -} → IO Word {- ^ # bytes written -}
exportToBuffer = exportWordToMutableByteArray
