module Integer where
import qualified Prim as P
import qualified Ref
import qualified Array.Byte


-- | Construct 'Integer' value from list of 'Int's.
-- This function is used by GHC for constructing 'Integer' literals.
mk ∷ B -- ^ sign of integer ('True' if non-negative)
   → [Int] -- ^ absolute value expressed in 31 bit chunks, least
           --   significant first (ideally these would be
           --   machine-word 'Word's rather than 31-bit truncated 'Int's)
   → I
mk = mkInteger; {-# inline mk #-}

fromI64 ∷ I64 → I
fromI64 = smallInteger
-- | Suitable for hashing
toI64 ∷ I → I64
toI64 = integerToInt
fromU64 ∷ U64 → I
fromU64 = wordToInteger
toU64 ∷ I → U64
toU64 = integerToWord
toF32 ∷ I → F32
toF32 = floatFromInteger
toF64 ∷ I → F64
toF64 = doubleFromInteger


infixl 7 *, /, //, %, %%
infixl 6 +, -
(+), (-), (*), (/), (//), (%), (%%),
  add, sub, mul, div, mod ∷ I → I → I
(+) = plusInteger ; add y x = x + y
(-) = minusInteger; sub y x = x - y
(*) = timesInteger; mul y x = x * y

(/) = divInteger   ; div y x = x / y
(//) = quotInteger ; quot y x = x // y
(%) = modInteger   ; mod y x = x % y
(%%) = remInteger  ; rem y x = x %% y

divMod, quotRem ∷ I → I → (# I , I #)
divMod y x = divModInteger x y; quotRem y x = quotRemInteger x y

negate, abs, signum ∷ I → I
negate = negateInteger 
abs = absInteger
signum = signumInteger

(≡) = eqInteger; eq = (≡)
(≠) = neqInteger; ne = (≠)
(<) = ltInteger; lt = (>)
(>) = gtInteger; gt = (<)
(≤) = leInteger; le = (≥)
(≥) = geInteger; ge = (≤)
cmp ∷ I → I → Ordering
cmp = compareInteger

-- * Bitwise operations

(∧), (∨), (⊕), and, or, xor ∷ I → I → I
(∧) = andInteger; and = andInteger
(∨) = orInteger; or = orInteger
(⊕) = xorInteger; xor = xorInteger

not ∷ I → I
not = complementInteger

shiftL#, shiftR# ∷ I64 → I → I
shiftL# i x = shiftLInteger x i
shiftR# i x = shiftRInteger x i

testBit ∷ I64 → I → B
testBit i x = testBitInteger x i

popCnt ∷ I → I64
popCnt = popCountInteger

bit ∷ I64 → I
bit = bitInteger


valid' ∷ I → P.B
valid' = isValidInteger#

gcd, lcm ∷ I → I → I
gcd = gcdInteger; lcm = lcmInteger
-- | @/a//s/ + /b//t/ = /gcd/@.
gcdExt ∷ I {- ^ a -} → I {- ^ b -} → (# I , I #) {- ^ (gcd , s) -}
gcdExt = gcdExtInteger

sqr ∷ I → I
sqr = sqrInteger

-- | b ^ (e % abs m)
-- Negative exponents are supported if an inverse modulo @/m/@
-- exists.
--
-- __Warning__: It's advised to avoid calling this primitive with
-- negative exponents unless it is guaranteed the inverse exists, as
-- failure to do so will likely cause program abortion due to a
-- divide-by-zero fault. See also 'recipMod'.
--
-- Future versions of @integer_gmp@ may not support negative @/e/@
-- values anymore.
--
powMod# , powModSec# ∷ I {- ^ base -} → I {- ^ exponent -} → I {- ^ modulo -} → I 
powMod# = powModInteger

-- | b ^ (e % m)
-- It is required that @/e/ >= 0@ and @/m/@ is odd.
-- This is a \"secure\" variant of 'powMod#' using the
-- @mpz_powm_sec()@ function which is designed to be resilient to side
-- channel attacks and is therefore intended for cryptographic applications.
powModSec# = powModSecInteger 
{-
#if HAVE_SECURE_POWM == 0
{-# WARNING powModSecInteger "The underlying GMP library does not support a
 - secure version of powModInteger which is side-channel resistant - you need at
 - least GMP version 5 to support this" #-}
#endif
 -}

-- | The inverse of @/x/@ modulo @/m/@.
-- If the inverse exists, the return value @/y/@ will satisfy @0 < /y/ <
-- abs(/m/)@, otherwise the result is @0@.
recipMod ∷ I {- ^ modulus -} → I → I
recipMod m x = recipModInteger x m

-- | Probalistic Miller-Rabin primality test.
--
-- \"@'testPrimeInteger' /n/ /k/@\" determines whether @/n/@ is prime
-- and returns one of the following results:
--
-- * @2#@ is returned if @/n/@ is definitely prime,
--
-- * @1#@ if @/n/@ is a /probable prime/, or
--
-- * @0#@ if @/n/@ is definitely not a prime.
--
-- The @/k/@ argument controls how many test rounds are performed for
-- determining a /probable prime/. For more details, see
-- <http://gmplib.org/manual/Number-Theoretic-Functions.html#index-mpz_005fprobab_005fprime_005fp-360 GMP documentation for `mpz_probab_prime_p()`>.
testPrime ∷ I → I64 → I64
testPrime = testPrimeInteger

-- | Compute next prime greater than @/n/@ probalistically.
--
-- According to the GMP documentation, the underlying function
-- @mpz_nextprime()@ \"uses a probabilistic algorithm to identify
-- primes. For practical purposes it's adequate, the chance of a
-- composite passing will be extremely small.\"
nextPrime ∷ I → I
nextPrime = nextPrimeInteger

sizeInBase ∷ I → I64 → U64
sizeInBase = sizeInBaseInteger

exportToRef ∷ I → Ref.Byte → I64 → IO Word
exportToRef = exportIntegerToAddr

-- | Dump 'Integer' (without sign) to mutable byte-array in base-256
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
exportToBuffer ∷ I → Array.Byte.M (☸) → U64 {- ^ offset -} → P.B {- ^ msfb -} → IO Word {- ^ # bytes written -}
exportToBuffer = exportIntegerToMutableByteArray

importFromRef ∷ Ref.Byte → U64 → I64 → IO I
importFromRef = importIntegerFromAddr
importFromBuffer ∷ Ref.Byte → U64 → I64 → IO I
importFromBuffer = importIntegerFromAddr
