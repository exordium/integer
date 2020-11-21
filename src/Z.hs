module Z (Z,  module Z) where
import qualified Ref.Byte as Byte
import qualified Array.Byte


-- | Construct 'Z' value from list of 'Int's.
-- This function is used by GHC for constructing 'Integer' literals.
mk ∷ B -- ^ sign of integer ('True' if non-negative)
   → [Int] -- ^ absolute value expressed in 31 bit chunks, least
           --   significant first (ideally these would be
           --   machine-word 'Word's rather than 31-bit truncated 'Int's)
   → Z
mk = mkInteger; {-# inline mk #-}

fromI ∷ I → Z
fromI = smallInteger
-- | Suitable for hashing
toI ∷ Z → I
toI = integerToInt
fromU ∷ U → Z 
fromU = wordToInteger
toU ∷ Z → U
toU = integerToWord
toF32 ∷ Z → F32
toF32 = floatFromInteger
toF64 ∷ Z → F64
toF64 = doubleFromInteger


infixl 7 *, /, //, %, %%
infixl 6 +, -
(+), (-), (*), (/), (//), (%), (%%),
  add, sub, mul, div, mod ∷ Z → Z → Z
(+) = plusInteger ; add y x = x + y
(-) = minusInteger; sub y x = x - y
(*) = timesInteger; mul y x = x * y
(/) = divInteger   ; div y x = x / y
(//) = quotInteger ; quot y x = x // y
(%) = modInteger   ; mod y x = x % y
(%%) = remInteger  ; rem y x = x %% y

divMod, quotRem ∷ Z → Z → (# Z , Z #)
divMod y x = divModInteger x y; quotRem y x = quotRemInteger x y

negate, abs, signum ∷ Z → Z
negate = negateInteger 
abs = absInteger
signum = signumInteger

(≡) = eqInteger; eq = (≡)
(≠) = neqInteger; ne = (≠)
(<) = ltInteger; lt = (>)
(>) = gtInteger; gt = (<)
(≤) = leInteger; le = (≥)
(≥) = geInteger; ge = (≤)
cmp ∷ Z → Z → Ordering
cmp = compareInteger

-- * Bitwise operations

(∧), (∨), (⊕), and, or, xor ∷ Z → Z → Z
(∧) = andInteger; and = andInteger
(∨) = orInteger; or = orInteger
(⊕) = xorInteger; xor = xorInteger

not ∷ Z → Z
not = complementInteger

shiftL#, shiftR# ∷ I → Z → Z
shiftL# i x = shiftLInteger x i
shiftR# i x = shiftRInteger x i

testBit ∷ I → Z → B
testBit i x = testBitInteger x i

popCnt ∷ Z → I
popCnt = popCountInteger

bit ∷ I → Z
bit = bitInteger


valid' ∷ Z → B#
valid' = isValidInteger#

gcd, lcm ∷ Z → Z → Z
gcd = gcdInteger; lcm = lcmInteger
-- | @/a//s/ + /b//t/ = /gcd/@.
gcdExt ∷ Z {- ^ a -} → Z {- ^ b -} → (# Z , Z #) {- ^ (gcd , s) -}
gcdExt = gcdExtInteger

sqr ∷ Z → Z
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
powMod# , powModSec# ∷ Z {- ^ base -} → Z {- ^ exponent -} → Z {- ^ modulo -} → Z 
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
recipMod ∷ Z {- ^ modulus -} → Z → Z
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
testPrime ∷ Z → I → I
testPrime = testPrimeInteger

-- | Compute next prime greater than @/n/@ probalistically.
--
-- According to the GMP documentation, the underlying function
-- @mpz_nextprime()@ \"uses a probabilistic algorithm to identify
-- primes. For practical purposes it's adequate, the chance of a
-- composite passing will be extremely small.\"
nextPrime ∷ Z → Z
nextPrime = nextPrimeInteger

sizeInBase ∷ Z → I → U
sizeInBase = sizeInBaseInteger

exportToRef ∷ Z → Byte.Ref → I → IO Word
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
exportToBuffer ∷ Z → Array.Byte.M (☸) → U {- ^ offset -} → B# {- ^ msfb -} → IO Word {- ^ # bytes written -}
exportToBuffer = exportIntegerToMutableByteArray

importFromRef ∷ Byte.Ref → U → I → IO Z
importFromRef = importIntegerFromAddr
importFromBuffer ∷ Byte.Ref → U → I → IO Z
importFromBuffer = importIntegerFromAddr
