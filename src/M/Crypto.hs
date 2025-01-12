-- | Crypto module
module M.Crypto
  ( -- * AES
    AES,
    Mode (..),
    aesnew,
    aesupdate,

    -- * RSA
    RSA,
    rsanew,
    rsaup,
    rsaspki,

    -- * Hash
    hashservnam,

    -- * General
    Error (..),
    AESClass,

    -- * Re-exports
    ByteString,
  )
where

import Control.Exception hiding (throw)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Unsafe
import Data.Data
import Data.Functor
import Foreign
import Foreign.C
import GHC.Generics

-- exported types

-- | error type
newtype Error = Error String
  deriving stock (Typeable, Data, Generic)
  deriving newtype (Show, Eq)
  -- Typeable: for catching exceptions
  deriving anyclass (Exception)

-- private function: throw an error
throw :: String -> IO a
throw lab = do
  e <-
    dumpallerrors <&> \case
      "" -> "(unknown error/no message)"
      e1 -> e1
  throwIO $ Error $ lab ++ ": " ++ e

-- | encryption/decryption mode
data Mode = Encrypt | Decrypt
  deriving stock (Eq, Show, Read, Typeable, Data, Generic, Enum, Bounded)

-- | encryption/decryption context for 'AES' (AES-128-CFB8)
newtype AES (mode :: Mode) = AES (ForeignPtr EVP_CIPHER_CTX)
  deriving stock (Show)
  deriving newtype (Eq)

-- | RSA pkey
newtype RSA = RSA (ForeignPtr EVP_PKEY)
  deriving stock (Show)
  deriving newtype (Eq)

-- private class, only some methods are exported
class AESClass (mode :: Mode) where
  aesnew_ :: AESCtor
  aesupdate_ :: AESUpdate

  -- | create a new AES context
  aesnew :: ByteString -> IO (AES mode)
  aesnew key | B.length key /= 16 = throw "aesnew: key must be 16 bytes"
  aesnew key = mask_ do
    unsafeUseAsCString key \keyptr -> do
      a <- aesnew_ @mode (castPtr keyptr)
      if a == nullPtr
        then throw "aesnew: failed to create AES context"
        else AES <$> newForeignPtr mmmfreeaescipherctx a

  -- | either encrypt or decrypt a message
  aesupdate :: AES mode -> ByteString -> IO ByteString
  aesupdate (AES ctx) bs = mask_ do
    unsafeUseAsCStringLen bs \(castPtr -> ib, le) ->
      alloca \outlen ->
        withForeignPtr ctx \ctxptr -> do
          let bad = throw "aesupdate: AES update failed"
          ob <- mallocBytes le
          r <- aesupdate_ @mode ctxptr (castPtr ob) outlen ib (fromIntegral le)
          if r == 1
            then do
              n <- peek outlen
              if n == fromIntegral le
                then unsafePackMallocCStringLen (ob, fromIntegral n)
                else bad
            else bad

instance AESClass 'Encrypt where
  aesnew_ = mmmnewaesenc
  aesupdate_ = evpencryptupdate

instance AESClass 'Decrypt where
  aesnew_ = mmmnewaesdec
  aesupdate_ = evpdecryptupdate

-- | create a new RSA pkey
rsanew :: Int -> IO RSA
rsanew bits = mask_ do
  a <- mmmnewrsa (fromIntegral bits)
  if a == nullPtr
    then throw "rsanew: failed to create RSA context"
    else RSA <$> newForeignPtr mmmfreersakey a

-- | write the SubjectPublicKeyInfo to a 'ByteString'
rsaspki :: RSA -> IO ByteString
rsaspki (RSA rsa) = mask_ do
  withForeignPtr rsa \rsaptr -> do
    alloca \out -> do
      r <- mmmwritepubkey rsaptr out
      if r == 1
        then do
          MMMRSAOUT op ol <- peek out
          unsafePackMallocCStringLen (castPtr op, fromIntegral ol)
        else throw "rsaspki: failed to write public key"

class RSAClass (mode :: Mode) where
  rsaup_ :: Ptr EVP_PKEY -> Ptr Word8 -> CSize -> Ptr MMMRSAOUT -> IO CInt

  -- | either encrypt or decrypt a message
  rsaup :: RSA -> ByteString -> IO ByteString
  rsaup (RSA rsa) input = mask_ do
    withForeignPtr rsa \rsaptr ->
      unsafeUseAsCStringLen input \(castPtr -> ip, len) -> do
        alloca \out -> do
          r <- rsaup_ @mode rsaptr (ip) (fromIntegral len) out
          if r == 1
            then do
              -- rsa input/outputs typically have different lengths
              MMMRSAOUT op ol <- peek out
              unsafePackMallocCStringLen (castPtr op, fromIntegral ol)
            else throw "rsaup: RSA operation failed"

instance RSAClass 'Encrypt where
  rsaup_ = mmmrsapubenc

instance RSAClass 'Decrypt where
  rsaup_ = mmmrsaprivdec

-- | generate a SHA1 hash of the server name
hashservnam ::
  -- | server id
  ByteString ->
  -- | shared secret
  ByteString ->
  -- | verify token
  ByteString ->
  -- | SHA1 hash (20 bytes)
  IO ByteString
hashservnam se sh ve = mask_ do
  unsafeUseAsCStringLen se \(castPtr -> sep, fromIntegral -> sel) ->
    unsafeUseAsCStringLen sh \(castPtr -> shp, fromIntegral -> shl) ->
      unsafeUseAsCStringLen ve \(castPtr -> vep, fromIntegral -> vel) -> do
        p <- mmmhashservnam sep sel shp shl vep vel
        case p of
          p1 | p1 == nullPtr -> throw "hashservnam: failed to hash"
          -- SHA1 has 20 bytes
          p1 -> unsafePackMallocCStringLen (castPtr p1, 20)

-- dump all errors to a 'String' and clear the error buffer
dumpallerrors :: IO String
dumpallerrors =
  mmmdumpallerrs >>= \case
    p | p == nullPtr -> pure ""
    p -> peekCString p -- copies the string

-- internal types
data EVP_CIPHER_CTX

data EVP_PKEY

-- represent the C struct @mmmrsaout@
data MMMRSAOUT
  = MMMRSAOUT
      (Ptr Word8) -- output
      CSize -- length

instance Storable MMMRSAOUT where
  sizeOf _ = sizeOf (undefined :: Ptr Word8) + sizeOf (undefined :: CSize)
  alignment _ = alignment (undefined :: Ptr ())
  peek ptr =
    MMMRSAOUT
      <$> (`peekByteOff` 0) ptr
      <*> (`peekByteOff` sizeOf (undefined :: Ptr Word8)) ptr
  poke ptr (MMMRSAOUT out len) = do
    (`pokeByteOff` 0) ptr out
    (`pokeByteOff` sizeOf (undefined :: Ptr Word8)) ptr len

-- AES functions
foreign import ccall unsafe "&mmmfreeaescipherctx"
  mmmfreeaescipherctx :: FinalizerPtr EVP_CIPHER_CTX

type AESCtor = Ptr Word8 -> IO (Ptr EVP_CIPHER_CTX)

foreign import ccall unsafe "mmmnewaesenc"
  mmmnewaesenc :: AESCtor

foreign import ccall unsafe "mmmnewaesdec"
  mmmnewaesdec :: AESCtor

-- https://linux.die.net/man/3/evp_encryptupdate

type AESUpdate =
  Ptr EVP_CIPHER_CTX ->
  Ptr Word8 -> -- output
  Ptr CInt ->
  Ptr Word8 -> -- input
  CInt ->
  IO CInt -- 1 = success, 0 = failure

foreign import ccall unsafe "EVP_EncryptUpdate"
  evpencryptupdate :: AESUpdate

foreign import ccall unsafe "EVP_DecryptUpdate"
  evpdecryptupdate :: AESUpdate

foreign import ccall unsafe "&mmmfreersakey"
  mmmfreersakey :: FinalizerPtr EVP_PKEY

foreign import ccall unsafe "mmmnewrsa"
  mmmnewrsa :: CInt -> IO (Ptr EVP_PKEY)

foreign import ccall unsafe "mmmrsapubenc"
  mmmrsapubenc :: Ptr EVP_PKEY -> Ptr Word8 -> CSize -> Ptr MMMRSAOUT -> IO CInt

foreign import ccall unsafe "mmmrsaprivdec"
  mmmrsaprivdec ::
    Ptr EVP_PKEY ->
    Ptr Word8 ->
    CSize ->
    Ptr MMMRSAOUT ->
    IO CInt

foreign import ccall unsafe "mmmwritepubkey"
  mmmwritepubkey :: Ptr EVP_PKEY -> Ptr MMMRSAOUT -> IO CInt

-- Hash function
foreign import ccall unsafe "mmmhashservnam"
  mmmhashservnam ::
    Ptr Word8 ->
    CSize ->
    Ptr Word8 ->
    CSize ->
    Ptr Word8 ->
    CSize ->
    IO (Ptr Word8)

foreign import ccall unsafe "mmmdumpallerrs"
  mmmdumpallerrs :: IO CString
