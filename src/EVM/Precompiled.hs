{-# Language ForeignFunctionInterface #-}

module EVM.Precompiled (execute) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe

-- | Opaque representation of the C library's context struct.
data EthjetContext

foreign import ccall "ethjet_init"
  ethjet_init :: IO (Ptr EthjetContext)
foreign import ccall "&ethjet_free"
  ethjet_free :: FunPtr (Ptr EthjetContext -> IO ())
foreign import ccall "ethjet"
  ethjet
    :: Ptr EthjetContext     -- initialized context
    -> CInt                  -- operation
    -> Ptr CChar -> CInt     -- input
    -> Ptr CChar -> CInt     -- output
    -> IO CInt               -- 1 if good

-- Lazy evaluation ensures this context is only initialized once,
-- and `unsafePerformIO` in such situations is a common pattern.
--
-- We use a "foreign pointer" annotated with a finalizer.
globalContext :: ForeignPtr EthjetContext
{-# NOINLINE globalContext #-}
globalContext =
  unsafePerformIO $
    ethjet_init >>= newForeignPtr ethjet_free

-- | Run a given precompiled contract using the C library.
execute
  :: Int                -- ^ The number of the precompiled contract
  -> ByteString         -- ^ The input buffer
  -> Int                -- ^ The desired output size
  -> Maybe ByteString   -- ^ Hopefully, the output buffer
execute contract input outputSize =

  -- This code looks messy because of the pointer handling,
  -- but it's actually simple.
  --
  -- We use `unsafePerformIO` because the contracts are pure.

  unsafePerformIO . BS.useAsCStringLen input $
    \(inputPtr, inputSize) -> do
       outputForeignPtr <- mallocForeignPtrBytes outputSize
       withForeignPtr outputForeignPtr $ \outputPtr -> do
         status <-
           withForeignPtr globalContext $ \contextPtr ->
             -- Finally, we can invoke the C library.
             ethjet contextPtr (fromIntegral contract)
               inputPtr (fromIntegral inputSize)
               outputPtr (fromIntegral outputSize)

         case status of
           1 -> Just <$> BS.packCStringLen (outputPtr, outputSize)
           _ -> pure Nothing
