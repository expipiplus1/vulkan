{-# language CPP #-}
-- No documentation found for Chapter "Zero"
module Vulkan.Zero  (Zero(..)) where

import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CInt)
import Foreign.C.Types (CSize)
import Foreign.Storable (Storable)
import Data.Int (Int16)
import Data.Int (Int32)
import Data.Int (Int64)
import Data.Int (Int8)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.TypeNats (KnownNat)
import Data.Word (Word16)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)


    -- | A class for initializing things with all zero data
    --
    -- Any instance should satisfy the following law:
    --
    -- @ new zero = calloc @ or @ with zero = withZeroCStruct @
    --
    -- i.e. Marshaling @zero@ to memory yeilds only zero-valued bytes, except
    -- for structs which require a "type" tag
    --
    class Zero a where
      zero :: a

    instance Zero Bool where
      zero = False

    instance Zero (FunPtr a) where
      zero = nullFunPtr

    instance Zero (Ptr a) where
      zero = nullPtr

    instance Zero Int8 where
      zero = 0

    instance Zero Int16 where
      zero = 0

    instance Zero Int32 where
      zero = 0

    instance Zero Int64 where
      zero = 0

    instance Zero Word8 where
      zero = 0

    instance Zero Word16 where
      zero = 0

    instance Zero Word32 where
      zero = 0

    instance Zero Word64 where
      zero = 0

    instance Zero Float where
      zero = 0

    instance Zero CFloat where
      zero = 0

    instance Zero CChar where
      zero = 0

    instance Zero CSize where
      zero = 0

    instance Zero CInt where
      zero = 0

