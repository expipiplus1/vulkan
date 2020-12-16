{-# LANGUAGE CPP #-}

module OpenXR.VulkanTypes
  ( Instance_T
  , PhysicalDevice_T
  , Device_T
  , Image(..)
  , Result(..)
  , Format(..)
  , InstanceCreateInfo
  , DeviceCreateInfo
  , AllocationCallbacks
  , PFN_vkGetInstanceProcAddr
  , SomeStruct(..)
  ) where

#if defined(USE_VULKAN_TYPES)

import           Vulkan
import           Vulkan.CStruct.Extends
import           Foreign.C.String
import           Foreign.Ptr

type PFN_vkGetInstanceProcAddr = FunPtr (CString -> IO (FunPtr ()))

#else

import           Data.Kind
import           Data.Word
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Storable
import           OpenXR.Zero

data Instance_T

data PhysicalDevice_T

data Device_T

newtype Image = Image Word64
  deriving stock (Eq, Show)
  deriving newtype (Zero, Storable)

newtype Result = Result Word32
  deriving stock (Eq, Show)
  deriving newtype (Zero, Storable)

newtype Format = Format Word32
  deriving stock (Eq, Show)
  deriving newtype (Zero, Storable)

data InstanceCreateInfo es
data DeviceCreateInfo es

data AllocationCallbacks

type PFN_vkGetInstanceProcAddr = FunPtr (CString -> IO (FunPtr ()))

data SomeStruct (a :: [Type] -> Type) where
  SomeStruct :: forall a es. a es -> SomeStruct a

#endif
