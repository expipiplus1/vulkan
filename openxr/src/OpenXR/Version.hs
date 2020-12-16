{-# language CPP #-}
-- No documentation found for Chapter "Version"
module OpenXR.Version  ( pattern CURRENT_API_VERSION
                       , pattern MAKE_VERSION
                       , _VERSION_MAJOR
                       , _VERSION_MINOR
                       , _VERSION_PATCH
                       , Version(..)
                       ) where

import Data.Bits ((.&.))
import Data.Bits ((.|.))
import Data.Bits (shiftL)
import Data.Bits (shiftR)
import OpenXR.Zero (Zero)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import Data.Word (Word16)
import Data.Word (Word32)
import Data.Word (Word64)

pattern CURRENT_API_VERSION :: Version
pattern CURRENT_API_VERSION = MAKE_VERSION 1 0 13


pattern MAKE_VERSION :: Word16 -> Word16 -> Word32 -> Version
pattern MAKE_VERSION major minor patch <-
  (\v -> (_VERSION_MAJOR v, _VERSION_MINOR v, _VERSION_PATCH v) -> (major, minor, patch))
  where MAKE_VERSION major minor patch = Version
                              $ fromIntegral major `shiftL` 48
                            .|. fromIntegral minor `shiftL` 32
                            .|. fromIntegral patch

_VERSION_MAJOR :: Version -> Word16
_VERSION_MAJOR (Version v) = fromIntegral $ (v `shiftR` 48) .&. 0xffff

_VERSION_MINOR :: Version -> Word16
_VERSION_MINOR (Version v) = fromIntegral $ (v `shiftR` 32) .&. 0xffff

_VERSION_PATCH :: Version -> Word32
_VERSION_PATCH (Version v) = fromIntegral $ v .&. 0xffffffff


-- | XrVersion - Type indicating multi-part version packed into 64-bit
-- integer
--
-- = Description
--
-- In each such use, the API major version number, minor version number,
-- and patch version number are packed into a 64-bit integer, referred to
-- as
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrVersion >,
-- as follows:
--
-- == Version Numbers
--
-- -   The major version number is a 16-bit integer packed into bits 63-48.
--
-- -   The minor version number is a 16-bit integer packed into bits 47-32.
--
-- -   The patch version number is a 32-bit integer packed into bits 31-0.
--
-- = See Also
--
-- 'OpenXR.Core10.Instance.ApiLayerProperties',
-- 'OpenXR.Core10.Instance.ApplicationInfo',
-- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.GraphicsRequirementsOpenGLESKHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsRequirementsOpenGLKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsRequirementsVulkanKHR',
-- 'OpenXR.Core10.Instance.InstanceProperties'
newtype Version = Version { unVersion :: Word64 }
  deriving stock (Typeable, Eq, Ord, Show, Read)
  deriving newtype (Storable, Zero)
#if defined(GENERIC_INSTANCES)
deriving instance Generic Version
#endif

