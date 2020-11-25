{-# language CPP #-}
-- | = Name
--
-- VK_EXT_display_surface_counter - instance extension
--
-- == VK_EXT_display_surface_counter
--
-- [__Name String__]
--     @VK_EXT_display_surface_counter@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     91
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_display@
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_display_surface_counter:%20&body=@cubanismo%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Pierre Boudier, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   Damien Leone, NVIDIA
--
--     -   Pierre-Loup Griffais, Valve
--
--     -   Daniel Vetter, Intel
--
-- == Description
--
-- This extension defines a vertical blanking period counter associated
-- with display surfaces. It provides a mechanism to query support for such
-- a counter from a 'Vulkan.Extensions.Handles.SurfaceKHR' object.
--
-- == New Commands
--
-- -   'getPhysicalDeviceSurfaceCapabilities2EXT'
--
-- == New Structures
--
-- -   'SurfaceCapabilities2EXT'
--
-- == New Enums
--
-- -   'SurfaceCounterFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'SurfaceCounterFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME'
--
-- -   'EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT'
--
-- == Version History
--
-- -   Revision 1, 2016-12-13 (James Jones)
--
--     -   Initial draft
--
-- = See Also
--
-- 'SurfaceCapabilities2EXT', 'SurfaceCounterFlagBitsEXT',
-- 'SurfaceCounterFlagsEXT', 'getPhysicalDeviceSurfaceCapabilities2EXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_display_surface_counter Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_display_surface_counter  ( getPhysicalDeviceSurfaceCapabilities2EXT
                                                         , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
                                                         , pattern SURFACE_COUNTER_VBLANK_EXT
                                                         , SurfaceCapabilities2EXT(..)
                                                         , SurfaceCounterFlagsEXT
                                                         , SurfaceCounterFlagBitsEXT( SURFACE_COUNTER_VBLANK_BIT_EXT
                                                                                    , ..
                                                                                    )
                                                         , EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
                                                         , pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
                                                         , EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
                                                         , pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
                                                         , SurfaceKHR(..)
                                                         , CompositeAlphaFlagBitsKHR(..)
                                                         , CompositeAlphaFlagsKHR
                                                         , SurfaceTransformFlagBitsKHR(..)
                                                         , SurfaceTransformFlagsKHR
                                                         ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (asum)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base ((<$))
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagsKHR)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceCapabilities2EXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagsKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilities2EXT
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilities2EXT -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilities2EXT -> IO Result

-- | vkGetPhysicalDeviceSurfaceCapabilities2EXT - Query surface capabilities
--
-- = Description
--
-- 'getPhysicalDeviceSurfaceCapabilities2EXT' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
-- with the ability to return extended information by adding extending
-- structures to the @pNext@ chain of its @pSurfaceCapabilities@ parameter.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2EXT-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2EXT-surface-parameter#
--     @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2EXT-pSurfaceCapabilities-parameter#
--     @pSurfaceCapabilities@ /must/ be a valid pointer to a
--     'SurfaceCapabilities2EXT' structure
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2EXT-commonparent# Both
--     of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'SurfaceCapabilities2EXT',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfaceCapabilities2EXT :: forall io
                                          . (MonadIO io)
                                         => -- | @physicalDevice@ is the physical device that will be associated with the
                                            -- swapchain to be created, as described for
                                            -- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                            PhysicalDevice
                                         -> -- | @surface@ is the surface that will be associated with the swapchain.
                                            SurfaceKHR
                                         -> io (SurfaceCapabilities2EXT)
getPhysicalDeviceSurfaceCapabilities2EXT physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceCapabilities2EXTPtr = pVkGetPhysicalDeviceSurfaceCapabilities2EXT (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceCapabilities2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceCapabilities2EXT is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceCapabilities2EXT' = mkVkGetPhysicalDeviceSurfaceCapabilities2EXT vkGetPhysicalDeviceSurfaceCapabilities2EXTPtr
  pPSurfaceCapabilities <- ContT (withZeroCStruct @SurfaceCapabilities2EXT)
  r <- lift $ vkGetPhysicalDeviceSurfaceCapabilities2EXT' (physicalDeviceHandle (physicalDevice)) (surface) (pPSurfaceCapabilities)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceCapabilities <- lift $ peekCStruct @SurfaceCapabilities2EXT pPSurfaceCapabilities
  pure $ (pSurfaceCapabilities)


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT"
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT = STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT


-- No documentation found for TopLevel "VK_SURFACE_COUNTER_VBLANK_EXT"
pattern SURFACE_COUNTER_VBLANK_EXT = SURFACE_COUNTER_VBLANK_BIT_EXT


-- | VkSurfaceCapabilities2EXT - Structure describing capabilities of a
-- surface
--
-- = Members
--
-- All members of 'SurfaceCapabilities2EXT' are identical to the
-- corresponding members of
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' where one
-- exists. The remaining members are:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_surface.CompositeAlphaFlagsKHR',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'SurfaceCounterFlagsEXT',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagsKHR',
-- 'getPhysicalDeviceSurfaceCapabilities2EXT'
data SurfaceCapabilities2EXT = SurfaceCapabilities2EXT
  { -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "minImageCount"
    minImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "maxImageCount"
    maxImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "currentExtent"
    currentExtent :: Extent2D
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "minImageExtent"
    minImageExtent :: Extent2D
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "maxImageExtent"
    maxImageExtent :: Extent2D
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "maxImageArrayLayers"
    maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedTransforms"
    supportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "currentTransform"
    currentTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedCompositeAlpha"
    supportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedUsageFlags"
    supportedUsageFlags :: ImageUsageFlags
  , -- | @supportedSurfaceCounters@ is a bitmask of 'SurfaceCounterFlagBitsEXT'
    -- indicating the supported surface counter types.
    --
    -- #VUID-VkSurfaceCapabilities2EXT-supportedSurfaceCounters-01246#
    -- @supportedSurfaceCounters@ /must/ not include
    -- 'SURFACE_COUNTER_VBLANK_BIT_EXT' unless the surface queried is a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#wsi-display-surfaces display surface>
    supportedSurfaceCounters :: SurfaceCounterFlagsEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilities2EXT)
#endif
deriving instance Show SurfaceCapabilities2EXT

instance ToCStruct SurfaceCapabilities2EXT where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilities2EXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (minImageCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxImageCount)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (currentExtent)
    poke ((p `plusPtr` 32 :: Ptr Extent2D)) (minImageExtent)
    poke ((p `plusPtr` 40 :: Ptr Extent2D)) (maxImageExtent)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxImageArrayLayers)
    poke ((p `plusPtr` 52 :: Ptr SurfaceTransformFlagsKHR)) (supportedTransforms)
    poke ((p `plusPtr` 56 :: Ptr SurfaceTransformFlagBitsKHR)) (currentTransform)
    poke ((p `plusPtr` 60 :: Ptr CompositeAlphaFlagsKHR)) (supportedCompositeAlpha)
    poke ((p `plusPtr` 64 :: Ptr ImageUsageFlags)) (supportedUsageFlags)
    poke ((p `plusPtr` 68 :: Ptr SurfaceCounterFlagsEXT)) (supportedSurfaceCounters)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    f

instance FromCStruct SurfaceCapabilities2EXT where
  peekCStruct p = do
    minImageCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxImageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    currentExtent <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    minImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 32 :: Ptr Extent2D))
    maxImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 40 :: Ptr Extent2D))
    maxImageArrayLayers <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    supportedTransforms <- peek @SurfaceTransformFlagsKHR ((p `plusPtr` 52 :: Ptr SurfaceTransformFlagsKHR))
    currentTransform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 56 :: Ptr SurfaceTransformFlagBitsKHR))
    supportedCompositeAlpha <- peek @CompositeAlphaFlagsKHR ((p `plusPtr` 60 :: Ptr CompositeAlphaFlagsKHR))
    supportedUsageFlags <- peek @ImageUsageFlags ((p `plusPtr` 64 :: Ptr ImageUsageFlags))
    supportedSurfaceCounters <- peek @SurfaceCounterFlagsEXT ((p `plusPtr` 68 :: Ptr SurfaceCounterFlagsEXT))
    pure $ SurfaceCapabilities2EXT
             minImageCount maxImageCount currentExtent minImageExtent maxImageExtent maxImageArrayLayers supportedTransforms currentTransform supportedCompositeAlpha supportedUsageFlags supportedSurfaceCounters

instance Storable SurfaceCapabilities2EXT where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceCapabilities2EXT where
  zero = SurfaceCapabilities2EXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT

-- | VkSurfaceCounterFlagBitsEXT - Surface-relative counter types
--
-- = See Also
--
-- 'SurfaceCounterFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_display_control.getSwapchainCounterEXT'
newtype SurfaceCounterFlagBitsEXT = SurfaceCounterFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SURFACE_COUNTER_VBLANK_BIT_EXT' specifies a counter incrementing once
-- every time a vertical blanking period occurs on the display associated
-- with the surface.
pattern SURFACE_COUNTER_VBLANK_BIT_EXT = SurfaceCounterFlagBitsEXT 0x00000001

conNameSurfaceCounterFlagBitsEXT :: String
conNameSurfaceCounterFlagBitsEXT = "SurfaceCounterFlagBitsEXT"

enumPrefixSurfaceCounterFlagBitsEXT :: String
enumPrefixSurfaceCounterFlagBitsEXT = "SURFACE_COUNTER_VBLANK_BIT_EXT"

showTableSurfaceCounterFlagBitsEXT :: [(SurfaceCounterFlagBitsEXT, String)]
showTableSurfaceCounterFlagBitsEXT = [(SURFACE_COUNTER_VBLANK_BIT_EXT, "")]

instance Show SurfaceCounterFlagBitsEXT where
  showsPrec p e = case lookup e showTableSurfaceCounterFlagBitsEXT of
    Just s -> showString enumPrefixSurfaceCounterFlagBitsEXT . showString s
    Nothing ->
      let SurfaceCounterFlagBitsEXT x = e
      in  showParen (p >= 11) (showString conNameSurfaceCounterFlagBitsEXT . showString " 0x" . showHex x)

instance Read SurfaceCounterFlagBitsEXT where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixSurfaceCounterFlagBitsEXT
          asum ((\(e, s) -> e <$ string s) <$> showTableSurfaceCounterFlagBitsEXT)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameSurfaceCounterFlagBitsEXT)
            v <- step readPrec
            pure (SurfaceCounterFlagBitsEXT v)
          )
    )


type EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION"
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1


type EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = "VK_EXT_display_surface_counter"

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME"
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = "VK_EXT_display_surface_counter"

