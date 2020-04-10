{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter  ( getPhysicalDeviceSurfaceCapabilities2EXT
                                                                  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
                                                                  , SurfaceCapabilities2EXT(..)
                                                                  , CompositeAlphaFlagBitsKHR( COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                                                                                             , COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
                                                                                             , COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
                                                                                             , COMPOSITE_ALPHA_INHERIT_BIT_KHR
                                                                                             , ..
                                                                                             )
                                                                  , CompositeAlphaFlagsKHR
                                                                  , SurfaceCounterFlagBitsEXT( SURFACE_COUNTER_VBLANK_EXT
                                                                                             , ..
                                                                                             )
                                                                  , SurfaceCounterFlagsEXT
                                                                  , EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
                                                                  , pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
                                                                  , EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
                                                                  , pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
                                                                  , SurfaceKHR(..)
                                                                  , SurfaceTransformFlagBitsKHR(..)
                                                                  , SurfaceTransformFlagsKHR
                                                                  ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.Core10.SharedTypes (Extent2D)
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceCapabilities2EXT))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagBitsKHR)
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagsKHR)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagBitsKHR(..))
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilities2EXT
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilities2EXT -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilities2EXT -> IO Result

-- | vkGetPhysicalDeviceSurfaceCapabilities2EXT - Query surface capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pSurfaceCapabilities@ is a pointer to a 'SurfaceCapabilities2EXT'
--     structure in which the capabilities are returned.
--
-- = Description
--
-- 'getPhysicalDeviceSurfaceCapabilities2EXT' behaves similarly to
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
-- with the ability to return extended information by adding extension
-- structures to the @pNext@ chain of its @pSurfaceCapabilities@ parameter.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   @pSurfaceCapabilities@ /must/ be a valid pointer to a
--     'SurfaceCapabilities2EXT' structure
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Instance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice',
-- 'SurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfaceCapabilities2EXT :: forall io . MonadIO io => PhysicalDevice -> SurfaceKHR -> io (SurfaceCapabilities2EXT)
getPhysicalDeviceSurfaceCapabilities2EXT physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceCapabilities2EXT' = mkVkGetPhysicalDeviceSurfaceCapabilities2EXT (pVkGetPhysicalDeviceSurfaceCapabilities2EXT (instanceCmds (physicalDevice :: PhysicalDevice)))
  pPSurfaceCapabilities <- ContT (withZeroCStruct @SurfaceCapabilities2EXT)
  r <- lift $ vkGetPhysicalDeviceSurfaceCapabilities2EXT' (physicalDeviceHandle (physicalDevice)) (surface) (pPSurfaceCapabilities)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceCapabilities <- lift $ peekCStruct @SurfaceCapabilities2EXT pPSurfaceCapabilities
  pure $ (pSurfaceCapabilities)


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT"
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT = STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT


-- | VkSurfaceCapabilities2EXT - Structure describing capabilities of a
-- surface
--
-- = Members
--
-- All members of 'SurfaceCapabilities2EXT' are identical to the
-- corresponding members of
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' where
-- one exists. The remaining members are:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'CompositeAlphaFlagsKHR', 'Graphics.Vulkan.Core10.SharedTypes.Extent2D',
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'SurfaceCounterFlagsEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.SurfaceTransformFlagBitsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.SurfaceTransformFlagsKHR',
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
  , -- | @supportedSurfaceCounters@ /must/ not include
    -- 'SURFACE_COUNTER_VBLANK_EXT' unless the surface queried is a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#wsi-display-surfaces display surface>.
    supportedSurfaceCounters :: SurfaceCounterFlagsEXT
  }
  deriving (Typeable)
deriving instance Show SurfaceCapabilities2EXT

instance ToCStruct SurfaceCapabilities2EXT where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilities2EXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (minImageCount)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (maxImageCount)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr Extent2D)) (currentExtent) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr Extent2D)) (minImageExtent) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr Extent2D)) (maxImageExtent) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxImageArrayLayers)
    lift $ poke ((p `plusPtr` 52 :: Ptr SurfaceTransformFlagsKHR)) (supportedTransforms)
    lift $ poke ((p `plusPtr` 56 :: Ptr SurfaceTransformFlagBitsKHR)) (currentTransform)
    lift $ poke ((p `plusPtr` 60 :: Ptr CompositeAlphaFlagsKHR)) (supportedCompositeAlpha)
    lift $ poke ((p `plusPtr` 64 :: Ptr ImageUsageFlags)) (supportedUsageFlags)
    lift $ poke ((p `plusPtr` 68 :: Ptr SurfaceCounterFlagsEXT)) (supportedSurfaceCounters)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr Extent2D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr Extent2D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    lift $ f

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


-- | VkCompositeAlphaFlagBitsKHR - alpha compositing modes supported on a
-- device
--
-- = Description
--
-- These values are described as follows:
--
-- = See Also
--
-- 'CompositeAlphaFlagsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
newtype CompositeAlphaFlagBitsKHR = CompositeAlphaFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'COMPOSITE_ALPHA_OPAQUE_BIT_KHR': The alpha channel, if it exists, of
-- the images is ignored in the compositing process. Instead, the image is
-- treated as if it has a constant alpha of 1.0.
pattern COMPOSITE_ALPHA_OPAQUE_BIT_KHR = CompositeAlphaFlagBitsKHR 0x00000001
-- | 'COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR': The alpha channel, if it
-- exists, of the images is respected in the compositing process. The
-- non-alpha channels of the image are expected to already be multiplied by
-- the alpha channel by the application.
pattern COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = CompositeAlphaFlagBitsKHR 0x00000002
-- | 'COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR': The alpha channel, if it
-- exists, of the images is respected in the compositing process. The
-- non-alpha channels of the image are not expected to already be
-- multiplied by the alpha channel by the application; instead, the
-- compositor will multiply the non-alpha channels of the image by the
-- alpha channel during compositing.
pattern COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = CompositeAlphaFlagBitsKHR 0x00000004
-- | 'COMPOSITE_ALPHA_INHERIT_BIT_KHR': The way in which the presentation
-- engine treats the alpha channel in the images is unknown to the Vulkan
-- API. Instead, the application is responsible for setting the composite
-- alpha blending mode using native window system commands. If the
-- application does not set the blending mode using native window system
-- commands, then a platform-specific default will be used.
pattern COMPOSITE_ALPHA_INHERIT_BIT_KHR = CompositeAlphaFlagBitsKHR 0x00000008

type CompositeAlphaFlagsKHR = CompositeAlphaFlagBitsKHR

instance Show CompositeAlphaFlagBitsKHR where
  showsPrec p = \case
    COMPOSITE_ALPHA_OPAQUE_BIT_KHR -> showString "COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
    COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR -> showString "COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
    COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR -> showString "COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
    COMPOSITE_ALPHA_INHERIT_BIT_KHR -> showString "COMPOSITE_ALPHA_INHERIT_BIT_KHR"
    CompositeAlphaFlagBitsKHR x -> showParen (p >= 11) (showString "CompositeAlphaFlagBitsKHR 0x" . showHex x)

instance Read CompositeAlphaFlagBitsKHR where
  readPrec = parens (choose [("COMPOSITE_ALPHA_OPAQUE_BIT_KHR", pure COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
                            , ("COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR", pure COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR)
                            , ("COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR", pure COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR)
                            , ("COMPOSITE_ALPHA_INHERIT_BIT_KHR", pure COMPOSITE_ALPHA_INHERIT_BIT_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "CompositeAlphaFlagBitsKHR")
                       v <- step readPrec
                       pure (CompositeAlphaFlagBitsKHR v)))


-- | VkSurfaceCounterFlagBitsEXT - Surface-relative counter types
--
-- = See Also
--
-- 'SurfaceCounterFlagsEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.getSwapchainCounterEXT'
newtype SurfaceCounterFlagBitsEXT = SurfaceCounterFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'SURFACE_COUNTER_VBLANK_EXT' specifies a counter incrementing once every
-- time a vertical blanking period occurs on the display associated with
-- the surface.
pattern SURFACE_COUNTER_VBLANK_EXT = SurfaceCounterFlagBitsEXT 0x00000001

type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT

instance Show SurfaceCounterFlagBitsEXT where
  showsPrec p = \case
    SURFACE_COUNTER_VBLANK_EXT -> showString "SURFACE_COUNTER_VBLANK_EXT"
    SurfaceCounterFlagBitsEXT x -> showParen (p >= 11) (showString "SurfaceCounterFlagBitsEXT 0x" . showHex x)

instance Read SurfaceCounterFlagBitsEXT where
  readPrec = parens (choose [("SURFACE_COUNTER_VBLANK_EXT", pure SURFACE_COUNTER_VBLANK_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "SurfaceCounterFlagBitsEXT")
                       v <- step readPrec
                       pure (SurfaceCounterFlagBitsEXT v)))


type EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION"
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1


type EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = "VK_EXT_display_surface_counter"

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME"
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = "VK_EXT_display_surface_counter"

