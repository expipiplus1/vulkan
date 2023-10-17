{-# language CPP #-}
-- | = Name
--
-- VK_NV_representative_fragment_test - device extension
--
-- == VK_NV_representative_fragment_test
--
-- [__Name String__]
--     @VK_NV_representative_fragment_test@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     167
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Kedarnath Thangudu
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_representative_fragment_test] @kthangudu%0A*Here describe the issue or question you have about the VK_NV_representative_fragment_test extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-09-13
--
-- [__Contributors__]
--
--     -   Kedarnath Thangudu, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Pierre Boudier, NVIDIA
--
--     -   Pat Brown, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
-- == Description
--
-- This extension provides a new representative fragment test that allows
-- implementations to reduce the amount of rasterization and fragment
-- processing work performed for each point, line, or triangle primitive.
-- For any primitive that produces one or more fragments that pass all
-- other early fragment tests, the implementation is permitted to choose
-- one or more “representative” fragments for processing and discard all
-- other fragments. For draw calls rendering multiple points, lines, or
-- triangles arranged in lists, strips, or fans, the representative
-- fragment test is performed independently for each of those primitives.
--
-- This extension is useful for applications that use an early render pass
-- to determine the full set of primitives that would be visible in the
-- final scene. In this render pass, such applications would set up a
-- fragment shader that enables early fragment tests and writes to an image
-- or shader storage buffer to record the ID of the primitive that
-- generated the fragment. Without this extension, the shader would record
-- the ID separately for each visible fragment of each primitive. With this
-- extension, fewer stores will be performed, particularly for large
-- primitives.
--
-- The representative fragment test has no effect if early fragment tests
-- are not enabled via the fragment shader. The set of fragments discarded
-- by the representative fragment test is implementation-dependent and may
-- vary from frame to frame. In some cases, the representative fragment
-- test may not discard any fragments for a given primitive.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineRepresentativeFragmentTestStateCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRepresentativeFragmentTestFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME'
--
-- -   'NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- (1) Is the representative fragment test guaranteed to have any effect?
--
-- __RESOLVED__: No. As specified, we only guarantee that each primitive
-- with at least one fragment that passes prior tests will have one
-- fragment passing the representative fragment tests. We do not guarantee
-- that any particular fragment will fail the test.
--
-- In the initial implementation of this extension, the representative
-- fragment test is treated as an optimization that may be completely
-- disabled for some pipeline states. This feature was designed for a use
-- case where the fragment shader records information on individual
-- primitives using shader storage buffers or storage images, with no
-- writes to color or depth buffers.
--
-- (2) Will the set of fragments that pass the representative fragment test
-- be repeatable if you draw the same scene over and over again?
--
-- __RESOLVED__: No. The set of fragments that pass the representative
-- fragment test is implementation-dependent and may vary due to the timing
-- of operations performed by the GPU.
--
-- (3) What happens if you enable the representative fragment test with
-- writes to color and\/or depth render targets enabled?
--
-- __RESOLVED__: If writes to the color or depth buffer are enabled, they
-- will be performed for any fragments that survive the relevant tests. Any
-- fragments that fail the representative fragment test will not update
-- color buffers. For the use cases intended for this feature, we do not
-- expect color or depth writes to be enabled.
--
-- (4) How do derivatives and automatic texture LOD computations work with
-- the representative fragment test enabled?
--
-- __RESOLVED__: If a fragment shader uses derivative functions or texture
-- lookups using automatic LOD computation, derivatives will be computed
-- identically whether or not the representative fragment test is enabled.
-- For the use cases intended for this feature, we do not expect the use of
-- derivatives in the fragment shader.
--
-- == Version History
--
-- -   Revision 2, 2018-09-13 (pbrown)
--
--     -   Add issues.
--
-- -   Revision 1, 2018-08-22 (Kedarnath Thangudu)
--
--     -   Internal Revisions
--
-- == See Also
--
-- 'PhysicalDeviceRepresentativeFragmentTestFeaturesNV',
-- 'PipelineRepresentativeFragmentTestStateCreateInfoNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_representative_fragment_test Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_representative_fragment_test  ( PhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
                                                             , PipelineRepresentativeFragmentTestStateCreateInfoNV(..)
                                                             , NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
                                                             , pattern NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
                                                             , NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME
                                                             , pattern NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME
                                                             ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV))
-- | VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV - Structure
-- describing the representative fragment test features that can be
-- supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceRepresentativeFragmentTestFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRepresentativeFragmentTestFeaturesNV' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_representative_fragment_test VK_NV_representative_fragment_test>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRepresentativeFragmentTestFeaturesNV = PhysicalDeviceRepresentativeFragmentTestFeaturesNV
  { -- | #features-representativeFragmentTest# @representativeFragmentTest@
    -- indicates whether the implementation supports the representative
    -- fragment test. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-rep-frag-test Representative Fragment Test>.
    representativeFragmentTest :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
#endif
deriving instance Show PhysicalDeviceRepresentativeFragmentTestFeaturesNV

instance ToCStruct PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRepresentativeFragmentTestFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (representativeFragmentTest))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  peekCStruct p = do
    representativeFragmentTest <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRepresentativeFragmentTestFeaturesNV
             (bool32ToBool representativeFragmentTest)

instance Storable PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  zero = PhysicalDeviceRepresentativeFragmentTestFeaturesNV
           zero


-- | VkPipelineRepresentativeFragmentTestStateCreateInfoNV - Structure
-- specifying representative fragment test
--
-- = Description
--
-- If this structure is not included in the @pNext@ chain,
-- @representativeFragmentTestEnable@ is considered to be
-- 'Vulkan.Core10.FundamentalTypes.FALSE', and the representative fragment
-- test is disabled.
--
-- If the active fragment shader does not specify the @EarlyFragmentTests@
-- execution mode, the representative fragment shader test has no effect,
-- even if enabled.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_representative_fragment_test VK_NV_representative_fragment_test>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRepresentativeFragmentTestStateCreateInfoNV = PipelineRepresentativeFragmentTestStateCreateInfoNV
  { -- | @representativeFragmentTestEnable@ controls whether the representative
    -- fragment test is enabled.
    representativeFragmentTestEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRepresentativeFragmentTestStateCreateInfoNV)
#endif
deriving instance Show PipelineRepresentativeFragmentTestStateCreateInfoNV

instance ToCStruct PipelineRepresentativeFragmentTestStateCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRepresentativeFragmentTestStateCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (representativeFragmentTestEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineRepresentativeFragmentTestStateCreateInfoNV where
  peekCStruct p = do
    representativeFragmentTestEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PipelineRepresentativeFragmentTestStateCreateInfoNV
             (bool32ToBool representativeFragmentTestEnable)

instance Storable PipelineRepresentativeFragmentTestStateCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRepresentativeFragmentTestStateCreateInfoNV where
  zero = PipelineRepresentativeFragmentTestStateCreateInfoNV
           zero


type NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION"
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION :: forall a . Integral a => a
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION = 2


type NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME = "VK_NV_representative_fragment_test"

-- No documentation found for TopLevel "VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME"
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME = "VK_NV_representative_fragment_test"

