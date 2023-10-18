{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_bias_control - device extension
--
-- == VK_EXT_depth_bias_control
--
-- [__Name String__]
--     @VK_EXT_depth_bias_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     284
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_bias_control] @Joshua-Ashton%0A*Here describe the issue or question you have about the VK_EXT_depth_bias_control extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_depth_bias_control.adoc VK_EXT_depth_bias_control>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-02-15
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, VALVE
--
--     -   Hans-Kristian Arntzen, VALVE
--
--     -   Mike Blumenkrantz, VALVE
--
--     -   Georg Lehmann, VALVE
--
--     -   Piers Daniell, NVIDIA
--
--     -   Lionel Landwerlin, INTEL
--
--     -   Tobias Hector, AMD
--
--     -   Ricardo Garcia, IGALIA
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Shahbaz Youssefi, GOOGLE
--
--     -   Tom Olson, ARM
--
-- == Description
--
-- This extension adds a new structure, 'DepthBiasRepresentationInfoEXT',
-- that can be added to a @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' and allows
-- setting the scaling and representation of depth bias for a pipeline.
--
-- This state can also be set dynamically by using the new structure
-- mentioned above in combination with the new 'cmdSetDepthBias2EXT'
-- command.
--
-- == New Commands
--
-- -   'cmdSetDepthBias2EXT'
--
-- == New Structures
--
-- -   'DepthBiasInfoEXT'
--
-- -   Extending 'DepthBiasInfoEXT',
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'DepthBiasRepresentationInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthBiasControlFeaturesEXT'
--
-- == New Enums
--
-- -   'DepthBiasRepresentationEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_BIAS_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_BIAS_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEPTH_BIAS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEPTH_BIAS_REPRESENTATION_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_BIAS_CONTROL_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-09-22 (Joshua Ashton)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'DepthBiasInfoEXT', 'DepthBiasRepresentationEXT',
-- 'DepthBiasRepresentationInfoEXT',
-- 'PhysicalDeviceDepthBiasControlFeaturesEXT', 'cmdSetDepthBias2EXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_depth_bias_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_bias_control  ( cmdSetDepthBias2EXT
                                                    , DepthBiasInfoEXT(..)
                                                    , DepthBiasRepresentationInfoEXT(..)
                                                    , PhysicalDeviceDepthBiasControlFeaturesEXT(..)
                                                    , DepthBiasRepresentationEXT( DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORMAT_EXT
                                                                                , DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORCE_UNORM_EXT
                                                                                , DEPTH_BIAS_REPRESENTATION_FLOAT_EXT
                                                                                , ..
                                                                                )
                                                    , EXT_DEPTH_BIAS_CONTROL_SPEC_VERSION
                                                    , pattern EXT_DEPTH_BIAS_CONTROL_SPEC_VERSION
                                                    , EXT_DEPTH_BIAS_CONTROL_EXTENSION_NAME
                                                    , pattern EXT_DEPTH_BIAS_CONTROL_EXTENSION_NAME
                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBias2EXT))
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEPTH_BIAS_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEPTH_BIAS_REPRESENTATION_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_BIAS_CONTROL_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBias2EXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct DepthBiasInfoEXT) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct DepthBiasInfoEXT) -> IO ()

-- | vkCmdSetDepthBias2EXT - Set depth bias factors and clamp dynamically for
-- a command buffer
--
-- = Description
--
-- This command is functionally identical to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBias', but includes
-- extensible sub-structures that include @sType@ and @pNext@ parameters,
-- allowing them to be more easily extended.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthBias2EXT-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthBias2EXT-pDepthBiasInfo-parameter#
--     @pDepthBiasInfo@ /must/ be a valid pointer to a valid
--     'DepthBiasInfoEXT' structure
--
-- -   #VUID-vkCmdSetDepthBias2EXT-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthBias2EXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthBias2EXT-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_bias_control VK_EXT_depth_bias_control>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DepthBiasInfoEXT'
cmdSetDepthBias2EXT :: forall a io
                     . (Extendss DepthBiasInfoEXT a, PokeChain a, MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command will be
                       -- recorded.
                       CommandBuffer
                    -> -- | @pDepthBiasInfo@ is a pointer to a 'DepthBiasInfoEXT' structure
                       -- specifying depth bias parameters.
                       (DepthBiasInfoEXT a)
                    -> io ()
cmdSetDepthBias2EXT commandBuffer depthBiasInfo = liftIO . evalContT $ do
  let vkCmdSetDepthBias2EXTPtr = pVkCmdSetDepthBias2EXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetDepthBias2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthBias2EXT is null" Nothing Nothing
  let vkCmdSetDepthBias2EXT' = mkVkCmdSetDepthBias2EXT vkCmdSetDepthBias2EXTPtr
  pDepthBiasInfo <- ContT $ withCStruct (depthBiasInfo)
  lift $ traceAroundEvent "vkCmdSetDepthBias2EXT" (vkCmdSetDepthBias2EXT'
                                                     (commandBufferHandle (commandBuffer))
                                                     (forgetExtensions pDepthBiasInfo))
  pure $ ()


-- | VkDepthBiasInfoEXT - Structure specifying depth bias parameters
--
-- = Description
--
-- If @pNext@ does not contain a 'DepthBiasRepresentationInfoEXT'
-- structure, then this command is equivalent to including a
-- 'DepthBiasRepresentationInfoEXT' with @depthBiasExact@ set to
-- 'Vulkan.Core10.FundamentalTypes.FALSE' and @depthBiasRepresentation@ set
-- to 'DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORMAT_EXT'.
--
-- == Valid Usage
--
-- -   #VUID-VkDepthBiasInfoEXT-depthBiasClamp-08950# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-depthBiasClamp depthBiasClamp>
--     feature is not enabled, @depthBiasClamp@ /must/ be @0.0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDepthBiasInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEPTH_BIAS_INFO_EXT'
--
-- -   #VUID-VkDepthBiasInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of 'DepthBiasRepresentationInfoEXT'
--
-- -   #VUID-VkDepthBiasInfoEXT-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_bias_control VK_EXT_depth_bias_control>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdSetDepthBias2EXT'
data DepthBiasInfoEXT (es :: [Type]) = DepthBiasInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @depthBiasConstantFactor@ is a scalar factor controlling the constant
    -- depth value added to each fragment.
    depthBiasConstantFactor :: Float
  , -- | @depthBiasClamp@ is the maximum (or minimum) depth bias of a fragment.
    depthBiasClamp :: Float
  , -- | @depthBiasSlopeFactor@ is a scalar factor applied to a fragment’s slope
    -- in depth bias calculations.
    depthBiasSlopeFactor :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DepthBiasInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DepthBiasInfoEXT es)

instance Extensible DepthBiasInfoEXT where
  extensibleTypeName = "DepthBiasInfoEXT"
  setNext DepthBiasInfoEXT{..} next' = DepthBiasInfoEXT{next = next', ..}
  getNext DepthBiasInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DepthBiasInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DepthBiasRepresentationInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss DepthBiasInfoEXT es
         , PokeChain es ) => ToCStruct (DepthBiasInfoEXT es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DepthBiasInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEPTH_BIAS_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (depthBiasConstantFactor))
    lift $ poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (depthBiasClamp))
    lift $ poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (depthBiasSlopeFactor))
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEPTH_BIAS_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (zero))
    lift $ f

instance ( Extendss DepthBiasInfoEXT es
         , PeekChain es ) => FromCStruct (DepthBiasInfoEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    depthBiasConstantFactor <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    depthBiasClamp <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    depthBiasSlopeFactor <- peek @CFloat ((p `plusPtr` 24 :: Ptr CFloat))
    pure $ DepthBiasInfoEXT
             next
             (coerce @CFloat @Float depthBiasConstantFactor)
             (coerce @CFloat @Float depthBiasClamp)
             (coerce @CFloat @Float depthBiasSlopeFactor)

instance es ~ '[] => Zero (DepthBiasInfoEXT es) where
  zero = DepthBiasInfoEXT
           ()
           zero
           zero
           zero


-- | VkDepthBiasRepresentationInfoEXT - Structure specifying depth bias
-- parameters
--
-- == Valid Usage
--
-- -   #VUID-VkDepthBiasRepresentationInfoEXT-leastRepresentableValueForceUnormRepresentation-08947#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-leastRepresentableValueForceUnormRepresentation leastRepresentableValueForceUnormRepresentation>
--     feature is not enabled, @depthBiasRepresentation@ /must/ not be
--     'DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORCE_UNORM_EXT'
--
-- -   #VUID-VkDepthBiasRepresentationInfoEXT-floatRepresentation-08948# If
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-floatRepresentation floatRepresentation>
--     feature is not enabled, @depthBiasRepresentation@ /must/ not be
--     'DEPTH_BIAS_REPRESENTATION_FLOAT_EXT'
--
-- -   #VUID-VkDepthBiasRepresentationInfoEXT-depthBiasExact-08949# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-depthBiasExact depthBiasExact>
--     feature is not enabled, @depthBiasExact@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDepthBiasRepresentationInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEPTH_BIAS_REPRESENTATION_INFO_EXT'
--
-- -   #VUID-VkDepthBiasRepresentationInfoEXT-depthBiasRepresentation-parameter#
--     @depthBiasRepresentation@ /must/ be a valid
--     'DepthBiasRepresentationEXT' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_bias_control VK_EXT_depth_bias_control>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'DepthBiasRepresentationEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DepthBiasRepresentationInfoEXT = DepthBiasRepresentationInfoEXT
  { -- | @depthBiasRepresentation@ is a 'DepthBiasRepresentationEXT' value
    -- specifying the depth bias representation.
    depthBiasRepresentation :: DepthBiasRepresentationEXT
  , -- | @depthBiasExact@ specifies that the implementation is not allowed to
    -- scale the depth bias value to ensure a minimum resolvable distance.
    depthBiasExact :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DepthBiasRepresentationInfoEXT)
#endif
deriving instance Show DepthBiasRepresentationInfoEXT

instance ToCStruct DepthBiasRepresentationInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DepthBiasRepresentationInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEPTH_BIAS_REPRESENTATION_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DepthBiasRepresentationEXT)) (depthBiasRepresentation)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (depthBiasExact))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEPTH_BIAS_REPRESENTATION_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DepthBiasRepresentationEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DepthBiasRepresentationInfoEXT where
  peekCStruct p = do
    depthBiasRepresentation <- peek @DepthBiasRepresentationEXT ((p `plusPtr` 16 :: Ptr DepthBiasRepresentationEXT))
    depthBiasExact <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ DepthBiasRepresentationInfoEXT
             depthBiasRepresentation (bool32ToBool depthBiasExact)

instance Storable DepthBiasRepresentationInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DepthBiasRepresentationInfoEXT where
  zero = DepthBiasRepresentationInfoEXT
           zero
           zero


-- | VkPhysicalDeviceDepthBiasControlFeaturesEXT - Structure indicating
-- support for depth bias scaling and representation control
--
-- = Members
--
-- This structure describes the following feature:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_bias_control VK_EXT_depth_bias_control>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDepthBiasControlFeaturesEXT = PhysicalDeviceDepthBiasControlFeaturesEXT
  { -- | #features-depthBiasControl# @depthBiasControl@ indicates whether the
    -- implementation supports the 'cmdSetDepthBias2EXT' command and the
    -- 'DepthBiasRepresentationInfoEXT' structure.
    depthBiasControl :: Bool
  , -- | #features-leastRepresentableValueForceUnormRepresentation#
    -- @leastRepresentableValueForceUnormRepresentation@ indicates whether the
    -- implementation supports using the
    -- 'DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORCE_UNORM_EXT'
    -- depth bias representation.
    leastRepresentableValueForceUnormRepresentation :: Bool
  , -- | #features-floatRepresentation# @floatRepresentation@ indicates whether
    -- the implementation supports using the
    -- 'DEPTH_BIAS_REPRESENTATION_FLOAT_EXT' depth bias representation.
    floatRepresentation :: Bool
  , -- | #features-depthBiasExact# @depthBiasExact@ indicates whether the
    -- implementation supports forcing depth bias to not be scaled to ensure a
    -- minimum resolvable difference using
    -- 'DepthBiasRepresentationInfoEXT'::@depthBiasExact@.
    depthBiasExact :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDepthBiasControlFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDepthBiasControlFeaturesEXT

instance ToCStruct PhysicalDeviceDepthBiasControlFeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDepthBiasControlFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_BIAS_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (depthBiasControl))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (leastRepresentableValueForceUnormRepresentation))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (floatRepresentation))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (depthBiasExact))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_BIAS_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDepthBiasControlFeaturesEXT where
  peekCStruct p = do
    depthBiasControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    leastRepresentableValueForceUnormRepresentation <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    floatRepresentation <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    depthBiasExact <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceDepthBiasControlFeaturesEXT
             (bool32ToBool depthBiasControl)
             (bool32ToBool leastRepresentableValueForceUnormRepresentation)
             (bool32ToBool floatRepresentation)
             (bool32ToBool depthBiasExact)

instance Storable PhysicalDeviceDepthBiasControlFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDepthBiasControlFeaturesEXT where
  zero = PhysicalDeviceDepthBiasControlFeaturesEXT
           zero
           zero
           zero
           zero


-- | VkDepthBiasRepresentationEXT - Specify the depth bias representation
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_bias_control VK_EXT_depth_bias_control>,
-- 'DepthBiasRepresentationInfoEXT'
newtype DepthBiasRepresentationEXT = DepthBiasRepresentationEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORMAT_EXT'
-- specifies that the depth bias representation is a factor of the format’s
-- r as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-depthbias-computation>.
pattern DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORMAT_EXT = DepthBiasRepresentationEXT 0

-- | 'DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORCE_UNORM_EXT'
-- specifies that the depth bias representation is a factor of a constant r
-- defined by the bit-size or mantissa of the format as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-depthbias-computation>.
pattern DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORCE_UNORM_EXT = DepthBiasRepresentationEXT 1

-- | 'DEPTH_BIAS_REPRESENTATION_FLOAT_EXT' specifies that the depth bias
-- representation is a factor of constant r equal to 1.
pattern DEPTH_BIAS_REPRESENTATION_FLOAT_EXT = DepthBiasRepresentationEXT 2

{-# COMPLETE
  DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORMAT_EXT
  , DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORCE_UNORM_EXT
  , DEPTH_BIAS_REPRESENTATION_FLOAT_EXT ::
    DepthBiasRepresentationEXT
  #-}

conNameDepthBiasRepresentationEXT :: String
conNameDepthBiasRepresentationEXT = "DepthBiasRepresentationEXT"

enumPrefixDepthBiasRepresentationEXT :: String
enumPrefixDepthBiasRepresentationEXT = "DEPTH_BIAS_REPRESENTATION_"

showTableDepthBiasRepresentationEXT :: [(DepthBiasRepresentationEXT, String)]
showTableDepthBiasRepresentationEXT =
  [
    ( DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORMAT_EXT
    , "LEAST_REPRESENTABLE_VALUE_FORMAT_EXT"
    )
  ,
    ( DEPTH_BIAS_REPRESENTATION_LEAST_REPRESENTABLE_VALUE_FORCE_UNORM_EXT
    , "LEAST_REPRESENTABLE_VALUE_FORCE_UNORM_EXT"
    )
  ,
    ( DEPTH_BIAS_REPRESENTATION_FLOAT_EXT
    , "FLOAT_EXT"
    )
  ]

instance Show DepthBiasRepresentationEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixDepthBiasRepresentationEXT
      showTableDepthBiasRepresentationEXT
      conNameDepthBiasRepresentationEXT
      (\(DepthBiasRepresentationEXT x) -> x)
      (showsPrec 11)

instance Read DepthBiasRepresentationEXT where
  readPrec =
    enumReadPrec
      enumPrefixDepthBiasRepresentationEXT
      showTableDepthBiasRepresentationEXT
      conNameDepthBiasRepresentationEXT
      DepthBiasRepresentationEXT

type EXT_DEPTH_BIAS_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEPTH_BIAS_CONTROL_SPEC_VERSION"
pattern EXT_DEPTH_BIAS_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEPTH_BIAS_CONTROL_SPEC_VERSION = 1


type EXT_DEPTH_BIAS_CONTROL_EXTENSION_NAME = "VK_EXT_depth_bias_control"

-- No documentation found for TopLevel "VK_EXT_DEPTH_BIAS_CONTROL_EXTENSION_NAME"
pattern EXT_DEPTH_BIAS_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEPTH_BIAS_CONTROL_EXTENSION_NAME = "VK_EXT_depth_bias_control"

