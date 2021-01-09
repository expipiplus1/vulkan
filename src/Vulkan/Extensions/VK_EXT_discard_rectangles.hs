{-# language CPP #-}
-- | = Name
--
-- VK_EXT_discard_rectangles - device extension
--
-- == VK_EXT_discard_rectangles
--
-- [__Name String__]
--     @VK_EXT_discard_rectangles@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     100
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_discard_rectangles:%20&body=@pdaniell-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-22
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_device_group@
--
--     -   Interacts with Vulkan 1.1
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension provides additional orthogonally aligned “discard
-- rectangles” specified in framebuffer-space coordinates that restrict
-- rasterization of all points, lines and triangles.
--
-- From zero to an implementation-dependent limit (specified by
-- @maxDiscardRectangles@) number of discard rectangles can be operational
-- at once. When one or more discard rectangles are active, rasterized
-- fragments can either survive if the fragment is within any of the
-- operational discard rectangles ('DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT'
-- mode) or be rejected if the fragment is within any of the operational
-- discard rectangles ('DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT' mode).
--
-- These discard rectangles operate orthogonally to the existing scissor
-- test functionality. The discard rectangles can be different for each
-- physical device in a device group by specifying the device mask and
-- setting discard rectangle dynamic state.
--
-- == New Commands
--
-- -   'cmdSetDiscardRectangleEXT'
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineDiscardRectangleStateCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDiscardRectanglePropertiesEXT'
--
-- == New Enums
--
-- -   'DiscardRectangleModeEXT'
--
-- == New Bitmasks
--
-- -   'PipelineDiscardRectangleStateCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DISCARD_RECTANGLES_EXTENSION_NAME'
--
-- -   'EXT_DISCARD_RECTANGLES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2016-12-22 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'DiscardRectangleModeEXT',
-- 'PhysicalDeviceDiscardRectanglePropertiesEXT',
-- 'PipelineDiscardRectangleStateCreateFlagsEXT',
-- 'PipelineDiscardRectangleStateCreateInfoEXT',
-- 'cmdSetDiscardRectangleEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_discard_rectangles Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_discard_rectangles  ( cmdSetDiscardRectangleEXT
                                                    , PhysicalDeviceDiscardRectanglePropertiesEXT(..)
                                                    , PipelineDiscardRectangleStateCreateInfoEXT(..)
                                                    , PipelineDiscardRectangleStateCreateFlagsEXT(..)
                                                    , DiscardRectangleModeEXT( DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
                                                                             , DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
                                                                             , ..
                                                                             )
                                                    , EXT_DISCARD_RECTANGLES_SPEC_VERSION
                                                    , pattern EXT_DISCARD_RECTANGLES_SPEC_VERSION
                                                    , EXT_DISCARD_RECTANGLES_EXTENSION_NAME
                                                    , pattern EXT_DISCARD_RECTANGLES_EXTENSION_NAME
                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDiscardRectangleEXT))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDiscardRectangleEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- | vkCmdSetDiscardRectangleEXT - Set discard rectangles dynamically
--
-- = Description
--
-- The discard rectangle taken from element i of @pDiscardRectangles@
-- replace the current state for the discard rectangle at index
-- @firstDiscardRectangle@ + i, for i in [0, @discardRectangleCount@).
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDiscardRectangleEXT-firstDiscardRectangle-00585# The
--     sum of @firstDiscardRectangle@ and @discardRectangleCount@ /must/ be
--     less than or equal to
--     'PhysicalDeviceDiscardRectanglePropertiesEXT'::@maxDiscardRectangles@
--
-- -   #VUID-vkCmdSetDiscardRectangleEXT-x-00587# The @x@ and @y@ member of
--     @offset@ in each 'Vulkan.Core10.FundamentalTypes.Rect2D' element of
--     @pDiscardRectangles@ /must/ be greater than or equal to @0@
--
-- -   #VUID-vkCmdSetDiscardRectangleEXT-offset-00588# Evaluation of
--     (@offset.x@ + @extent.width@) in each
--     'Vulkan.Core10.FundamentalTypes.Rect2D' element of
--     @pDiscardRectangles@ /must/ not cause a signed integer addition
--     overflow
--
-- -   #VUID-vkCmdSetDiscardRectangleEXT-offset-00589# Evaluation of
--     (@offset.y@ + @extent.height@) in each
--     'Vulkan.Core10.FundamentalTypes.Rect2D' element of
--     @pDiscardRectangles@ /must/ not cause a signed integer addition
--     overflow
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDiscardRectangleEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDiscardRectangleEXT-pDiscardRectangles-parameter#
--     @pDiscardRectangles@ /must/ be a valid pointer to an array of
--     @discardRectangleCount@ 'Vulkan.Core10.FundamentalTypes.Rect2D'
--     structures
--
-- -   #VUID-vkCmdSetDiscardRectangleEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDiscardRectangleEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDiscardRectangleEXT-discardRectangleCount-arraylength#
--     @discardRectangleCount@ /must/ be greater than @0@
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D'
cmdSetDiscardRectangleEXT :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @firstDiscardRectangle@ is the index of the first discard rectangle
                             -- whose state is updated by the command.
                             ("firstDiscardRectangle" ::: Word32)
                          -> -- | @pDiscardRectangles@ is a pointer to an array of
                             -- 'Vulkan.Core10.FundamentalTypes.Rect2D' structures specifying discard
                             -- rectangles.
                             ("discardRectangles" ::: Vector Rect2D)
                          -> io ()
cmdSetDiscardRectangleEXT commandBuffer firstDiscardRectangle discardRectangles = liftIO . evalContT $ do
  let vkCmdSetDiscardRectangleEXTPtr = pVkCmdSetDiscardRectangleEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetDiscardRectangleEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDiscardRectangleEXT is null" Nothing Nothing
  let vkCmdSetDiscardRectangleEXT' = mkVkCmdSetDiscardRectangleEXT vkCmdSetDiscardRectangleEXTPtr
  pPDiscardRectangles <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (discardRectangles)) * 16) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDiscardRectangles `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (discardRectangles)
  lift $ traceAroundEvent "vkCmdSetDiscardRectangleEXT" (vkCmdSetDiscardRectangleEXT' (commandBufferHandle (commandBuffer)) (firstDiscardRectangle) ((fromIntegral (Data.Vector.length $ (discardRectangles)) :: Word32)) (pPDiscardRectangles))
  pure $ ()


-- | VkPhysicalDeviceDiscardRectanglePropertiesEXT - Structure describing
-- discard rectangle limits that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceDiscardRectanglePropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceDiscardRectanglePropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDiscardRectanglePropertiesEXT = PhysicalDeviceDiscardRectanglePropertiesEXT
  { -- | #limits-maxDiscardRectangles# @maxDiscardRectangles@ is the maximum
    -- number of active discard rectangles that /can/ be specified.
    maxDiscardRectangles :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDiscardRectanglePropertiesEXT)
#endif
deriving instance Show PhysicalDeviceDiscardRectanglePropertiesEXT

instance ToCStruct PhysicalDeviceDiscardRectanglePropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDiscardRectanglePropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxDiscardRectangles)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceDiscardRectanglePropertiesEXT where
  peekCStruct p = do
    maxDiscardRectangles <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceDiscardRectanglePropertiesEXT
             maxDiscardRectangles

instance Storable PhysicalDeviceDiscardRectanglePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDiscardRectanglePropertiesEXT where
  zero = PhysicalDeviceDiscardRectanglePropertiesEXT
           zero


-- | VkPipelineDiscardRectangleStateCreateInfoEXT - Structure specifying
-- discard rectangle
--
-- = Description
--
-- If the
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
-- dynamic state is enabled for a pipeline, the @pDiscardRectangles@ member
-- is ignored.
--
-- When this structure is included in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo', it defines
-- parameters of the discard rectangle test. If this structure is not
-- included in the @pNext@ chain, it is equivalent to specifying this
-- structure with a @discardRectangleCount@ of @0@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DiscardRectangleModeEXT',
-- 'PipelineDiscardRectangleStateCreateFlagsEXT',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineDiscardRectangleStateCreateInfoEXT = PipelineDiscardRectangleStateCreateInfoEXT
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkPipelineDiscardRectangleStateCreateInfoEXT-flags-zerobitmask#
    -- @flags@ /must/ be @0@
    flags :: PipelineDiscardRectangleStateCreateFlagsEXT
  , -- | @discardRectangleMode@ is a 'DiscardRectangleModeEXT' value determining
    -- whether the discard rectangle test is inclusive or exclusive.
    --
    -- #VUID-VkPipelineDiscardRectangleStateCreateInfoEXT-discardRectangleMode-parameter#
    -- @discardRectangleMode@ /must/ be a valid 'DiscardRectangleModeEXT' value
    discardRectangleMode :: DiscardRectangleModeEXT
  , -- | @pDiscardRectangles@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.Rect2D' structures defining discard
    -- rectangles.
    discardRectangles :: Vector Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineDiscardRectangleStateCreateInfoEXT)
#endif
deriving instance Show PipelineDiscardRectangleStateCreateInfoEXT

instance ToCStruct PipelineDiscardRectangleStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineDiscardRectangleStateCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineDiscardRectangleStateCreateFlagsEXT)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr DiscardRectangleModeEXT)) (discardRectangleMode)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (discardRectangles)) :: Word32))
    pPDiscardRectangles' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (discardRectangles)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDiscardRectangles' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (discardRectangles)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Rect2D))) (pPDiscardRectangles')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr DiscardRectangleModeEXT)) (zero)
    f

instance FromCStruct PipelineDiscardRectangleStateCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PipelineDiscardRectangleStateCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PipelineDiscardRectangleStateCreateFlagsEXT))
    discardRectangleMode <- peek @DiscardRectangleModeEXT ((p `plusPtr` 20 :: Ptr DiscardRectangleModeEXT))
    discardRectangleCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pDiscardRectangles <- peek @(Ptr Rect2D) ((p `plusPtr` 32 :: Ptr (Ptr Rect2D)))
    pDiscardRectangles' <- generateM (fromIntegral discardRectangleCount) (\i -> peekCStruct @Rect2D ((pDiscardRectangles `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ PipelineDiscardRectangleStateCreateInfoEXT
             flags discardRectangleMode pDiscardRectangles'

instance Zero PipelineDiscardRectangleStateCreateInfoEXT where
  zero = PipelineDiscardRectangleStateCreateInfoEXT
           zero
           zero
           mempty


-- | VkPipelineDiscardRectangleStateCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'PipelineDiscardRectangleStateCreateFlagsEXT' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'PipelineDiscardRectangleStateCreateInfoEXT'
newtype PipelineDiscardRectangleStateCreateFlagsEXT = PipelineDiscardRectangleStateCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineDiscardRectangleStateCreateFlagsEXT :: String
conNamePipelineDiscardRectangleStateCreateFlagsEXT = "PipelineDiscardRectangleStateCreateFlagsEXT"

enumPrefixPipelineDiscardRectangleStateCreateFlagsEXT :: String
enumPrefixPipelineDiscardRectangleStateCreateFlagsEXT = ""

showTablePipelineDiscardRectangleStateCreateFlagsEXT :: [(PipelineDiscardRectangleStateCreateFlagsEXT, String)]
showTablePipelineDiscardRectangleStateCreateFlagsEXT = []

instance Show PipelineDiscardRectangleStateCreateFlagsEXT where
  showsPrec = enumShowsPrec enumPrefixPipelineDiscardRectangleStateCreateFlagsEXT
                            showTablePipelineDiscardRectangleStateCreateFlagsEXT
                            conNamePipelineDiscardRectangleStateCreateFlagsEXT
                            (\(PipelineDiscardRectangleStateCreateFlagsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineDiscardRectangleStateCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixPipelineDiscardRectangleStateCreateFlagsEXT
                          showTablePipelineDiscardRectangleStateCreateFlagsEXT
                          conNamePipelineDiscardRectangleStateCreateFlagsEXT
                          PipelineDiscardRectangleStateCreateFlagsEXT


-- | VkDiscardRectangleModeEXT - Specify the discard rectangle mode
--
-- = See Also
--
-- 'PipelineDiscardRectangleStateCreateInfoEXT'
newtype DiscardRectangleModeEXT = DiscardRectangleModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT' specifies that the discard
-- rectangle test is inclusive.
pattern DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT = DiscardRectangleModeEXT 0
-- | 'DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT' specifies that the discard
-- rectangle test is exclusive.
pattern DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT = DiscardRectangleModeEXT 1
{-# complete DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT,
             DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT :: DiscardRectangleModeEXT #-}

conNameDiscardRectangleModeEXT :: String
conNameDiscardRectangleModeEXT = "DiscardRectangleModeEXT"

enumPrefixDiscardRectangleModeEXT :: String
enumPrefixDiscardRectangleModeEXT = "DISCARD_RECTANGLE_MODE_"

showTableDiscardRectangleModeEXT :: [(DiscardRectangleModeEXT, String)]
showTableDiscardRectangleModeEXT =
  [(DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT, "INCLUSIVE_EXT"), (DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT, "EXCLUSIVE_EXT")]

instance Show DiscardRectangleModeEXT where
  showsPrec = enumShowsPrec enumPrefixDiscardRectangleModeEXT
                            showTableDiscardRectangleModeEXT
                            conNameDiscardRectangleModeEXT
                            (\(DiscardRectangleModeEXT x) -> x)
                            (showsPrec 11)

instance Read DiscardRectangleModeEXT where
  readPrec = enumReadPrec enumPrefixDiscardRectangleModeEXT
                          showTableDiscardRectangleModeEXT
                          conNameDiscardRectangleModeEXT
                          DiscardRectangleModeEXT


type EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION"
pattern EXT_DISCARD_RECTANGLES_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1


type EXT_DISCARD_RECTANGLES_EXTENSION_NAME = "VK_EXT_discard_rectangles"

-- No documentation found for TopLevel "VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME"
pattern EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DISCARD_RECTANGLES_EXTENSION_NAME = "VK_EXT_discard_rectangles"

