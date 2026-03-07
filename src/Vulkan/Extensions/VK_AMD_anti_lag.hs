{-# language CPP #-}
-- | = Name
--
-- VK_AMD_anti_lag - device extension
--
-- = VK_AMD_anti_lag
--
-- [__Name String__]
--     @VK_AMD_anti_lag@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     477
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Stu Smith
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_AMD_anti_lag.adoc VK_AMD_anti_lag>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-06-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Stuart Smith, AMD
--
--     -   Arkadiusz Sarwa, AMD
--
-- == Description
--
-- This extension automatically paces the CPU to make sure it does not get
-- too far ahead of the GPU, reducing the latency between inputs received
-- and updates on the screen. Additionally, Anti-Lag+ offers applications
-- the ability to inform the driver when input processing begins, in order
-- to align the timing of display updates, enabling even lower latency
-- between receiving input and displaying on the screen.
--
-- == New Commands
--
-- -   'antiLagUpdateAMD'
--
-- == New Structures
--
-- -   'AntiLagDataAMD'
--
-- -   'AntiLagPresentationInfoAMD'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceAntiLagFeaturesAMD'
--
-- == New Enums
--
-- -   'AntiLagModeAMD'
--
-- -   'AntiLagStageAMD'
--
-- == New Enum Constants
--
-- -   'AMD_ANTI_LAG_EXTENSION_NAME'
--
-- -   'AMD_ANTI_LAG_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANTI_LAG_DATA_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANTI_LAG_PRESENTATION_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ANTI_LAG_FEATURES_AMD'
--
-- == Version History
--
-- -   Revision 1, 2024-06-06 (Arkadiusz Sarw)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_anti_lag Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_anti_lag  ( antiLagUpdateAMD
                                          , PhysicalDeviceAntiLagFeaturesAMD(..)
                                          , AntiLagDataAMD(..)
                                          , AntiLagPresentationInfoAMD(..)
                                          , AntiLagModeAMD( ANTI_LAG_MODE_DRIVER_CONTROL_AMD
                                                          , ANTI_LAG_MODE_ON_AMD
                                                          , ANTI_LAG_MODE_OFF_AMD
                                                          , ..
                                                          )
                                          , AntiLagStageAMD( ANTI_LAG_STAGE_INPUT_AMD
                                                           , ANTI_LAG_STAGE_PRESENT_AMD
                                                           , ..
                                                           )
                                          , AMD_ANTI_LAG_SPEC_VERSION
                                          , pattern AMD_ANTI_LAG_SPEC_VERSION
                                          , AMD_ANTI_LAG_EXTENSION_NAME
                                          , pattern AMD_ANTI_LAG_EXTENSION_NAME
                                          ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (maybePeek)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkAntiLagUpdateAMD))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ANTI_LAG_DATA_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ANTI_LAG_PRESENTATION_INFO_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ANTI_LAG_FEATURES_AMD))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAntiLagUpdateAMD
  :: FunPtr (Ptr Device_T -> Ptr AntiLagDataAMD -> IO ()) -> Ptr Device_T -> Ptr AntiLagDataAMD -> IO ()

-- | vkAntiLagUpdateAMD - Provide information to reduce latency
--
-- = Description
--
-- This command should be executed immediately before the application
-- processes user input. If @pData@ is not @NULL@ and
-- 'AntiLagDataAMD'::@presentationInfo@ is not @NULL@, this command
-- /should/ be executed again before
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', with
-- @pPresentationInfo@ set to matching values.
--
-- == Valid Usage
--
-- -   #VUID-vkAntiLagUpdateAMD-antiLag-10061# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-antiLag antiLag>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkAntiLagUpdateAMD-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkAntiLagUpdateAMD-pData-parameter# @pData@ /must/ be a valid
--     pointer to a valid 'AntiLagDataAMD' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_anti_lag VK_AMD_anti_lag>,
-- 'AntiLagDataAMD', 'Vulkan.Core10.Handles.Device'
antiLagUpdateAMD :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device
                    Device
                 -> -- | @pData@ is a pointer to a 'AntiLagDataAMD' structure containing latency
                    -- reduction parameters.
                    AntiLagDataAMD
                 -> io ()
antiLagUpdateAMD device data' = liftIO . evalContT $ do
  let vkAntiLagUpdateAMDPtr = pVkAntiLagUpdateAMD (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkAntiLagUpdateAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAntiLagUpdateAMD is null" Nothing Nothing
  let vkAntiLagUpdateAMD' = mkVkAntiLagUpdateAMD vkAntiLagUpdateAMDPtr
  pData <- ContT $ withCStruct (data')
  lift $ traceAroundEvent "vkAntiLagUpdateAMD" (vkAntiLagUpdateAMD'
                                                  (deviceHandle (device))
                                                  pData)
  pure $ ()


-- | VkPhysicalDeviceAntiLagFeaturesAMD - Structure describing whether
-- VK_AMD_anti_lag can be supported by an implementation.
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceAntiLagFeaturesAMD' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceAntiLagFeaturesAMD' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively
-- enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_anti_lag VK_AMD_anti_lag>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceAntiLagFeaturesAMD = PhysicalDeviceAntiLagFeaturesAMD
  { -- | #features-antiLag# @antiLag@ indicates whether the implementation
    -- supports AMD Radeon™ Anti-Lag functionality. The @antiLag@ feature only
    -- supports a single GPU and /must/ not be enabled if
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo'::@physicalDeviceCount@
    -- is greater than 1.
    antiLag :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceAntiLagFeaturesAMD)
#endif
deriving instance Show PhysicalDeviceAntiLagFeaturesAMD

instance ToCStruct PhysicalDeviceAntiLagFeaturesAMD where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceAntiLagFeaturesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ANTI_LAG_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (antiLag))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ANTI_LAG_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceAntiLagFeaturesAMD where
  peekCStruct p = do
    antiLag <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceAntiLagFeaturesAMD
             (bool32ToBool antiLag)

instance Storable PhysicalDeviceAntiLagFeaturesAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceAntiLagFeaturesAMD where
  zero = PhysicalDeviceAntiLagFeaturesAMD
           zero


-- | VkAntiLagDataAMD - Structure specifying the parameters for
-- vkAntiLagUpdateAMD
--
-- = Description
--
-- This structure specifies anti-lag parameters.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAntiLagDataAMD-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANTI_LAG_DATA_AMD'
--
-- -   #VUID-VkAntiLagDataAMD-mode-parameter# @mode@ /must/ be a valid
--     'AntiLagModeAMD' value
--
-- -   #VUID-VkAntiLagDataAMD-pPresentationInfo-parameter# If
--     @pPresentationInfo@ is not @NULL@, @pPresentationInfo@ /must/ be a
--     valid pointer to a valid 'AntiLagPresentationInfoAMD' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_anti_lag VK_AMD_anti_lag>,
-- 'AntiLagModeAMD', 'AntiLagPresentationInfoAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'antiLagUpdateAMD'
data AntiLagDataAMD = AntiLagDataAMD
  { -- | @mode@ is a 'AntiLagModeAMD' value specifying the anti-lag status.
    mode :: AntiLagModeAMD
  , -- | @maxFPS@ is the framerate limit, in frames per second, used by the
    -- application. This limit will be imposed if anti-lag is enabled. If the
    -- application tries to render faster, the framerate will be reduced to
    -- match this limit. A value of 0 will disable the limit.
    maxFPS :: Word32
  , -- | @pPresentationInfo@ is a pointer to a 'AntiLagPresentationInfoAMD'
    -- structure containing information about the application stage.
    presentationInfo :: Maybe AntiLagPresentationInfoAMD
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AntiLagDataAMD)
#endif
deriving instance Show AntiLagDataAMD

instance ToCStruct AntiLagDataAMD where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AntiLagDataAMD{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANTI_LAG_DATA_AMD)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AntiLagModeAMD)) (mode)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (maxFPS)
    pPresentationInfo'' <- case (presentationInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AntiLagPresentationInfoAMD))) pPresentationInfo''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANTI_LAG_DATA_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AntiLagModeAMD)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct AntiLagDataAMD where
  peekCStruct p = do
    mode <- peek @AntiLagModeAMD ((p `plusPtr` 16 :: Ptr AntiLagModeAMD))
    maxFPS <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pPresentationInfo <- peek @(Ptr AntiLagPresentationInfoAMD) ((p `plusPtr` 24 :: Ptr (Ptr AntiLagPresentationInfoAMD)))
    pPresentationInfo' <- maybePeek (\j -> peekCStruct @AntiLagPresentationInfoAMD (j)) pPresentationInfo
    pure $ AntiLagDataAMD
             mode maxFPS pPresentationInfo'

instance Zero AntiLagDataAMD where
  zero = AntiLagDataAMD
           zero
           zero
           Nothing


-- | VkAntiLagPresentationInfoAMD - Structure specifying information about
-- stage
--
-- = Description
--
-- This structure specifies information about the presentation stage for
-- which anti-lag parameters are being set.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_anti_lag VK_AMD_anti_lag>,
-- 'AntiLagDataAMD', 'AntiLagStageAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AntiLagPresentationInfoAMD = AntiLagPresentationInfoAMD
  { -- | @stage@ is a 'AntiLagStageAMD' value specifying the current application
    -- stage.
    --
    -- #VUID-VkAntiLagPresentationInfoAMD-stage-parameter# @stage@ /must/ be a
    -- valid 'AntiLagStageAMD' value
    stage :: AntiLagStageAMD
  , -- | @frameIndex@ is set just before the application processes input data
    -- ('ANTI_LAG_STAGE_INPUT_AMD'). The same @frameIndex@ value /should/ be
    -- set before the frame with current input data will be presented by
    -- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR'
    -- ('ANTI_LAG_STAGE_PRESENT_AMD'). This /should/ be done for each frame.
    frameIndex :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AntiLagPresentationInfoAMD)
#endif
deriving instance Show AntiLagPresentationInfoAMD

instance ToCStruct AntiLagPresentationInfoAMD where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AntiLagPresentationInfoAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANTI_LAG_PRESENTATION_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AntiLagStageAMD)) (stage)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (frameIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANTI_LAG_PRESENTATION_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AntiLagStageAMD)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct AntiLagPresentationInfoAMD where
  peekCStruct p = do
    stage <- peek @AntiLagStageAMD ((p `plusPtr` 16 :: Ptr AntiLagStageAMD))
    frameIndex <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ AntiLagPresentationInfoAMD
             stage frameIndex

instance Storable AntiLagPresentationInfoAMD where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AntiLagPresentationInfoAMD where
  zero = AntiLagPresentationInfoAMD
           zero
           zero


-- | VkAntiLagModeAMD - Set the status of the anti-lag feature
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_anti_lag VK_AMD_anti_lag>,
-- 'AntiLagDataAMD'
newtype AntiLagModeAMD = AntiLagModeAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ANTI_LAG_MODE_DRIVER_CONTROL_AMD' specifies that anti-lag will be
-- enabled or disabled depending on driver settings.
pattern ANTI_LAG_MODE_DRIVER_CONTROL_AMD = AntiLagModeAMD 0

-- | 'ANTI_LAG_MODE_ON_AMD' specifies that anti-lag will be enabled.
pattern ANTI_LAG_MODE_ON_AMD = AntiLagModeAMD 1

-- | 'ANTI_LAG_MODE_OFF_AMD' specifies that anti-lag will be disabled.
pattern ANTI_LAG_MODE_OFF_AMD = AntiLagModeAMD 2

{-# COMPLETE
  ANTI_LAG_MODE_DRIVER_CONTROL_AMD
  , ANTI_LAG_MODE_ON_AMD
  , ANTI_LAG_MODE_OFF_AMD ::
    AntiLagModeAMD
  #-}

conNameAntiLagModeAMD :: String
conNameAntiLagModeAMD = "AntiLagModeAMD"

enumPrefixAntiLagModeAMD :: String
enumPrefixAntiLagModeAMD = "ANTI_LAG_MODE_"

showTableAntiLagModeAMD :: [(AntiLagModeAMD, String)]
showTableAntiLagModeAMD =
  [
    ( ANTI_LAG_MODE_DRIVER_CONTROL_AMD
    , "DRIVER_CONTROL_AMD"
    )
  , (ANTI_LAG_MODE_ON_AMD, "ON_AMD")
  , (ANTI_LAG_MODE_OFF_AMD, "OFF_AMD")
  ]

instance Show AntiLagModeAMD where
  showsPrec =
    enumShowsPrec
      enumPrefixAntiLagModeAMD
      showTableAntiLagModeAMD
      conNameAntiLagModeAMD
      (\(AntiLagModeAMD x) -> x)
      (showsPrec 11)

instance Read AntiLagModeAMD where
  readPrec =
    enumReadPrec
      enumPrefixAntiLagModeAMD
      showTableAntiLagModeAMD
      conNameAntiLagModeAMD
      AntiLagModeAMD

-- | VkAntiLagStageAMD - Report the application stage
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_anti_lag VK_AMD_anti_lag>,
-- 'AntiLagPresentationInfoAMD'
newtype AntiLagStageAMD = AntiLagStageAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ANTI_LAG_STAGE_INPUT_AMD' specifies the stage before processing input.
pattern ANTI_LAG_STAGE_INPUT_AMD = AntiLagStageAMD 0

-- | 'ANTI_LAG_STAGE_PRESENT_AMD' specifies the stage before
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR'.
pattern ANTI_LAG_STAGE_PRESENT_AMD = AntiLagStageAMD 1

{-# COMPLETE
  ANTI_LAG_STAGE_INPUT_AMD
  , ANTI_LAG_STAGE_PRESENT_AMD ::
    AntiLagStageAMD
  #-}

conNameAntiLagStageAMD :: String
conNameAntiLagStageAMD = "AntiLagStageAMD"

enumPrefixAntiLagStageAMD :: String
enumPrefixAntiLagStageAMD = "ANTI_LAG_STAGE_"

showTableAntiLagStageAMD :: [(AntiLagStageAMD, String)]
showTableAntiLagStageAMD =
  [ (ANTI_LAG_STAGE_INPUT_AMD, "INPUT_AMD")
  , (ANTI_LAG_STAGE_PRESENT_AMD, "PRESENT_AMD")
  ]

instance Show AntiLagStageAMD where
  showsPrec =
    enumShowsPrec
      enumPrefixAntiLagStageAMD
      showTableAntiLagStageAMD
      conNameAntiLagStageAMD
      (\(AntiLagStageAMD x) -> x)
      (showsPrec 11)

instance Read AntiLagStageAMD where
  readPrec =
    enumReadPrec
      enumPrefixAntiLagStageAMD
      showTableAntiLagStageAMD
      conNameAntiLagStageAMD
      AntiLagStageAMD

type AMD_ANTI_LAG_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_ANTI_LAG_SPEC_VERSION"
pattern AMD_ANTI_LAG_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_ANTI_LAG_SPEC_VERSION = 1


type AMD_ANTI_LAG_EXTENSION_NAME = "VK_AMD_anti_lag"

-- No documentation found for TopLevel "VK_AMD_ANTI_LAG_EXTENSION_NAME"
pattern AMD_ANTI_LAG_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_ANTI_LAG_EXTENSION_NAME = "VK_AMD_anti_lag"

