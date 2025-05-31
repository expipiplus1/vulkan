{-# language CPP #-}
-- | = Name
--
-- VK_EXT_layer_settings - instance extension
--
-- == VK_EXT_layer_settings
--
-- [__Name String__]
--     @VK_EXT_layer_settings@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     497
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Christophe Riccio
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_layer_settings] @christophe%0A*Here describe the issue or question you have about the VK_EXT_layer_settings extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_layer_settings.adoc VK_EXT_layer_settings>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-09-23
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Christophe Riccio, LunarG
--
--     -   Mark Lobodzinski, LunarG
--
--     -   Charles Giessen, LunarG
--
--     -   Spencer Fricke, LunarG
--
--     -   Juan Ramos, LunarG
--
--     -   Daniel Rakos, RasterGrid
--
--     -   Shahbaz Youssefi, Google
--
--     -   Lina Versace, Google
--
--     -   Bill Hollings, The Brenwill Workshop
--
--     -   Jon Leech, Khronos
--
--     -   Tom Olson, Arm
--
-- == Description
--
-- This extension provides a mechanism for configuring programmatically
-- through the Vulkan API the behavior of layers.
--
-- This extension provides the 'LayerSettingsCreateInfoEXT' struct that can
-- be included in the @pNext@ chain of the
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure passed
-- as the @pCreateInfo@ parameter of
-- 'Vulkan.Core10.DeviceInitialization.createInstance'.
--
-- The structure contains an array of 'LayerSettingEXT' structure values
-- that configure specific features of layers.
--
-- == Example
--
-- @VK_EXT_layer_settings@ is implemented by the Vulkan Profiles layer.
--
-- It allows the profiles layer tests used by the profiles layer C.I. to
-- programmatically configure the layer for each test without affecting the
-- C.I. environment, allowing to run multiple tests concurrently.
--
-- > const char* profile_file_data = JSON_TEST_FILES_PATH "VP_KHR_roadmap_2022.json";
-- > const char* profile_name_data = "VP_KHR_roadmap_2022";
-- > VkBool32 emulate_portability_data = VK_TRUE;
-- > const char* simulate_capabilities[] = {
-- >     "SIMULATE_API_VERSION_BIT",
-- >     "SIMULATE_FEATURES_BIT",
-- >     "SIMULATE_PROPERTIES_BIT",
-- >     "SIMULATE_EXTENSIONS_BIT",
-- >     "SIMULATE_FORMATS_BIT",
-- >     "SIMULATE_QUEUE_FAMILY_PROPERTIES_BIT"
-- > };
-- > const char* debug_reports[] = {
-- >     "DEBUG_REPORT_ERROR_BIT",
-- >     "DEBUG_REPORT_WARNING_BIT",
-- >     "DEBUG_REPORT_NOTIFICATION_BIT",
-- >     "DEBUG_REPORT_DEBUG_BIT"
-- > };
-- >
-- > const VkLayerSettingEXT settings[] = {
-- >      {kLayerName, kLayerSettingsProfileFile, VK_LAYER_SETTING_TYPE_STRING_EXT, 1, &profile_file_data},
-- >      {kLayerName, kLayerSettingsProfileName, VK_LAYER_SETTING_TYPE_STRING_EXT, 1, &profile_name_data},
-- >      {kLayerName, kLayerSettingsEmulatePortability, VK_LAYER_SETTING_TYPE_BOOL32_EXT, 1, &emulate_portability_data},
-- >      {kLayerName, kLayerSettingsSimulateCapabilities, VK_LAYER_SETTING_TYPE_STRING_EXT,
-- >         static_cast<uint32_t>(std::size(simulate_capabilities)), simulate_capabilities},
-- >      {kLayerName, kLayerSettingsDebugReports, VK_LAYER_SETTING_TYPE_STRING_EXT,
-- >         static_cast<uint32_t>(std::size(debug_reports)), debug_reports}
-- > };
-- >
-- > const VkLayerSettingsCreateInfoEXT layer_settings_create_info{
-- >     VK_STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT, nullptr,
-- >     static_cast<uint32_t>(std::size(settings)), settings};
-- >
-- > VkInstanceCreateInfo inst_create_info = {};
-- > ...
-- > inst_create_info.pNext = &layer_settings_create_info;
-- > vkCreateInstance(&inst_create_info, nullptr, &_instances);
--
-- Note
--
-- The @VK_EXT_layer_settings@ extension subsumes all the functionality
-- provided in the @VK_EXT_validation_flags@ extension and the
-- @VK_EXT_validation_features@ extension.
--
-- == New Structures
--
-- -   'LayerSettingEXT'
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'LayerSettingsCreateInfoEXT'
--
-- == New Enums
--
-- -   'LayerSettingTypeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_LAYER_SETTINGS_EXTENSION_NAME'
--
-- -   'EXT_LAYER_SETTINGS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT'
--
-- == Issues
--
-- -   How should application developers figure out the list of available
--     settings?
--
-- This extension does not provide a reflection API for layer settings.
-- Layer settings are described in each layer JSON manifest and the
-- documentation of each layer which implements this extension.
--
-- == Version History
--
-- -   Revision 1, 2020-06-17 (Mark Lobodzinski)
--
--     -   Initial revision for Validation layer internal usages
--
-- -   Revision 2, 2023-09-26 (Christophe Riccio)
--
--     -   Refactor APIs for any layer usages and public release
--
-- == See Also
--
-- 'LayerSettingEXT', 'LayerSettingTypeEXT', 'LayerSettingsCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_layer_settings Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_layer_settings  ( LayerSettingsCreateInfoEXT(..)
                                                , LayerSettingEXT(..)
                                                , LayerSettingTypeEXT( LAYER_SETTING_TYPE_BOOL32_EXT
                                                                     , LAYER_SETTING_TYPE_INT32_EXT
                                                                     , LAYER_SETTING_TYPE_INT64_EXT
                                                                     , LAYER_SETTING_TYPE_UINT32_EXT
                                                                     , LAYER_SETTING_TYPE_UINT64_EXT
                                                                     , LAYER_SETTING_TYPE_FLOAT32_EXT
                                                                     , LAYER_SETTING_TYPE_FLOAT64_EXT
                                                                     , LAYER_SETTING_TYPE_STRING_EXT
                                                                     , ..
                                                                     )
                                                , EXT_LAYER_SETTINGS_SPEC_VERSION
                                                , pattern EXT_LAYER_SETTINGS_SPEC_VERSION
                                                , EXT_LAYER_SETTINGS_EXTENSION_NAME
                                                , pattern EXT_LAYER_SETTINGS_EXTENSION_NAME
                                                ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
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
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT))
-- | VkLayerSettingsCreateInfoEXT - Specify layer capabilities for a Vulkan
-- instance
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkLayerSettingsCreateInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT'
--
-- -   #VUID-VkLayerSettingsCreateInfoEXT-pSettings-parameter# If
--     @settingCount@ is not @0@, @pSettings@ /must/ be a valid pointer to
--     an array of @settingCount@ valid 'LayerSettingEXT' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_layer_settings VK_EXT_layer_settings>,
-- 'LayerSettingEXT', 'Vulkan.Core10.Enums.StructureType.StructureType'
data LayerSettingsCreateInfoEXT = LayerSettingsCreateInfoEXT
  { -- | @pSettings@ is a pointer to an array of @settingCount@ 'LayerSettingEXT'
    -- values specifying the setting to be configured.
    settings :: Vector LayerSettingEXT }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LayerSettingsCreateInfoEXT)
#endif
deriving instance Show LayerSettingsCreateInfoEXT

instance ToCStruct LayerSettingsCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LayerSettingsCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (settings)) :: Word32))
    pPSettings' <- ContT $ allocaBytes @LayerSettingEXT ((Data.Vector.length (settings)) * 32)
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPSettings' `plusPtr` (32 * (i)) :: Ptr LayerSettingEXT) (e) . ($ ())) (settings)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr LayerSettingEXT))) (pPSettings')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct LayerSettingsCreateInfoEXT where
  peekCStruct p = do
    settingCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pSettings <- peek @(Ptr LayerSettingEXT) ((p `plusPtr` 24 :: Ptr (Ptr LayerSettingEXT)))
    pSettings' <- generateM (fromIntegral settingCount) (\i -> peekCStruct @LayerSettingEXT ((pSettings `advancePtrBytes` (32 * (i)) :: Ptr LayerSettingEXT)))
    pure $ LayerSettingsCreateInfoEXT
             pSettings'

instance Zero LayerSettingsCreateInfoEXT where
  zero = LayerSettingsCreateInfoEXT
           mempty


-- | VkLayerSettingEXT - Specify a layer capability to configure
--
-- = Description
--
-- When multiple 'LayerSettingsCreateInfoEXT' structures are chained and
-- the same @pSettingName@ is referenced for the same @pLayerName@, the
-- value of the first reference of the layer setting is used.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkLayerSettingEXT-pLayerName-parameter# @pLayerName@ /must/ be
--     a null-terminated UTF-8 string
--
-- -   #VUID-VkLayerSettingEXT-pSettingName-parameter# @pSettingName@
--     /must/ be a null-terminated UTF-8 string
--
-- -   #VUID-VkLayerSettingEXT-type-parameter# @type@ /must/ be a valid
--     'LayerSettingTypeEXT' value
--
-- -   #VUID-VkLayerSettingEXT-pValues-parameter# If @valueCount@ is not
--     @0@, @pValues@ /must/ be a valid pointer to an array of @valueCount@
--     bytes
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_layer_settings VK_EXT_layer_settings>,
-- 'LayerSettingTypeEXT', 'LayerSettingsCreateInfoEXT'
data LayerSettingEXT = LayerSettingEXT
  { -- | @pLayerName@ is a pointer to a null-terminated UTF-8 string naming the
    -- layer to configure the setting from.
    layerName :: ByteString
  , -- | @pSettingName@ is a pointer to a null-terminated UTF-8 string naming the
    -- setting to configure. Unknown @pSettingName@ by the layer are ignored.
    settingName :: ByteString
  , -- | @type@ is a 'LayerSettingTypeEXT' value specifying the type of the
    -- @pValues@ values.
    type' :: LayerSettingTypeEXT
  , -- No documentation found for Nested "VkLayerSettingEXT" "valueCount"
    valueCount :: Word32
  , -- | @pValues@ is a pointer to an array of @count@ values of the type
    -- indicated by @type@ to configure the layer setting.
    values :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LayerSettingEXT)
#endif
deriving instance Show LayerSettingEXT

instance ToCStruct LayerSettingEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LayerSettingEXT{..} f = evalContT $ do
    pLayerName'' <- ContT $ useAsCString (layerName)
    lift $ poke ((p `plusPtr` 0 :: Ptr (Ptr CChar))) pLayerName''
    pSettingName'' <- ContT $ useAsCString (settingName)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr CChar))) pSettingName''
    lift $ poke ((p `plusPtr` 16 :: Ptr LayerSettingTypeEXT)) (type')
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (valueCount)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (values)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    pLayerName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 0 :: Ptr (Ptr CChar))) pLayerName''
    pSettingName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr CChar))) pSettingName''
    lift $ poke ((p `plusPtr` 16 :: Ptr LayerSettingTypeEXT)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    lift $ f

instance FromCStruct LayerSettingEXT where
  peekCStruct p = do
    pLayerName <- packCString =<< peek ((p `plusPtr` 0 :: Ptr (Ptr CChar)))
    pSettingName <- packCString =<< peek ((p `plusPtr` 8 :: Ptr (Ptr CChar)))
    type' <- peek @LayerSettingTypeEXT ((p `plusPtr` 16 :: Ptr LayerSettingTypeEXT))
    valueCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pValues <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ LayerSettingEXT
             pLayerName pSettingName type' valueCount pValues

instance Zero LayerSettingEXT where
  zero = LayerSettingEXT
           mempty
           mempty
           zero
           zero
           zero


-- | VkLayerSettingTypeEXT - Type of the values that can be passed to a layer
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_layer_settings VK_EXT_layer_settings>,
-- 'LayerSettingEXT'
newtype LayerSettingTypeEXT = LayerSettingTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'LAYER_SETTING_TYPE_BOOL32_EXT' specifies that the layer setting’s type
-- is 'Vulkan.Core10.FundamentalTypes.Bool32'.
pattern LAYER_SETTING_TYPE_BOOL32_EXT = LayerSettingTypeEXT 0

-- | 'LAYER_SETTING_TYPE_INT32_EXT' specifies that the layer setting’s type
-- is signed 32-bit integer.
pattern LAYER_SETTING_TYPE_INT32_EXT = LayerSettingTypeEXT 1

-- | 'LAYER_SETTING_TYPE_INT64_EXT' specifies that the layer setting’s type
-- is signed 64-bit integer.
pattern LAYER_SETTING_TYPE_INT64_EXT = LayerSettingTypeEXT 2

-- | 'LAYER_SETTING_TYPE_UINT32_EXT' specifies that the layer setting’s type
-- is unsigned 32-bit integer.
pattern LAYER_SETTING_TYPE_UINT32_EXT = LayerSettingTypeEXT 3

-- | 'LAYER_SETTING_TYPE_UINT64_EXT' specifies that the layer setting’s type
-- is unsigned 64-bit integer.
pattern LAYER_SETTING_TYPE_UINT64_EXT = LayerSettingTypeEXT 4

-- | 'LAYER_SETTING_TYPE_FLOAT32_EXT' specifies that the layer setting’s type
-- is 32-bit floating-point.
pattern LAYER_SETTING_TYPE_FLOAT32_EXT = LayerSettingTypeEXT 5

-- | 'LAYER_SETTING_TYPE_FLOAT64_EXT' specifies that the layer setting’s type
-- is 64-bit floating-point.
pattern LAYER_SETTING_TYPE_FLOAT64_EXT = LayerSettingTypeEXT 6

-- | 'LAYER_SETTING_TYPE_STRING_EXT' specifies that the layer setting’s type
-- is a pointer to a null-terminated UTF-8 string.
pattern LAYER_SETTING_TYPE_STRING_EXT = LayerSettingTypeEXT 7

{-# COMPLETE
  LAYER_SETTING_TYPE_BOOL32_EXT
  , LAYER_SETTING_TYPE_INT32_EXT
  , LAYER_SETTING_TYPE_INT64_EXT
  , LAYER_SETTING_TYPE_UINT32_EXT
  , LAYER_SETTING_TYPE_UINT64_EXT
  , LAYER_SETTING_TYPE_FLOAT32_EXT
  , LAYER_SETTING_TYPE_FLOAT64_EXT
  , LAYER_SETTING_TYPE_STRING_EXT ::
    LayerSettingTypeEXT
  #-}

conNameLayerSettingTypeEXT :: String
conNameLayerSettingTypeEXT = "LayerSettingTypeEXT"

enumPrefixLayerSettingTypeEXT :: String
enumPrefixLayerSettingTypeEXT = "LAYER_SETTING_TYPE_"

showTableLayerSettingTypeEXT :: [(LayerSettingTypeEXT, String)]
showTableLayerSettingTypeEXT =
  [ (LAYER_SETTING_TYPE_BOOL32_EXT, "BOOL32_EXT")
  , (LAYER_SETTING_TYPE_INT32_EXT, "INT32_EXT")
  , (LAYER_SETTING_TYPE_INT64_EXT, "INT64_EXT")
  , (LAYER_SETTING_TYPE_UINT32_EXT, "UINT32_EXT")
  , (LAYER_SETTING_TYPE_UINT64_EXT, "UINT64_EXT")
  , (LAYER_SETTING_TYPE_FLOAT32_EXT, "FLOAT32_EXT")
  , (LAYER_SETTING_TYPE_FLOAT64_EXT, "FLOAT64_EXT")
  , (LAYER_SETTING_TYPE_STRING_EXT, "STRING_EXT")
  ]

instance Show LayerSettingTypeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixLayerSettingTypeEXT
      showTableLayerSettingTypeEXT
      conNameLayerSettingTypeEXT
      (\(LayerSettingTypeEXT x) -> x)
      (showsPrec 11)

instance Read LayerSettingTypeEXT where
  readPrec =
    enumReadPrec
      enumPrefixLayerSettingTypeEXT
      showTableLayerSettingTypeEXT
      conNameLayerSettingTypeEXT
      LayerSettingTypeEXT

type EXT_LAYER_SETTINGS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_LAYER_SETTINGS_SPEC_VERSION"
pattern EXT_LAYER_SETTINGS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_LAYER_SETTINGS_SPEC_VERSION = 2


type EXT_LAYER_SETTINGS_EXTENSION_NAME = "VK_EXT_layer_settings"

-- No documentation found for TopLevel "VK_EXT_LAYER_SETTINGS_EXTENSION_NAME"
pattern EXT_LAYER_SETTINGS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_LAYER_SETTINGS_EXTENSION_NAME = "VK_EXT_layer_settings"

