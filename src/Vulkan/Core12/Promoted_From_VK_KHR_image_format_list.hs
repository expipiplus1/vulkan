{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_image_format_list"
module Vulkan.Core12.Promoted_From_VK_KHR_image_format_list  ( ImageFormatListCreateInfo(..)
                                                             , StructureType(..)
                                                             ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkImageFormatListCreateInfo - Specify that an image /can/ be used with a
-- particular set of formats
--
-- = Description
--
-- If @viewFormatCount@ is zero, @pViewFormats@ is ignored and the image is
-- created as if the 'ImageFormatListCreateInfo' structure were not
-- included in the @pNext@ list of 'Vulkan.Core10.Image.ImageCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageFormatListCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO'
--
-- -   #VUID-VkImageFormatListCreateInfo-pViewFormats-parameter# If
--     @viewFormatCount@ is not @0@, @pViewFormats@ /must/ be a valid
--     pointer to an array of @viewFormatCount@ valid
--     'Vulkan.Core10.Enums.Format.Format' values
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageFormatListCreateInfo = ImageFormatListCreateInfo
  { -- | @pViewFormats@ is an array which lists of all formats which /can/ be
    -- used when creating views of this image.
    viewFormats :: Vector Format }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageFormatListCreateInfo)
#endif
deriving instance Show ImageFormatListCreateInfo

instance ToCStruct ImageFormatListCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageFormatListCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewFormats)) :: Word32))
    pPViewFormats' <- ContT $ allocaBytesAligned @Format ((Data.Vector.length (viewFormats)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (viewFormats)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Format))) (pPViewFormats')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImageFormatListCreateInfo where
  peekCStruct p = do
    viewFormatCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pViewFormats <- peek @(Ptr Format) ((p `plusPtr` 24 :: Ptr (Ptr Format)))
    pViewFormats' <- generateM (fromIntegral viewFormatCount) (\i -> peek @Format ((pViewFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    pure $ ImageFormatListCreateInfo
             pViewFormats'

instance Zero ImageFormatListCreateInfo where
  zero = ImageFormatListCreateInfo
           mempty

