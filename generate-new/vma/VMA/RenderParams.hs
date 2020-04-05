module VMA.RenderParams
  ( renderParams
  ) where

import           Relude                  hiding ( uncons
                                                , Type
                                                , Handle
                                                )
import qualified Data.HashSet                  as Set
import qualified Data.Vector.Storable.Sized    as VSS
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import qualified Data.Text                     as T
import           Data.Text.Extra                ( lowerCaseFirst
                                                , upperCaseFirst
                                                )
import           Data.Char                      ( isLower )
import           Language.Haskell.TH            ( mkName )

import           Foreign.Ptr

import           CType
import           Render.Element
import           Spec.Parse
import           Render.Type.Preserve
import           Haskell
import qualified Bespoke.RenderParams          as Vk

renderParams :: Vector Handle -> RenderParams
renderParams handles = r
 where
  dispatchableHandleNames = Set.fromList
    [ hName | Handle {..} <- toList handles, hDispatchable == Dispatchable ]
  vulkanParams = Vk.renderParams handles
  r            = RenderParams
    { mkTyName                    = TyConName . upperCaseFirst . dropVma
    , mkConName                   = \_ -> ConName . upperCaseFirst . dropVma
    , mkMemberName = TermName . lowerCaseFirst . dropPointer . unCName
    , mkFunName                   = TermName . lowerCaseFirst . dropVma
    , mkParamName                 = TermName . dropPointer . unCName
    , mkPatternName               = ConName . upperCaseFirst . dropVma
    , mkFuncPointerName           = TyConName . T.tail . unCName
    , mkFuncPointerMemberName = TermName . ("p" <>) . upperCaseFirst . unCName
    , mkEmptyDataName             = TermName . (<> "_T") . dropVma
    , mkDispatchableHandlePtrName = TermName
                                    . (<> "Handle")
                                    . lowerCaseFirst
                                    . dropVma
    , alwaysQualifiedNames        = V.fromList [''VSS.Vector]
    , mkIdiomaticType             = mkIdiomaticType vulkanParams
    , mkHsTypeOverride = \preserve t -> case vulkanManifest vulkanParams t of
                           Just r -> pure r
                           _      -> case preserve of
                             DoNotPreserve -> Nothing
                             _             -> case t of
                               TypeName n
                                 | Set.member n dispatchableHandleNames -> Just
                                 $ ConT ''Ptr
                                 :@ ConT
                                      ( mkName
                                      . ("Graphics.Vulkan." <>)
                                      . T.unpack
                                      . unName
                                      . mkEmptyDataName vulkanParams
                                      $ n
                                      )
                               _ -> Nothing
    , unionDiscriminators         = mempty
    , successCodeType             = TypeName "VkResult"
    , isSuccessCodeReturned       = (/= "VK_SUCCESS")
    , firstSuccessCode            = "VK_SUCCESS"
    , exceptionTypeName           = TyConName "VulkanException"
    , complexMemberLengthFunction = \_ _ _ -> Nothing
    , isExternalName = let vk s = Just (ModName $ "Graphics.Vulkan." <> s)
                       in  \case
                             TermName  "advancePtrBytes" -> vk "CStruct.Utils"
                             TermName  "lowerArrayPtr"   -> vk "CStruct.Utils"
                             TyConName "Zero"            -> vk "Zero"
                             TyConName "ToCStruct"       -> vk "CStruct"
                             TyConName "FromCStruct"     -> vk "CStruct"
                             TyConName "IsHandle" -> vk "Core10.APIConstants"
                             TyConName ":::"             -> vk "NamedType"
                             _                           -> Nothing
    }

dropVma :: CName -> Text
dropVma (CName t) = fromMaybe t (dropPrefix "vma" t)

dropPrefix
  :: Text
  -- ^ Prefix
  -> Text
  -- ^ body
  -> Maybe Text
  -- ^ Nothing if 'body' didn't begin with 'prefix'
dropPrefix p t = if p `T.isPrefixOf` T.toLower t
  then Just . T.dropWhile (== '_') . T.drop (T.length p) $ t
  else Nothing

dropPointer :: Text -> Text
dropPointer =
  lowerCaseFirst
    . uncurry (<>)
    . first (\p -> if T.all (== 'p') p then "" else p)
    . T.span isLower

-- TODO: Generate this automatically
vulkanManifest :: RenderParams -> CType -> Maybe Type
vulkanManifest RenderParams {..} =
  let vk =
        Just
          . ConT
          . mkName
          . T.unpack
          . ("Graphics.Vulkan." <>)
          . unName
          . mkTyName
  in  \case
        TypeName n
          | n
            `elem` [ "VkFlags"
                   , "VkAllocation"
                   , "VkAllocationCallbacks"
                   , "VkBool32"
                   , "VkCommandBuffer_T"
                   , "VkDeviceMemory"
                   , "VkDeviceSize"
                   , "VkDevice_T"
                   , "VkInstance_T"
                   , "VkMemoryPropertyFlags"
                   , "PFN_vkAllocateMemory"
                   , "PFN_vkBindBufferMemory"
                   , "PFN_vkBindBufferMemory2KHR"
                   , "PFN_vkBindImageMemory"
                   , "PFN_vkBindImageMemory2KHR"
                   , "PFN_vkCmdCopyBuffer"
                   , "PFN_vkCreateBuffer"
                   , "PFN_vkCreateImage"
                   , "PFN_vkDestroyBuffer"
                   , "PFN_vkDestroyImage"
                   , "PFN_vkFlushMappedMemoryRanges"
                   , "PFN_vkFreeMemory"
                   , "PFN_vkGetBufferMemoryRequirements"
                   , "PFN_vkGetBufferMemoryRequirements2KHR"
                   , "PFN_vkGetImageMemoryRequirements"
                   , "PFN_vkGetImageMemoryRequirements2KHR"
                   , "PFN_vkGetPhysicalDeviceMemoryProperties"
                   , "PFN_vkGetPhysicalDeviceMemoryProperties2KHR"
                   , "PFN_vkGetPhysicalDeviceProperties"
                   , "PFN_vkInvalidateMappedMemoryRanges"
                   , "PFN_vkMapMemory"
                   , "PFN_vkUnmapMemory"
                   , "VkPhysicalDevice_T"
                   ]
          -> vk n
        _ -> Nothing
