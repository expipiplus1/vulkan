module VMA.RenderParams
  ( renderParams
  ) where

import           Relude                  hiding ( uncons
                                                , Type
                                                , Handle
                                                )
import qualified Data.HashSet                  as Set
import           Data.Vector                    ( Vector )
import qualified Data.Text                     as T
import           Data.Text.Extra                ( lowerCaseFirst
                                                , upperCaseFirst
                                                )
import           Data.Char                      ( isLower )
import           Language.Haskell.TH
import           Data.Generics.Uniplate.Data
import           Polysemy

import           Foreign.Ptr

import qualified Bespoke.RenderParams          as Vk
import           CType
import           Haskell
import           Render.Element
import           Render.Type.Preserve
import           Spec.Parse
import           VkModulePrefix

renderParams :: Vector Handle -> RenderParams
renderParams handles = r
 where
  dispatchableHandleNames = Set.fromList
    [ hName | Handle {..} <- toList handles, hDispatchable == Dispatchable ]
  vulkanParams = Vk.renderParams handles
  r            = RenderParams
    { mkTyName = \n -> TyConName $ fromMaybe (upperCaseFirst . dropVma $ n)
                                             (vulkanNameOverrides n)
    , mkConName = \_ n -> ConName $ fromMaybe (upperCaseFirst . dropVma $ n)
                                              (vulkanNameOverrides n)
    , mkMemberName = TermName . lowerCaseFirst . dropPointer . unCName
    , mkFunName                   = TermName . lowerCaseFirst . dropVma
    , mkParamName                 = TermName . dropPointer . unCName
    , mkPatternName               =
      \n -> ConName
        $ fromMaybe (upperCaseFirst . dropVma $ n) (vulkanNameOverrides n)
    , mkFuncPointerName           = TyConName . T.tail . unCName
    , mkFuncPointerMemberName = TermName . ("p" <>) . upperCaseFirst . unCName
    , mkEmptyDataName             = TermName . (<> "_T") . dropVma
    , mkDispatchableHandlePtrName = TermName
                                    . (<> "Handle")
                                    . lowerCaseFirst
                                    . dropVma
    , alwaysQualifiedNames        = mempty
    , mkIdiomaticType = let dropVulkanModule = transformBi
                              (\n ->
                                if nameModule n
                                   == Just (T.unpack vulkanModulePrefix)
                                then
                                  mkName (nameBase n)
                                else
                                  n
                              )
                        in  mkIdiomaticType vulkanParams . dropVulkanModule
    , mkHsTypeOverride            = \structStyle preserve t ->
      case vulkanManifest structStyle vulkanParams t of
        Just t -> Just $ do
          t <- t
          case preserve of
            DoNotPreserve -> case mkIdiomaticType r t of
              Just i  -> pure $ itType i
              Nothing -> pure t
            _ -> pure t
        _ -> case preserve of
          DoNotPreserve -> Nothing
          _             -> case t of
            TypeName n | Set.member n dispatchableHandleNames ->
              Just . pure $ ConT ''Ptr :@ ConT
                ( mkName
                . ((T.unpack vulkanModulePrefix <> ".") <>)
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
    , isExternalName              =
      let vk s = Just (ModName $ vulkanModulePrefix <> "." <> s)
      in  \case
            TermName  "advancePtrBytes"  -> vk "CStruct.Utils"
            TermName  "lowerArrayPtr"    -> vk "CStruct.Utils"
            TermName  "boolToBool32"     -> vk "Core10.FundamentalTypes"
            TermName  "bool32ToBool"     -> vk "Core10.FundamentalTypes"
            TyConName "Zero"             -> vk "Zero"
            TyConName "ToCStruct"        -> vk "CStruct"
            TyConName "FromCStruct"      -> vk "CStruct"
            TyConName "IsHandle"         -> vk "Core10.APIConstants"
            TyConName ":::"              -> vk "NamedType"
            TyConName "MAX_MEMORY_TYPES" -> vk "Core10.APIConstants"
            TyConName "MAX_MEMORY_HEAPS" -> vk "Core10.APIConstants"
            ConName   "MAX_MEMORY_TYPES" -> vk "Core10.APIConstants"
            ConName   "MAX_MEMORY_HEAPS" -> vk "Core10.APIConstants"
            TyConName "SomeStruct"       -> vk "CStruct.Extends"
            TyConName "PokeChain"        -> vk "CStruct.Extends"
            TyConName "Extendss"         -> vk "CStruct.Extends"
            TermName  "forgetExtensions" -> vk "CStruct.Extends"
            TyConName "VulkanException"  -> vk "Exception"
            ConName   "SUCCESS"          -> vk "Core10.Enums.Result"
            _                            -> Nothing
    , externalDocHTML             = Nothing
    , objectTypePattern           = const Nothing
    }

dropVma :: CName -> Text
dropVma (CName t) = fromMaybe t (dropPrefix "vma" t)

vulkanNameOverrides :: CName -> Maybe Text
vulkanNameOverrides = \case
  "VK_MAX_MEMORY_TYPES" -> Just "MAX_MEMORY_TYPES"
  "VK_MAX_MEMORY_HEAPS" -> Just "MAX_MEMORY_HEAPS"
  "VK_SUCCESS"          -> Just "SUCCESS"
  _                     -> Nothing

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
vulkanManifest
  :: ExtensibleStructStyle r -> RenderParams -> CType -> Maybe (Sem r Type)
vulkanManifest structStyle RenderParams {..} =
  let vk =
        Just
          . pure
          . ConT
          . mkName
          . T.unpack
          . ((vulkanModulePrefix <> ".") <>)
          . unName
          . mkTyName
      someVk t = Just $ do
        let structTyCon =
              ConT
                . mkName
                . T.unpack
                . ((vulkanModulePrefix <> ".") <>)
                . unName
                . mkTyName
                $ t
        case structStyle of
          Applied getVar -> do
            v <- getVar
            pure $ structTyCon :@ v
          Wrapped -> pure $ ConT (mkName "SomeStruct") :@ structTyCon
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
                   , "VkPhysicalDevice_T"
                   , "VkBuffer"
                   , "VkBufferCopy"
                   , "VkBufferMemoryRequirementsInfo2"
                   , "VkImage"
                   , "VkMappedMemoryRange"
                   , "VkMemoryMapFlags"
                   , "VkMemoryRequirements"
                   , "VkPhysicalDeviceMemoryProperties"
                   , "VkPhysicalDeviceProperties"
                   , "VkResult"
                   ]
          -> vk n
          | n
            `elem` [ "VkMemoryAllocateInfo"
                   , "VkBindBufferMemoryInfo"
                   , "VkBindImageMemoryInfo"
                   , "VkBufferCreateInfo"
                   , "VkImageCreateInfo"
                   , "VkMemoryRequirements2"
                   , "VkImageMemoryRequirementsInfo2"
                   , "VkPhysicalDeviceMemoryProperties2"
                   ]
          -> someVk n
        _ -> Nothing

