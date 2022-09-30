module Bespoke.MarshalParams
  ( marshalParams
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import           Polysemy
import           Relude                  hiding ( Handle
                                                , Type
                                                , uncons
                                                )
import           Relude.Extra.Map

import           Bespoke
import           CType
import           Marshal.Scheme
import           Spec.Parse

marshalParams :: KnownSpecFlavor t => Spec t -> Sem r MarshalParams
marshalParams spec@Spec {..} = do
  bespokeSchemes <- bespokeSchemes spec
  let
    aliasMap :: Map.HashMap CName CName
    aliasMap = fromList [ (aName, aTarget) | Alias {..} <- toList specAliases ]
    resolveAlias :: CName -> CName
    resolveAlias t = maybe t resolveAlias (Map.lookup t aliasMap) -- TODO: handle cycles!
    bitmaskNames :: HashSet CName
    bitmaskNames = fromList
      [ n
      | Enum {..}        <- toList specEnums
      , ABitmask flags _ <- pure eType
      , n                <- [eName, flags]
      ]
    isBitmask     = (`member` bitmaskNames)
    isBitmaskType = \case
      TypeName n -> isBitmask n || isBitmask (resolveAlias n)
      _          -> False
    nonDispatchableHandleNames :: HashSet CName
    nonDispatchableHandleNames = fromList
      [ hName
      | Handle {..} <- toList specHandles
      , hDispatchable == NonDispatchable
      ]
    isNonDispatchableHandle     = (`member` nonDispatchableHandleNames)
    isNonDispatchableHandleType = \case
      TypeName n ->
        isNonDispatchableHandle n || isNonDispatchableHandle (resolveAlias n)
      _ -> False
    dispatchableHandleNames :: HashSet CName
    dispatchableHandleNames = fromList
      [ hName
      | Handle {..} <- toList specHandles
      , hDispatchable == Dispatchable
      ]
    -- TODO Remove, these will not be defaultable once we bundle the
    -- command pointers
    isDispatchableHandle     = (`member` dispatchableHandleNames)
    isDispatchableHandleType = \case
      TypeName n ->
        isDispatchableHandle n || isDispatchableHandle (resolveAlias n)
      _ -> False
    atomNames :: HashSet CName
    atomNames  = fromList [ atName | Atom {..} <- toList specAtoms ]
    isAtomType = \case
      TypeName n -> n `member` atomNames
      _          -> False
  pure MarshalParams
    { isDefaultable       = isDefaultable'
                            <||> isBitmaskType
                            <||> isNonDispatchableHandleType
                            <||> isDispatchableHandleType
                            <||> isAtomType
    , isPassAsPointerType = isPassAsPointerType'
    , isForeignStruct     = isForeignStruct'
    , getBespokeScheme    = \p a ->
      asum . fmap (\(BespokeScheme f) -> f p a) $ bespokeSchemes
    }

----------------------------------------------------------------
-- Bespoke Vulkan stuff
----------------------------------------------------------------

isDefaultable' :: CType -> Bool
isDefaultable' t =
  isDefaultableForeignType t || isIntegral t || isFloating t || hasUnknownEnum t

isIntegral :: CType -> Bool
isIntegral =
  (`elem` [ Int
          , Char
          , TypeName "uint8_t"
          , TypeName "uint16_t"
          , TypeName "uint32_t"
          , TypeName "uint64_t"
          , TypeName "int8_t"
          , TypeName "int16_t"
          , TypeName "int32_t"
          , TypeName "int64_t"
          , TypeName "size_t"
          , TypeName "VkDeviceSize"
          , TypeName "VkDeviceAddress"
          , TypeName "VkDeviceOrHostAddressConstKHR"
          , TypeName "VkDeviceOrHostAddressKHR"
          , TypeName "VkBool32"
          , TypeName "VkExtent2D"
          , TypeName "VkExtent3D"
          , TypeName "LARGE_INTEGER"
          -- TODO: Get these from spec
          -- Base types
          , TypeName "XrTime"
          , TypeName "XrDuration"
          , TypeName "XrBool32"
          ]
  )

isFloating :: CType -> Bool
isFloating = (`elem` [Float, Double])

-- | Foreign handles
isDefaultableForeignType :: CType -> Bool
isDefaultableForeignType t =
  (      t
    `elem` [ TypeName "HANDLE"
           , TypeName "DWORD"
           , TypeName "LPCWSTR"
           , Ptr CType.Const (TypeName "SECURITY_ATTRIBUTES")
           , TypeName "zx_handle_t"
           , TypeName "IOSurfaceRef"
           ]
    )
    || case t of
         TypeName (CName n) -> "PFN_" `T.isPrefixOf` n
         _                  -> False

-- TODO: These shouldn't be defaultable, probably a spec oversight
hasUnknownEnum :: CType -> Bool
hasUnknownEnum = (`elem` [TypeName "VkFormat", TypeName "VkObjectType", TypeName "VkOpticalFlowPerformanceLevelNV"])

-- | Is this a type we don't want to marshal
isPassAsPointerType' :: CType -> Bool
isPassAsPointerType' = \case
  TypeName n ->
    n
      `elem` [ "MirConnection"
             , "wl_display"
             , "wl_surface"
             , "Display"
             , "xcb_connection_t"
             , "AHardwareBuffer"
             , "ANativeWindow"
             , "CAMetalLayer"
             , "SECURITY_ATTRIBUTES"
             , "IDirectFB"
             , "IDirectFBSurface"
             , "IUnknown"
             , "jobject"
             , "_screen_window"
             , "_screen_context"
             -- This is used in a slightly weird way in
             -- VkGetPipelinePropertiesEXT, Just fall back to having the user
             -- allocate it and assume they know what they're doing
             , "VkBaseOutStructure"
             , "VkBaseInStructure"
             -- TODO: remove these
             , "VkInstanceCreateInfo"
             , "VkAllocationCallbacks"
             , "VkDeviceCreateInfo"
             , "VkAllocationCallbacks"
             ]
  _ -> False

-- | Is this a foreign struct we've defined (not specified in the spec)
isForeignStruct' :: CType -> Bool
isForeignStruct' = \case
  TypeName "LARGE_INTEGER" -> True
  TypeName "timespec"      -> True
  _                        -> False
