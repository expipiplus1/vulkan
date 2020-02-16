module Main
  where

import           Relude                  hiding ( runReader )
import           Say
import           Polysemy
import           Polysemy.Reader
import qualified Data.ByteString               as BS
import           Data.Text.Prettyprint.Doc      ( vsep )
import           Data.Text.Prettyprint.Doc.Render.Text

import           Spec.Parse
import           Error
import           Marshal
import           CType
import           Data.Text.Extra                ( (<+>) )
import           Render.Struct
import           Marshal.Scheme

main :: IO ()
main = do
  specText <- readFileBS "./Vulkan-Docs/xml/vk.xml"
  let rps = RenderParams id id id
      mps = MarshalParams isDefaultable' isStruct' isPassAsPointerType'
      r   = do
        Spec {..} <- parseSpec specText
        ss        <- traverseV marshalStruct specStructs
        traverseV renderStruct ss
  case run . runReader mps . runReader rps . runErr $ r of
    Left es -> do
      traverse_ sayErr es
      sayErr (show (length es) <+> "errors")
    Right r ->
      withFile "out/Vulkan.hs" WriteMode $ \h -> (hPutDoc h) (vsep (toList r))

----------------------------------------------------------------
-- Bespoke Vulkan stuff
----------------------------------------------------------------

isDefaultable' :: CType -> Bool
isDefaultable' t = do
  -- isBitmask'              <- (isJust .) <$> asks lIsBitmask
  -- isNonDispatchableHandle <-
  --   (maybe False (\h -> hHandleType h == NonDispatchable) .) <$> asks lIsHandle
  isDefaultableForeignType t || isIntegral t
    -- TODO
    -- || isBitmask || isNonDispatchableHandle

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
          ]
  )

isDefaultableForeignType :: CType -> Bool
isDefaultableForeignType =
  (`elem` [ TypeName "HANDLE"
          , TypeName "DWORD"
          , TypeName "LPCWSTR"
          , TypeName "PFN_vkInternalAllocationNotification"
          , TypeName "PFN_vkInternalFreeNotification"
          , TypeName "PFN_vkAllocationFunction"
          , TypeName "PFN_vkReallocationFunction"
          , TypeName "PFN_vkFreeFunction"
          , Ptr CType.Const (TypeName "SECURITY_ATTRIBUTES")
          ]
  )

isStruct' :: Text -> Bool
-- isStruct' = const False
isStruct' t =
  t == "VkBaseInStructure" || t == "VkPipelineRasterizationStateCreateInfo"


-- | Is this a type we don't want to marshal
isPassAsPointerType' :: CType -> Bool
isPassAsPointerType' = \case
  TypeName n
    | n
      `elem` [ "MirConnection"
             , "wl_display"
             , "wl_surface"
             , "Display"
             , "xcb_connection_t"
             , "AHardwareBuffer"
             , "ANativeWindow"
             , "CAMetalLayer"
             , "SECURITY_ATTRIBUTES"
             ]
    -> True
  _ -> False
