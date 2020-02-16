module Main
  where

import           Relude                  hiding ( runReader )
import           Relude.Extra.Map
import           Say
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector                   as V

import           CType
import           Data.Text.Extra                ( (<+>) )
import           Error
import           Marshal
import           Marshal.Scheme
import           Render.Command
import           Render.Element
import           Render.Struct
import           Render.Type
import           Spec.Parse

main :: IO ()
main = do
  sayErr "Reading spec"
  specText <- readFileBS "./Vulkan-Docs/xml/vk.xml"
  let
    rps :: RenderParams
    rps = RenderParams id id id id id
    r   = do
      sayErr "Parsing spec"
      Spec {..} <- parseSpec specText
      let structNames :: HashSet Text
          structNames =
            fromList
              . (extraStructNames <>)
              . toList
              . fmap structName
              $ specStructs
          isStruct' = (`member` structNames)
          mps = MarshalParams isDefaultable' isStruct' isPassAsPointerType'
      runReader mps $ do
        sayErr "Marshaling structs"
        ss <- traverseV marshalStruct specStructs
        sayErr "Marshaling commands"
        cs <- traverseV marshalCommand specCommands
        sayErr "Rendering structs and commands"
        sequenceV (fmap renderStruct ss <> fmap renderCommand cs)
  (runM . runReader rps . runErr $ r) >>= \case
    Left es -> do
      traverse_ sayErr es
      sayErr (show (length es) <+> "errors")
    Right rs -> renderModule "out" (V.singleton "Vulkan") rs

----------------------------------------------------------------
-- Bespoke Vulkan stuff
----------------------------------------------------------------

isDefaultable' :: CType -> Bool
isDefaultable' t =
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

-- TODO: Remove, extra union names and handle
extraStructNames :: [Text]
extraStructNames = ["VkClearColorValue", "VkSemaphore"]
