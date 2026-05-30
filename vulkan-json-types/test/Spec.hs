{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Prelude                  hiding (Enum)

import           Data.Aeson               (FromJSON, eitherDecodeFileStrict)
import qualified Data.Map.Strict          as Map
import           Data.Map.Strict          (Map)
import           Data.Text                (Text)
import           System.FilePath          ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit

import           Vulkan.JSON.Bitmask      (Bitmask)
import           Vulkan.JSON.Command      (Command)
import           Vulkan.JSON.Enum         (Enum)
import           Vulkan.JSON.Extension    (Extension)
import           Vulkan.JSON.Flags        (Flags)
import           Vulkan.JSON.Format       (Format)
import           Vulkan.JSON.Handle       (Handle)
import           Vulkan.JSON.Spirv        (Spirv)
import           Vulkan.JSON.Struct       (Struct)
import           Vulkan.JSON.SyncAccess   (SyncAccess)
import           Vulkan.JSON.SyncStage    (SyncStage)
import           Vulkan.JSON.Version      (Version)
import qualified Vulkan.JSON.VulkanObject as VO
import           Vulkan.JSON.VulkanObject (VulkanObject)

jsonsDir :: FilePath
jsonsDir = "../jsons"

parses :: forall a. FromJSON a => FilePath -> Assertion
parses name = do
  r <- eitherDecodeFileStrict @a (jsonsDir </> name)
  case r of
    Left err -> assertFailure (name <> ": " <> err)
    Right _  -> pure ()

main :: IO ()
main = defaultMain $ testGroup "vulkan-json-types"
  [ testGroup "decodes every jsons/ file"
      [ testCase "vk.json"            $ parses @VulkanObject "vk.json"
      , testCase "vk-aliases.json"    $ parses @(Map Text (Map Text (Map Text (Maybe Text))))
                                                            "vk-aliases.json"
      , testCase "vk-bitmasks.json"   $ parses @(Map Text Bitmask)   "vk-bitmasks.json"
      , testCase "vk-commands.json"   $ parses @(Map Text Command)   "vk-commands.json"
      , testCase "vk-enums.json"      $ parses @(Map Text Enum)      "vk-enums.json"
      , testCase "vk-extensions.json" $ parses @(Map Text Extension) "vk-extensions.json"
      , testCase "vk-flags.json"      $ parses @(Map Text Flags)     "vk-flags.json"
      , testCase "vk-formats.json"    $ parses @(Map Text Format)    "vk-formats.json"
      , testCase "vk-handles.json"    $ parses @(Map Text Handle)    "vk-handles.json"
      , testCase "vk-spirv.json"      $ parses @[Spirv]              "vk-spirv.json"
      , testCase "vk-structs.json"    $ parses @(Map Text Struct)    "vk-structs.json"
      , testCase "vk-syncAccess.json" $ parses @[SyncAccess]         "vk-syncAccess.json"
      , testCase "vk-syncStage.json"  $ parses @[SyncStage]          "vk-syncStage.json"
      , testCase "vk-versions.json"   $ parses @(Map Text Version)   "vk-versions.json"
      ]
  , testCase "vk.json smoke check" $ do
      r <- eitherDecodeFileStrict @VulkanObject (jsonsDir </> "vk.json")
      case r of
        Left err -> assertFailure ("vk.json: " <> err)
        Right vo -> do
          assertEqual "headerVersionComplete" "1.4.351" (VO.headerVersionComplete vo)
          assertBool  "non-empty constants"     (not (Map.null (VO.constants vo)))
          assertBool  "non-empty funcPointers"  (not (Map.null (VO.funcPointers vo)))
          assertBool  "non-empty syncPipeline"  (not (null (VO.syncPipeline vo)))
          assertBool  "non-empty videoCodecs"   (not (Map.null (VO.videoCodecs vo)))
          assertBool  "non-empty platforms"     (not (Map.null (VO.platforms vo)))
          assertBool  "non-empty vendorTags"    (not (null (VO.vendorTags vo)))
  ]
