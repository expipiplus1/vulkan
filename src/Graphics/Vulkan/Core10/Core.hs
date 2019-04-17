{-# language Strict #-}
{-# language CPP #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.Core
  ( Format
  , ObjectType
  , Result
  , StructureType
  , bool32ToBool
  , boolToBool32
  , Zero(..)
  ) where




import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , pattern VK_FALSE
  , pattern VK_TRUE
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )


-- No documentation found for TopLevel "Format"
type Format = VkFormat
-- No documentation found for TopLevel "ObjectType"
type ObjectType = VkObjectType
-- No documentation found for TopLevel "Result"
type Result = VkResult
-- No documentation found for TopLevel "StructureType"
type StructureType = VkStructureType
bool32ToBool :: VkBool32 -> Bool
bool32ToBool = \case
  VK_FALSE -> False
  VK_TRUE  -> True
  -- TODO: add pattern totality
  _        -> error "unhandled VkBool32 Value"

boolToBool32 :: Bool -> VkBool32
boolToBool32 = \case
  False -> VK_FALSE
  True  -> VK_TRUE
