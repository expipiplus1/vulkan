{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module VkCtx (vkCtx) where

import qualified Data.Map                      as Map
import qualified Language.C.Inline             as C
import qualified Language.C.Inline.Context     as C
import qualified Language.C.Types              as C
import qualified Vulkan                        as Vk
import           Language.Haskell.TH

vkCtx :: C.Context
vkCtx = mempty { C.ctxTypesTable = Map.fromList ts }
 where
  ts =
    [ (C.TypeName "VkExtent3D", [t| Vk.Extent3D |])
    , ( C.TypeName "VkAccelerationStructureInstanceKHR"
      , [t| Vk.AccelerationStructureInstanceKHR |]
      )
    , (C.TypeName "VkApplicationInfo", [t| Vk.ApplicationInfo |])
    , ( C.TypeName "VkBindBufferMemoryDeviceGroupInfo"
      , [t| Vk.BindBufferMemoryDeviceGroupInfo |]
      )
    ]
