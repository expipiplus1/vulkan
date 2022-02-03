{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_synchronization2"
module Vulkan.Core13.Promoted_From_VK_KHR_synchronization2  ( BufferMemoryBarrier2
                                                            , CommandBufferSubmitInfo
                                                            , DependencyInfo
                                                            , ImageMemoryBarrier2
                                                            , MemoryBarrier2
                                                            , PhysicalDeviceSynchronization2Features
                                                            , SemaphoreSubmitInfo
                                                            , SubmitInfo2
                                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BufferMemoryBarrier2

instance ToCStruct BufferMemoryBarrier2
instance Show BufferMemoryBarrier2

instance FromCStruct BufferMemoryBarrier2


data CommandBufferSubmitInfo

instance ToCStruct CommandBufferSubmitInfo
instance Show CommandBufferSubmitInfo

instance FromCStruct CommandBufferSubmitInfo


data DependencyInfo

instance ToCStruct DependencyInfo
instance Show DependencyInfo

instance FromCStruct DependencyInfo


data ImageMemoryBarrier2

instance ToCStruct ImageMemoryBarrier2
instance Show ImageMemoryBarrier2

instance FromCStruct ImageMemoryBarrier2


data MemoryBarrier2

instance ToCStruct MemoryBarrier2
instance Show MemoryBarrier2

instance FromCStruct MemoryBarrier2


data PhysicalDeviceSynchronization2Features

instance ToCStruct PhysicalDeviceSynchronization2Features
instance Show PhysicalDeviceSynchronization2Features

instance FromCStruct PhysicalDeviceSynchronization2Features


data SemaphoreSubmitInfo

instance ToCStruct SemaphoreSubmitInfo
instance Show SemaphoreSubmitInfo

instance FromCStruct SemaphoreSubmitInfo


type role SubmitInfo2 nominal
data SubmitInfo2 (es :: [Type])

instance (Extendss SubmitInfo2 es, PokeChain es) => ToCStruct (SubmitInfo2 es)
instance Show (Chain es) => Show (SubmitInfo2 es)

instance (Extendss SubmitInfo2 es, PeekChain es) => FromCStruct (SubmitInfo2 es)

