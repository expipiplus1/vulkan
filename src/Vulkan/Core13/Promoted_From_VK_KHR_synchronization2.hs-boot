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
type role BufferMemoryBarrier2 nominal
data BufferMemoryBarrier2 (es :: [Type])

instance ( Extendss BufferMemoryBarrier2 es
         , PokeChain es ) => ToCStruct (BufferMemoryBarrier2 es)
instance Show (Chain es) => Show (BufferMemoryBarrier2 es)

instance ( Extendss BufferMemoryBarrier2 es
         , PeekChain es ) => FromCStruct (BufferMemoryBarrier2 es)


type role CommandBufferSubmitInfo nominal
data CommandBufferSubmitInfo (es :: [Type])

instance ( Extendss CommandBufferSubmitInfo es
         , PokeChain es ) => ToCStruct (CommandBufferSubmitInfo es)
instance Show (Chain es) => Show (CommandBufferSubmitInfo es)

instance ( Extendss CommandBufferSubmitInfo es
         , PeekChain es ) => FromCStruct (CommandBufferSubmitInfo es)


data DependencyInfo

instance ToCStruct DependencyInfo
instance Show DependencyInfo

instance FromCStruct DependencyInfo


type role ImageMemoryBarrier2 nominal
data ImageMemoryBarrier2 (es :: [Type])

instance ( Extendss ImageMemoryBarrier2 es
         , PokeChain es ) => ToCStruct (ImageMemoryBarrier2 es)
instance Show (Chain es) => Show (ImageMemoryBarrier2 es)

instance ( Extendss ImageMemoryBarrier2 es
         , PeekChain es ) => FromCStruct (ImageMemoryBarrier2 es)


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

