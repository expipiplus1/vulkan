{-# language CPP #-}
module Graphics.Vulkan.Core10  ( pattern API_VERSION_1_0
                               , module Graphics.Vulkan.Core10.APIConstants
                               , module Graphics.Vulkan.Core10.AllocationCallbacks
                               , module Graphics.Vulkan.Core10.BaseType
                               , module Graphics.Vulkan.Core10.Buffer
                               , module Graphics.Vulkan.Core10.BufferView
                               , module Graphics.Vulkan.Core10.CommandBuffer
                               , module Graphics.Vulkan.Core10.CommandBufferBuilding
                               , module Graphics.Vulkan.Core10.CommandPool
                               , module Graphics.Vulkan.Core10.DescriptorSet
                               , module Graphics.Vulkan.Core10.Device
                               , module Graphics.Vulkan.Core10.DeviceInitialization
                               , module Graphics.Vulkan.Core10.Enums
                               , module Graphics.Vulkan.Core10.Event
                               , module Graphics.Vulkan.Core10.ExtensionDiscovery
                               , module Graphics.Vulkan.Core10.Fence
                               , module Graphics.Vulkan.Core10.FuncPointers
                               , module Graphics.Vulkan.Core10.Handles
                               , module Graphics.Vulkan.Core10.Image
                               , module Graphics.Vulkan.Core10.ImageView
                               , module Graphics.Vulkan.Core10.LayerDiscovery
                               , module Graphics.Vulkan.Core10.Memory
                               , module Graphics.Vulkan.Core10.MemoryManagement
                               , module Graphics.Vulkan.Core10.OtherTypes
                               , module Graphics.Vulkan.Core10.Pass
                               , module Graphics.Vulkan.Core10.Pipeline
                               , module Graphics.Vulkan.Core10.PipelineCache
                               , module Graphics.Vulkan.Core10.PipelineLayout
                               , module Graphics.Vulkan.Core10.Query
                               , module Graphics.Vulkan.Core10.Queue
                               , module Graphics.Vulkan.Core10.QueueSemaphore
                               , module Graphics.Vulkan.Core10.Sampler
                               , module Graphics.Vulkan.Core10.Shader
                               , module Graphics.Vulkan.Core10.SharedTypes
                               , module Graphics.Vulkan.Core10.SparseResourceMemoryManagement
                               ) where
import Graphics.Vulkan.Core10.APIConstants
import Graphics.Vulkan.Core10.AllocationCallbacks
import Graphics.Vulkan.Core10.BaseType
import Graphics.Vulkan.Core10.Buffer
import Graphics.Vulkan.Core10.BufferView
import Graphics.Vulkan.Core10.CommandBuffer
import Graphics.Vulkan.Core10.CommandBufferBuilding
import Graphics.Vulkan.Core10.CommandPool
import Graphics.Vulkan.Core10.DescriptorSet
import Graphics.Vulkan.Core10.Device
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Enums
import Graphics.Vulkan.Core10.Event
import Graphics.Vulkan.Core10.ExtensionDiscovery
import Graphics.Vulkan.Core10.Fence
import Graphics.Vulkan.Core10.FuncPointers
import Graphics.Vulkan.Core10.Handles
import Graphics.Vulkan.Core10.Image
import Graphics.Vulkan.Core10.ImageView
import Graphics.Vulkan.Core10.LayerDiscovery
import Graphics.Vulkan.Core10.Memory
import Graphics.Vulkan.Core10.MemoryManagement
import Graphics.Vulkan.Core10.OtherTypes
import Graphics.Vulkan.Core10.Pass
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.PipelineCache
import Graphics.Vulkan.Core10.PipelineLayout
import Graphics.Vulkan.Core10.Query
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.Core10.QueueSemaphore
import Graphics.Vulkan.Core10.Sampler
import Graphics.Vulkan.Core10.Shader
import Graphics.Vulkan.Core10.SharedTypes
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
import Data.Word (Word32)
import Graphics.Vulkan.Version (pattern MAKE_VERSION)
pattern API_VERSION_1_0 :: Word32
pattern API_VERSION_1_0 = MAKE_VERSION 1 0 0

