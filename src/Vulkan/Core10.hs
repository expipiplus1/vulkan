{-# language CPP #-}
-- No documentation found for Chapter "Core10"
module Vulkan.Core10  ( pattern API_VERSION_1_0
                      , module Vulkan.Core10.APIConstants
                      , module Vulkan.Core10.AllocationCallbacks
                      , module Vulkan.Core10.Buffer
                      , module Vulkan.Core10.BufferView
                      , module Vulkan.Core10.CommandBuffer
                      , module Vulkan.Core10.CommandBufferBuilding
                      , module Vulkan.Core10.CommandPool
                      , module Vulkan.Core10.DescriptorSet
                      , module Vulkan.Core10.Device
                      , module Vulkan.Core10.DeviceInitialization
                      , module Vulkan.Core10.Enums
                      , module Vulkan.Core10.Event
                      , module Vulkan.Core10.ExtensionDiscovery
                      , module Vulkan.Core10.Fence
                      , module Vulkan.Core10.FuncPointers
                      , module Vulkan.Core10.FundamentalTypes
                      , module Vulkan.Core10.Handles
                      , module Vulkan.Core10.Image
                      , module Vulkan.Core10.ImageView
                      , module Vulkan.Core10.LayerDiscovery
                      , module Vulkan.Core10.Memory
                      , module Vulkan.Core10.MemoryManagement
                      , module Vulkan.Core10.OtherTypes
                      , module Vulkan.Core10.Pass
                      , module Vulkan.Core10.Pipeline
                      , module Vulkan.Core10.PipelineCache
                      , module Vulkan.Core10.PipelineLayout
                      , module Vulkan.Core10.Query
                      , module Vulkan.Core10.Queue
                      , module Vulkan.Core10.QueueSemaphore
                      , module Vulkan.Core10.Sampler
                      , module Vulkan.Core10.Shader
                      , module Vulkan.Core10.SparseResourceMemoryManagement
                      ) where
import Vulkan.Core10.APIConstants
import Vulkan.Core10.AllocationCallbacks
import Vulkan.Core10.Buffer
import Vulkan.Core10.BufferView
import Vulkan.Core10.CommandBuffer
import Vulkan.Core10.CommandBufferBuilding
import Vulkan.Core10.CommandPool
import Vulkan.Core10.DescriptorSet
import Vulkan.Core10.Device
import Vulkan.Core10.DeviceInitialization
import Vulkan.Core10.Enums
import Vulkan.Core10.Event
import Vulkan.Core10.ExtensionDiscovery
import Vulkan.Core10.Fence
import Vulkan.Core10.FuncPointers
import Vulkan.Core10.FundamentalTypes
import Vulkan.Core10.Handles
import Vulkan.Core10.Image
import Vulkan.Core10.ImageView
import Vulkan.Core10.LayerDiscovery
import Vulkan.Core10.Memory
import Vulkan.Core10.MemoryManagement
import Vulkan.Core10.OtherTypes
import Vulkan.Core10.Pass
import Vulkan.Core10.Pipeline
import Vulkan.Core10.PipelineCache
import Vulkan.Core10.PipelineLayout
import Vulkan.Core10.Query
import Vulkan.Core10.Queue
import Vulkan.Core10.QueueSemaphore
import Vulkan.Core10.Sampler
import Vulkan.Core10.Shader
import Vulkan.Core10.SparseResourceMemoryManagement
import Data.Word (Word32)
import Vulkan.Version (pattern MAKE_VERSION)
pattern API_VERSION_1_0 :: Word32
pattern API_VERSION_1_0 = MAKE_VERSION 1 0 0

