{-# LANGUAGE OverloadedLists #-}

module AccelerationStructure
  ( createTLAS
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Coerce (coerce)
import Data.Vector (Vector)
import Foreign.Storable (Storable (poke, sizeOf))
import Scene
import UnliftIO.Foreign (castPtr)
import Vulkan.CStruct
import Vulkan.CStruct.Extends
import Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
import Vulkan.Extensions.VK_KHR_acceleration_structure as AccelerationStructureDeviceAddressInfoKHR (AccelerationStructureDeviceAddressInfoKHR (..))
import qualified Vulkan.Extensions.VK_KHR_acceleration_structure as RT
import Vulkan.Utils.Debug (nameObject)
import Vulkan.Utils.QueueAssignment
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.VulkanContext (VulkanContext (..))
import Vulkan.Zero
import VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

----------------------------------------------------------------
-- TLAS
----------------------------------------------------------------

createTLAS
  :: (MonadResource m, MonadFail m)
  => VulkanContext
  -> VMA.Allocator
  -> SceneBuffers
  -> m (ReleaseKey, RT.AccelerationStructureKHR)
createTLAS vc vma sceneBuffers = do
  let dev = vcDevice vc
  (_blasReleaseKey, blas) <- createBLAS vc vma sceneBuffers
  blasAddress <-
    RT.getAccelerationStructureDeviceAddressKHR
      dev
      zero
        { AccelerationStructureDeviceAddressInfoKHR.accelerationStructure = blas
        }
  let
    identity = RT.TransformMatrixKHR (1, 0, 0, 0) (0, 1, 0, 0) (0, 0, 1, 0)
    inst :: RT.AccelerationStructureInstanceKHR
    inst =
      zero
        { RT.transform = identity
        , RT.instanceCustomIndex = 0
        , RT.mask = complement 0
        , RT.instanceShaderBindingTableRecordOffset = 0
        , RT.flags = RT.GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
        , RT.accelerationStructureReference = coerce blasAddress
        }

  let
    numInstances = 1
    instanceDescsSize =
      numInstances * cStructSize @RT.AccelerationStructureInstanceKHR
  (_instBufferReleaseKey, (instBuffer, instBufferAllocation, _)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.usage =
            Vk.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
              .|. Vk.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
        , Vk.size = fromIntegral instanceDescsSize
        }
      zero
        { VMA.requiredFlags =
            Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
              .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
        }
      allocate
  nameObject dev instBuffer "TLAS instances"
  instBufferDeviceAddress <-
    getBufferDeviceAddress
      dev
      zero
        { buffer = instBuffer
        }

  (instMapKey, instMapPtr) <- VMA.withMappedMemory vma instBufferAllocation allocate
  liftIO $ poke (castPtr @_ @RT.AccelerationStructureInstanceKHR instMapPtr) inst
  release instMapKey

  let
    buildGeometries =
      [ SomeStruct
          zero
            { RT.geometryType = RT.GEOMETRY_TYPE_INSTANCES_KHR
            , RT.geometry =
                RT.Instances
                  RT.AccelerationStructureGeometryInstancesDataKHR
                    { RT.arrayOfPointers = False
                    , RT.data' = RT.DeviceAddressConst instBufferDeviceAddress
                    }
            , RT.flags = RT.GEOMETRY_OPAQUE_BIT_KHR
            }
      ]
    buildInfo =
      zero
        { RT.type' = RT.ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
        , RT.mode = RT.BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR
        , RT.srcAccelerationStructure = Vk.NULL_HANDLE
        , RT.dstAccelerationStructure = Vk.NULL_HANDLE
        , RT.geometries = buildGeometries
        , RT.scratchData = zero
        }
    maxPrimitiveCounts = [1]
    rangeInfos = [zero{RT.primitiveCount = 1, RT.primitiveOffset = 0}]
  sizes <-
    RT.getAccelerationStructureBuildSizesKHR
      dev
      RT.ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
      buildInfo
      maxPrimitiveCounts

  (_tlasBufferKey, tlasKey, tlas) <-
    buildAccelerationStructure
      vc
      vma
      buildInfo
      rangeInfos
      sizes
  nameObject dev tlas "TLAS"
  pure (tlasKey, tlas)

buildAccelerationStructure
  :: (MonadResource m, MonadFail m)
  => VulkanContext
  -> VMA.Allocator
  -> RT.AccelerationStructureBuildGeometryInfoKHR
  -> Vector RT.AccelerationStructureBuildRangeInfoKHR
  -> RT.AccelerationStructureBuildSizesInfoKHR
  -> m (ReleaseKey, ReleaseKey, RT.AccelerationStructureKHR)
buildAccelerationStructure vc vma geom ranges sizes = do
  let
    dev = vcDevice vc
    bufferSize = RT.accelerationStructureSize sizes

  (asBufferKey, (asBuffer, _, _)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.size = bufferSize
        , Vk.usage = Vk.BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
        }
      zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
      allocate

  (scratchBufferKey, (scratchBuffer, _, _)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.size = RT.buildScratchSize sizes
        , Vk.usage = Vk.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
        }
      zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
      allocate
  scratchBufferDeviceAddress <-
    getBufferDeviceAddress
      dev
      zero
        { buffer = scratchBuffer
        }

  let asci =
        zero
          { RT.type' = RT.ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
          , RT.buffer = asBuffer
          , RT.offset = 0
          , RT.size = bufferSize
          }
  (asKey, as) <- RT.withAccelerationStructureKHR dev asci Nothing allocate

  oneShotComputeCommands vc $ \cmd ->
    RT.cmdBuildAccelerationStructuresKHR
      cmd
      [ geom
          { RT.dstAccelerationStructure = as
          , RT.scratchData = RT.DeviceAddress scratchBufferDeviceAddress
          }
      ]
      [ranges]

  release scratchBufferKey

  pure (asKey, asBufferKey, as)

createBLAS
  :: (MonadResource m, MonadFail m)
  => VulkanContext
  -> VMA.Allocator
  -> SceneBuffers
  -> m (ReleaseKey, RT.AccelerationStructureKHR)
createBLAS vc vma sceneBuffers = do
  let dev = vcDevice vc
  (sceneGeom, sceneOffsets) <- sceneGeometry vc sceneBuffers

  let
    buildInfo =
      zero
        { RT.type' = RT.ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
        , RT.mode = RT.BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR
        , RT.srcAccelerationStructure = Vk.NULL_HANDLE
        , RT.dstAccelerationStructure = Vk.NULL_HANDLE
        , RT.geometries = [SomeStruct sceneGeom]
        , RT.scratchData = zero
        }
    maxPrimitiveCounts = [sceneSize sceneBuffers]
  sizes <-
    RT.getAccelerationStructureBuildSizesKHR
      dev
      RT.ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
      buildInfo
      maxPrimitiveCounts

  (_blasBufferKey, blasKey, blas) <- buildAccelerationStructure vc vma buildInfo sceneOffsets sizes
  nameObject dev blas "BLAS"
  pure (blasKey, blas)

sceneGeometry
  :: (MonadIO m)
  => VulkanContext
  -> SceneBuffers
  -> m
       ( RT.AccelerationStructureGeometryKHR '[]
       , Vector RT.AccelerationStructureBuildRangeInfoKHR
       )
sceneGeometry vc SceneBuffers{..} = do
  boxAddr <- getBufferDeviceAddress (vcDevice vc) zero{buffer = sceneAabbs}
  let
    boxData =
      RT.AccelerationStructureGeometryAabbsDataKHR
        { data' = RT.DeviceAddressConst boxAddr
        , stride = fromIntegral (sizeOf (undefined :: RT.AabbPositionsKHR))
        }
    geom :: RT.AccelerationStructureGeometryKHR '[]
    geom =
      zero
        { RT.geometryType = RT.GEOMETRY_TYPE_AABBS_KHR
        , RT.flags = RT.GEOMETRY_OPAQUE_BIT_KHR
        , RT.geometry = RT.Aabbs boxData
        }
  let offsetInfo = [zero{RT.primitiveCount = sceneSize, RT.primitiveOffset = 0}]
  pure (geom, offsetInfo)

----------------------------------------------------------------
-- One-shot command submission for setup work
----------------------------------------------------------------

oneShotComputeCommands
  :: (MonadResource m, MonadFail m)
  => VulkanContext
  -> (Vk.CommandBuffer -> IO ())
  -> m ()
oneShotComputeCommands vc cmds = do
  let
    dev = vcDevice vc
    (QueueFamilyIndex graphicsQueueFamilyIndex, graphicsQueue) = qGraphics (vcQueues vc)
  (poolKey, commandPool) <- Vk.withCommandPool dev zero{CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex} Nothing allocate
  [commandBuffer] <-
    Vk.allocateCommandBuffers
      dev
      zero
        { Vk.commandPool = commandPool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }

  Vk.useCommandBuffer
    commandBuffer
    zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
    (liftIO (cmds commandBuffer))
  (fenceKey, fence) <- Vk.withFence dev zero Nothing allocate
  Vk.queueSubmit
    graphicsQueue
    [SomeStruct zero{Vk.commandBuffers = [Vk.commandBufferHandle commandBuffer]}]
    fence
  let oneSecond = 1e9
  Vk.waitForFencesSafe dev [fence] True oneSecond >>= \case
    Vk.SUCCESS -> pure ()
    Vk.TIMEOUT -> error "Timed out running one shot commands"
    _ -> error "Unhandled exit code in oneShotComputeCommands"
  release fenceKey
  release poolKey
