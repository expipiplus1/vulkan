{-# LANGUAGE OverloadedLists #-}

module AccelerationStructure
  ( createTLAS
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Coerce                    ( coerce )
import           Data.Vector                    ( Vector )
import           Foreign.Storable               ( Storable(poke, sizeOf) )
import           Scene
import           UnliftIO.Foreign               ( castPtr )
import           VkResources                    ( Queues(..)
                                                , VkResources(..)
                                                )
import           Vulkan.CStruct
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
import           Vulkan.Extensions.VK_KHR_acceleration_structure
import           Vulkan.Utils.Debug             ( nameObject )
import           Vulkan.Utils.QueueAssignment
import           Vulkan.Zero
import           VulkanMemoryAllocator         as VMA
                                         hiding ( getPhysicalDeviceProperties )

----------------------------------------------------------------
-- TLAS
----------------------------------------------------------------

createTLAS
  :: (MonadResource m, MonadFail m)
  => VkResources
  -> SceneBuffers
  -> m (ReleaseKey, AccelerationStructureKHR)
createTLAS vr sceneBuffers = do
  let dev = vrDevice vr
      vma = vrAllocator vr
  --
  -- Create the bottom level acceleration structure.
  --
  (_blasReleaseKey, blas) <- createBLAS vr sceneBuffers
  blasAddress             <- getAccelerationStructureDeviceAddressKHR dev zero
    { accelerationStructure = blas
    }
  let identity = TransformMatrixKHR (1, 0, 0, 0) (0, 1, 0, 0) (0, 0, 1, 0)
      inst :: AccelerationStructureInstanceKHR
      inst = zero
        { transform                              = identity
        , instanceCustomIndex                    = 0
        , mask                                   = complement 0
        , instanceShaderBindingTableRecordOffset = 0
        , flags = GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
        , accelerationStructureReference         = coerce blasAddress
        }

  let numInstances = 1
      instanceDescsSize =
        numInstances * cStructSize @AccelerationStructureInstanceKHR
  (_instBufferReleaseKey, (instBuffer, instBufferAllocation, _)) <- VMA.withBuffer
    vma
    zero
      { usage =
          BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
            .|. BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
      , size  = fromIntegral instanceDescsSize
      }
    zero
      { requiredFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT
                          .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
      }
    allocate
  nameObject dev instBuffer "TLAS instances"
  instBufferDeviceAddress <- getBufferDeviceAddress dev zero
    { buffer = instBuffer
    }

  (instMapKey, instMapPtr) <- VMA.withMappedMemory vma instBufferAllocation allocate
  liftIO $ poke (castPtr @_ @AccelerationStructureInstanceKHR instMapPtr) inst
  release instMapKey

  let buildGeometries =
        [ SomeStruct zero
            { geometryType = GEOMETRY_TYPE_INSTANCES_KHR
            , geometry = Instances AccelerationStructureGeometryInstancesDataKHR
                           { arrayOfPointers = False
                           , data' = DeviceAddressConst instBufferDeviceAddress
                           }
            , flags = GEOMETRY_OPAQUE_BIT_KHR
            }
        ]
      buildInfo = zero
        { type'                    = ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
        , mode                     = BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR
        , srcAccelerationStructure = NULL_HANDLE
        , dstAccelerationStructure = NULL_HANDLE
        , geometries               = buildGeometries
        , scratchData              = zero
        }
      maxPrimitiveCounts = [1]
      rangeInfos         = [zero { primitiveCount = 1, primitiveOffset = 0 }]
  sizes <- getAccelerationStructureBuildSizesKHR
    dev
    ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
    buildInfo
    maxPrimitiveCounts

  (_tlasBufferKey, tlasKey, tlas) <- buildAccelerationStructure
    vr
    buildInfo
    rangeInfos
    sizes
  nameObject dev tlas "TLAS"
  pure (tlasKey, tlas)

buildAccelerationStructure
  :: (MonadResource m, MonadFail m)
  => VkResources
  -> AccelerationStructureBuildGeometryInfoKHR
  -> Vector AccelerationStructureBuildRangeInfoKHR
  -> AccelerationStructureBuildSizesInfoKHR
  -> m (ReleaseKey, ReleaseKey, AccelerationStructureKHR)
buildAccelerationStructure vr geom ranges sizes = do
  let dev = vrDevice vr
      vma = vrAllocator vr
      bufferSize = accelerationStructureSize sizes

  (asBufferKey, (asBuffer, _, _)) <- VMA.withBuffer
    vma
    zero { size  = bufferSize
         , usage = BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
         }
    zero { usage = MEMORY_USAGE_GPU_ONLY }
    allocate

  (scratchBufferKey, (scratchBuffer, _, _)) <- VMA.withBuffer
    vma
    zero { size  = buildScratchSize sizes
         , usage = BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
         }
    zero { usage = MEMORY_USAGE_GPU_ONLY }
    allocate
  scratchBufferDeviceAddress <- getBufferDeviceAddress dev zero
    { buffer = scratchBuffer
    }

  let asci = zero { type'  = ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
                  , buffer = asBuffer
                  , offset = 0
                  , size   = bufferSize
                  }
  (asKey, as) <- withAccelerationStructureKHR dev asci Nothing allocate

  oneShotComputeCommands vr $ \cmd ->
    cmdBuildAccelerationStructuresKHR
      cmd
      [ geom { dstAccelerationStructure = as
             , scratchData = DeviceAddress scratchBufferDeviceAddress
             }
      ]
      [ranges]

  release scratchBufferKey

  pure (asKey, asBufferKey, as)

createBLAS
  :: (MonadResource m, MonadFail m)
  => VkResources
  -> SceneBuffers
  -> m (ReleaseKey, AccelerationStructureKHR)
createBLAS vr sceneBuffers = do
  let dev = vrDevice vr
  (sceneGeom, sceneOffsets) <- sceneGeometry vr sceneBuffers

  let buildInfo = zero
        { type'                    = ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
        , mode                     = BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR
        , srcAccelerationStructure = NULL_HANDLE
        , dstAccelerationStructure = NULL_HANDLE
        , geometries               = [SomeStruct sceneGeom]
        , scratchData              = zero
        }
      maxPrimitiveCounts = [sceneSize sceneBuffers]
  sizes <- getAccelerationStructureBuildSizesKHR
    dev
    ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
    buildInfo
    maxPrimitiveCounts

  (_blasBufferKey, blasKey, blas) <- buildAccelerationStructure
    vr
    buildInfo
    sceneOffsets
    sizes
  nameObject dev blas "BLAS"
  pure (blasKey, blas)

sceneGeometry
  :: MonadIO m
  => VkResources
  -> SceneBuffers
  -> m
       ( AccelerationStructureGeometryKHR '[]
       , Vector AccelerationStructureBuildRangeInfoKHR
       )
sceneGeometry vr SceneBuffers {..} = do
  boxAddr <- getBufferDeviceAddress (vrDevice vr) zero { buffer = sceneAabbs }
  let boxData = AccelerationStructureGeometryAabbsDataKHR
        { data'  = DeviceAddressConst boxAddr
        , stride = fromIntegral (sizeOf (undefined :: AabbPositionsKHR))
        }
      geom :: AccelerationStructureGeometryKHR '[]
      geom = zero { geometryType = GEOMETRY_TYPE_AABBS_KHR
                  , flags        = GEOMETRY_OPAQUE_BIT_KHR
                  , geometry     = Aabbs boxData
                  }
  let offsetInfo = [zero { primitiveCount = sceneSize, primitiveOffset = 0 }]
  pure (geom, offsetInfo)

----------------------------------------------------------------
-- One-shot command submission for setup work
----------------------------------------------------------------

oneShotComputeCommands
  :: (MonadResource m, MonadFail m)
  => VkResources
  -> (CommandBuffer -> IO ())
  -> m ()
oneShotComputeCommands vr cmds = do
  let dev = vrDevice vr
      (QueueFamilyIndex graphicsQueueFamilyIndex, graphicsQueue) =
        qGraphics (vrQueues vr)
  (poolKey, commandPool) <- withCommandPool
    dev
    zero { queueFamilyIndex = graphicsQueueFamilyIndex }
    Nothing
    allocate
  ~[commandBuffer] <- allocateCommandBuffers
    dev
    zero { commandPool        = commandPool
         , level              = COMMAND_BUFFER_LEVEL_PRIMARY
         , commandBufferCount = 1
         }

  useCommandBuffer commandBuffer
                   zero { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
                   (liftIO (cmds commandBuffer))
  (fenceKey, fence) <- withFence dev zero Nothing allocate
  queueSubmit
    graphicsQueue
    [SomeStruct zero { commandBuffers = [commandBufferHandle commandBuffer] }]
    fence
  let oneSecond = 1e9
  waitForFencesSafe dev [fence] True oneSecond >>= \case
    SUCCESS -> pure ()
    TIMEOUT -> error "Timed out running one shot commands"
    _       -> error "Unhandled exit code in oneShotComputeCommands"
  release fenceKey
  release poolKey
