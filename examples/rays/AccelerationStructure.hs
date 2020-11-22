{-# LANGUAGE OverloadedLists #-}

module AccelerationStructure where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Coerce                    ( coerce )
import           Foreign.Storable               ( Storable(sizeOf) )
import           Foreign.Storable               ( Storable(poke) )
import           MonadVulkan
import           Say
import           UnliftIO.Foreign               ( castPtr )
import           Vulkan.CStruct
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core11                  ( MemoryRequirements2(..) )
import           Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
import           Vulkan.Extensions.VK_KHR_ray_tracing
import           Vulkan.Utils.QueueAssignment
import           Vulkan.Zero
import           VulkanMemoryAllocator

----------------------------------------------------------------
-- TLAS
----------------------------------------------------------------

createTLAS :: V (ReleaseKey, AccelerationStructureKHR)
createTLAS = do
  let geomType = zero { geometryType      = GEOMETRY_TYPE_INSTANCES_KHR
                      , maxPrimitiveCount = 1
                      , allowsTransforms  = False
                      }
      asci = zero
        { type'         = ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
        , flags         = BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR
        , geometryInfos = [geomType]
        }
  (tlasKey, tlas) <- withAccelerationStructureKHR' asci
  createAndBindAccelerationMemory tlas
  nameObject' tlas "TLAS"

  let numInstances = 1
      instanceDescsSize =
        numInstances * cStructSize @AccelerationStructureInstanceKHR
  (instBufferReleaseKey, (instBuffer, instBufferAllocation, _)) <- withBuffer'
    zero
      { usage = BUFFER_USAGE_RAY_TRACING_BIT_KHR
                  .|. BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
      , size  = fromIntegral instanceDescsSize
      }
    -- TODO: Make this GPU only and transfer to it
    zero
      { requiredFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT
                          .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
      }
  nameObject' instBuffer "TLAS instances"
  instBufferDeviceAddress <- getBufferDeviceAddress' zero { buffer = instBuffer
                                                          }

  (blasReleaseKey, blas) <- createBlasOnDevice
  blasAddress            <- getAccelerationStructureDeviceAddressKHR' zero
    { accelerationStructure = blas
    }
  let identity = TransformMatrixKHR ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0))
      inst :: AccelerationStructureInstanceKHR
      inst = zero
        { transform                      = identity
        , instanceCustomIndex            = complement 0 -- TODO: unnecessary
        , mask                           = complement 0
        , instanceShaderBindingTableRecordOffset = 0
        , flags = GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
        , accelerationStructureReference = coerce blasAddress
        }
  (instMapKey, instMapPtr) <- withMappedMemory' instBufferAllocation
  liftIO $ poke (castPtr @_ @AccelerationStructureInstanceKHR instMapPtr) inst
  release instMapKey

  let geomData = AccelerationStructureGeometryInstancesDataKHR
        { arrayOfPointers = False
        , data'           = DeviceAddressConst instBufferDeviceAddress
        }
      geom :: AccelerationStructureGeometryKHR
      geom = zero { geometryType = GEOMETRY_TYPE_INSTANCES_KHR
                  , geometry     = Instances geomData
                  , flags        = GEOMETRY_OPAQUE_BIT_KHR
                  }
  let offsetInfo = zero { primitiveCount = 1, primitiveOffset = 0 }

  buildTlasOnDevice tlas geom offsetInfo

  pure (tlasKey, tlas)

-- TODO: deduplicate this and buildBlasOnDevice
buildTlasOnDevice
  :: AccelerationStructureKHR
  -> AccelerationStructureGeometryKHR
  -> AccelerationStructureBuildOffsetInfoKHR
  -> V ()
buildTlasOnDevice tlas geom offsetInfo = do
  --
  -- Get the memory requirements for building the AS
  --
  MemoryRequirements2 () reqs <- getAccelerationStructureMemoryRequirementsKHR'
    zero
      { type'                 =
        ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR
      , buildType             = ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
      , accelerationStructure = tlas
      }

  --
  -- Allocate that space on the device
  --
  (scratchKey, (scratch, scratchAllocation, scratchAllocationInfo)) <-
    withBuffer'
      zero
        { usage = BUFFER_USAGE_RAY_TRACING_BIT_KHR
                    .|. BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
        , size  = size (reqs :: MemoryRequirements)
        }
      zero
  scratchAddr <- getBufferDeviceAddress' zero { buffer = scratch }

  --
  -- Create command buffers
  --
  graphicsQueue                             <- getGraphicsQueue
  QueueFamilyIndex graphicsQueueFamilyIndex <- getGraphicsQueueFamilyIndex
  (poolKey, commandPool)                    <- withCommandPool' zero
    { queueFamilyIndex = graphicsQueueFamilyIndex
    }
  ~[commandBuffer] <- allocateCommandBuffers' zero
    { commandPool        = commandPool
    , level              = COMMAND_BUFFER_LEVEL_PRIMARY
    , commandBufferCount = 1
    }

  let buildGeomInfo = zero { type' = ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
                           , update = False
                           , dstAccelerationStructure = tlas
                           , scratchData = DeviceAddress scratchAddr
                           , geometries = [geom]
                           }

  --
  -- Record and kick off the build commands
  --
  useCommandBuffer' commandBuffer zero $ do
    cmdBuildAccelerationStructureKHR' [SomeStruct buildGeomInfo] [offsetInfo]
  (fenceKey, fence) <- withFence' zero
  queueSubmit
    graphicsQueue
    [SomeStruct zero { commandBuffers = [commandBufferHandle commandBuffer] }]
    fence
  let oneSecond = 1e9
  waitForFencesSafe' [fence] True oneSecond >>= \case
    SUCCESS -> pure ()
    TIMEOUT -> error "Timed out building acceleration structure"
    _       -> error "blah"
  sayErr "Built acceleration structure"

  -- free our building resources
  release fenceKey
  release poolKey
  release scratchKey

----------------------------------------------------------------
-- BLAS
----------------------------------------------------------------

createBlasOnDevice :: V (ReleaseKey, AccelerationStructureKHR)
createBlasOnDevice = do
  --
  -- Create the AS
  --
  let asci :: AccelerationStructureCreateInfoKHR
      asci = zero
        { type'         = ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
        , flags         = BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR
        , geometryInfos = [ zero { geometryType      = GEOMETRY_TYPE_AABBS_KHR
                                 , maxPrimitiveCount = 1
                                 }
                          ]
        }
  (blasKey, blas) <- withAccelerationStructureKHR' asci
  createAndBindAccelerationMemory blas
  nameObject' blas "BLAS"

  (_, geom, offsetInfo) <- biunitBoxGeometry

  buildBlasOnDevice blas geom offsetInfo

  pure (blasKey, blas)

buildBlasOnDevice
  :: AccelerationStructureKHR
  -> AccelerationStructureGeometryKHR
  -> AccelerationStructureBuildOffsetInfoKHR
  -> V ()
buildBlasOnDevice blas geom offsetInfo = do
  --
  -- Get the memory requirements for building the AS
  --
  MemoryRequirements2 () reqs <- getAccelerationStructureMemoryRequirementsKHR'
    zero
      { type'                 =
        ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR
      , buildType             = ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
      , accelerationStructure = blas
      }

  --
  -- Allocate that space on the device
  --
  (scratchKey, (scratch, scratchAllocation, scratchAllocationInfo)) <-
    withBuffer'
      zero
        { usage = BUFFER_USAGE_RAY_TRACING_BIT_KHR
                    .|. BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
        , size  = size (reqs :: MemoryRequirements)
        }
      zero
  scratchAddr <- getBufferDeviceAddress' zero { buffer = scratch }

  --
  -- Create command buffers
  --
  graphicsQueue                             <- getGraphicsQueue
  QueueFamilyIndex graphicsQueueFamilyIndex <- getGraphicsQueueFamilyIndex
  (poolKey, commandPool)                    <- withCommandPool' zero
    { queueFamilyIndex = graphicsQueueFamilyIndex
    }
  ~[commandBuffer] <- allocateCommandBuffers' zero
    { commandPool        = commandPool
    , level              = COMMAND_BUFFER_LEVEL_PRIMARY
    , commandBufferCount = 1
    }

  let buildGeomInfo = zero
        { type' = ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
        , update                   = False
        , dstAccelerationStructure = blas
        , scratchData              = DeviceAddress scratchAddr
        , geometries               = [geom]
        }

  --
  -- Record and kick off the build commands
  --
  useCommandBuffer' commandBuffer zero $ do
    cmdBuildAccelerationStructureKHR' [SomeStruct buildGeomInfo] [offsetInfo]
  (fenceKey, fence) <- withFence' zero
  queueSubmit
    graphicsQueue
    [SomeStruct zero { commandBuffers = [commandBufferHandle commandBuffer] }]
    fence
  let oneSecond = 1e9
  waitForFencesSafe' [fence] True oneSecond >>= \case
    SUCCESS -> pure ()
    TIMEOUT -> error "Timed out building acceleration structure"
    _       -> error "blah"
  sayErr "Built acceleration structure"

  -- free our building resources
  release fenceKey
  release poolKey
  release scratchKey
  pure ()

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

--
-- The geometry for a biunit AABB on the device
--
biunitBoxGeometry
  :: V
       ( ReleaseKey
       , AccelerationStructureGeometryKHR
       , AccelerationStructureBuildOffsetInfoKHR
       )
biunitBoxGeometry = do
  let -- a bounding box for the unit sphere
      box = AabbPositionsKHR (-1) (-1) (-1) 1 1 1
  (bufferKey, (buffer, bufferAllocation, _)) <- withBuffer'
    zero
      { usage = BUFFER_USAGE_TRANSFER_SRC_BIT
                .|. BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
                .|. BUFFER_USAGE_RAY_TRACING_BIT_KHR
      , size  = fromIntegral (sizeOf box)
      }
    zero
      { requiredFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT
                          .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
      }
  (boxMapKey, boxMapPtr) <- withMappedMemory' bufferAllocation
  liftIO $ poke (castPtr boxMapPtr) box
  release boxMapKey
  boxAddr <- getBufferDeviceAddress' zero { buffer = buffer }
  let boxData = AccelerationStructureGeometryAabbsDataKHR
        { data'  = DeviceAddressConst boxAddr
        , stride = fromIntegral (sizeOf box)
        }
  let geom :: AccelerationStructureGeometryKHR
      geom = zero { geometryType = GEOMETRY_TYPE_AABBS_KHR
                  , flags        = GEOMETRY_OPAQUE_BIT_KHR
                  , geometry     = Aabbs boxData
                  }
  let offsetInfo = zero { primitiveCount = 1, primitiveOffset = 0 }
  pure (bufferKey, geom, offsetInfo)

createAndBindAccelerationMemory :: AccelerationStructureKHR -> V ()
createAndBindAccelerationMemory as = do
  MemoryRequirements2 () reqs <- getAccelerationStructureMemoryRequirementsKHR'
    zero { type' = ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR
         , buildType = ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
         , accelerationStructure = as
         }

  (releaseMem, (allocation, allocationInfo)) <- withMemory'
    reqs
    zero { usage         = MEMORY_USAGE_GPU_ONLY
         , requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT
         }

  let bindInfo :: BindAccelerationStructureMemoryInfoKHR
      bindInfo = zero
        { accelerationStructure = as
        , memory                = deviceMemory allocationInfo
        , memoryOffset          = offset (allocationInfo :: AllocationInfo)
        }
  bindAccelerationStructureMemoryKHR' [bindInfo]
