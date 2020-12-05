{-# LANGUAGE OverloadedLists #-}

module AccelerationStructure where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Coerce                    ( coerce )
import           Data.Vector                    ( Vector )
import           Foreign.Storable               ( Storable(poke, sizeOf) )
import           HasVulkan
import           MonadVulkan
import           Scene
import           UnliftIO.Foreign               ( castPtr )
import           Vulkan.CStruct
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
import           Vulkan.Extensions.VK_KHR_acceleration_structure
import           Vulkan.Utils.QueueAssignment
import           Vulkan.Zero
import           VulkanMemoryAllocator          ( AllocationCreateInfo
                                                  ( requiredFlags
                                                  , usage
                                                  )
                                                , MemoryUsage
                                                  ( MEMORY_USAGE_GPU_ONLY
                                                  )
                                                )

----------------------------------------------------------------
-- TLAS
----------------------------------------------------------------

createTLAS :: SceneBuffers -> V (ReleaseKey, AccelerationStructureKHR)
createTLAS sceneBuffers = do
  --
  -- Create the bottom level accelerationStructures
  --
  (_blasReleaseKey, blas) <- createBLAS sceneBuffers
  blasAddress             <- getAccelerationStructureDeviceAddressKHR' zero
    { accelerationStructure = blas
    }
  let identity = TransformMatrixKHR (1, 0, 0, 0) (0, 1, 0, 0) (0, 0, 1, 0)
      inst :: AccelerationStructureInstanceKHR
      inst = zero
        { transform                      = identity
        , instanceCustomIndex            = 0
        , mask                           = complement 0
        , instanceShaderBindingTableRecordOffset = 0
        , flags = GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
        , accelerationStructureReference = coerce blasAddress
        }

  --
  -- Create the buffer for the top level instances
  --
  let numInstances = 1
      instanceDescsSize =
        numInstances * cStructSize @AccelerationStructureInstanceKHR
  (_instBufferReleaseKey, (instBuffer, instBufferAllocation, _)) <- withBuffer'
    zero
      { usage =
        BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
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

  --
  -- populate the instance buffer
  --
  (instMapKey, instMapPtr) <- withMappedMemory' instBufferAllocation
  liftIO $ poke (castPtr @_ @AccelerationStructureInstanceKHR instMapPtr) inst
  release instMapKey

  let buildGeometries =
        [ zero
            { geometryType = GEOMETRY_TYPE_INSTANCES_KHR
            , geometry = Instances AccelerationStructureGeometryInstancesDataKHR
                           { arrayOfPointers = False
                           , data' = DeviceAddressConst instBufferDeviceAddress
                           }
            , flags = GEOMETRY_OPAQUE_BIT_KHR
            }
        ]
      buildInfo = zero { type' = ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
                       , mode = BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR -- ignored but used later
                       , srcAccelerationStructure = NULL_HANDLE -- ignored
                       , dstAccelerationStructure = NULL_HANDLE -- ignored
                       , geometries = buildGeometries
                       , scratchData = zero
                       }
      maxPrimitiveCounts = [1]
      rangeInfos         = [zero { primitiveCount = 1, primitiveOffset = 0 }]
  sizes <- getAccelerationStructureBuildSizesKHR'
    ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
    buildInfo
    maxPrimitiveCounts

  (_tlasBufferKey, tlasKey, tlas) <- buildAccelerationStructure buildInfo
                                                                rangeInfos
                                                                sizes
  nameObject' tlas "TLAS"
  pure (tlasKey, tlas)

buildAccelerationStructure
  :: AccelerationStructureBuildGeometryInfoKHR
  -> Vector AccelerationStructureBuildRangeInfoKHR
  -> AccelerationStructureBuildSizesInfoKHR
  -> V (ReleaseKey, ReleaseKey, AccelerationStructureKHR)
buildAccelerationStructure geom ranges sizes = do
  --
  -- Allocate the buffer to hold the acceleration structure
  --
  let bufferSize = accelerationStructureSize sizes
  (asBufferKey, (asBuffer, _, _)) <- withBuffer'
    zero { size  = bufferSize
         , usage = BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
         }
    zero { usage = MEMORY_USAGE_GPU_ONLY }

  --
  -- Allocate scratch space for building
  --
  (scratchBufferKey, (scratchBuffer, _, _)) <- withBuffer'
    zero { size  = buildScratchSize sizes
         , usage = BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
         }
    zero { usage = MEMORY_USAGE_GPU_ONLY }
  scratchBufferDeviceAddress <- getBufferDeviceAddress' zero
    { buffer = scratchBuffer
    }

  let asci = zero { type'  = ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
                  , buffer = asBuffer
                  , offset = 0
                  , size   = bufferSize
                  }
  (asKey, as) <- withAccelerationStructureKHR' asci

  oneShotComputeCommands $ do
    cmdBuildAccelerationStructuresKHR'
      [ geom { dstAccelerationStructure = as
             , scratchData = DeviceAddress scratchBufferDeviceAddress
             }
      ]
      [ranges]

  release scratchBufferKey

  pure (asKey, asBufferKey, as)

--
-- Create the bottom level acceleration structure
--
createBLAS :: SceneBuffers -> V (ReleaseKey, AccelerationStructureKHR)
createBLAS sceneBuffers = do
  (sceneGeom, sceneOffsets) <- sceneGeometry sceneBuffers

  let buildInfo = zero { type' = ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
                       , mode = BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR -- ignored but used later
                       , srcAccelerationStructure = NULL_HANDLE -- ignored
                       , dstAccelerationStructure = NULL_HANDLE -- ignored
                       , geometries = [sceneGeom]
                       , scratchData = zero
                       }
      maxPrimitiveCounts = [sceneSize sceneBuffers]
  sizes <- getAccelerationStructureBuildSizesKHR'
    ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
    buildInfo
    maxPrimitiveCounts

  (_blasBufferKey, blasKey, blas) <- buildAccelerationStructure buildInfo
                                                                sceneOffsets
                                                                sizes
  nameObject' blas "BLAS"
  pure (blasKey, blas)

sceneGeometry
  :: SceneBuffers
  -> V
       ( AccelerationStructureGeometryKHR
       , Vector AccelerationStructureBuildRangeInfoKHR
       )
sceneGeometry SceneBuffers {..} = do
  boxAddr <- getBufferDeviceAddress' zero { buffer = sceneAabbs }
  let boxData = AccelerationStructureGeometryAabbsDataKHR
        { data'  = DeviceAddressConst boxAddr
        , stride = fromIntegral (sizeOf (undefined :: AabbPositionsKHR))
        }
      geom :: AccelerationStructureGeometryKHR
      geom = zero { geometryType = GEOMETRY_TYPE_AABBS_KHR
                  , flags        = GEOMETRY_OPAQUE_BIT_KHR
                  , geometry     = Aabbs boxData
                  }
  let offsetInfo = [zero { primitiveCount = sceneSize, primitiveOffset = 0 }]
  pure (geom, offsetInfo)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- TODO: use compute queue here
oneShotComputeCommands :: CmdT V () -> V ()
oneShotComputeCommands cmds = do
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

  --
  -- Record and kick off the build commands
  --
  useCommandBuffer' commandBuffer
                    zero { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
                    cmds
  (fenceKey, fence) <- withFence' zero
  queueSubmit
    graphicsQueue
    [SomeStruct zero { commandBuffers = [commandBufferHandle commandBuffer] }]
    fence
  let oneSecond = 1e9
  waitForFencesSafe' [fence] True oneSecond >>= \case
    SUCCESS -> pure ()
    TIMEOUT -> error "Timed out running one shot commands"
    _       -> error "Unhandled exit code in oneShotComputeCommands"
  release fenceKey
  release poolKey
