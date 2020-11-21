{-# LANGUAGE OverloadedLists #-}

module AccelerationStructure where

import           MonadVulkan
import           UnliftIO.Foreign               ( allocaBytesAligned
                                                , castPtr
                                                , withArray
                                                )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core11                  ( MemoryRequirements2(..) )
import           Vulkan.Extensions.VK_KHR_ray_tracing
import           Vulkan.Zero

createBLAS :: V ()
createBLAS = do
  let asci :: AccelerationStructureCreateInfoKHR
      asci = zero
        { type'         = ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
        , flags         = BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR
        , geometryInfos = [ zero { geometryType      = GEOMETRY_TYPE_AABBS_KHR
                                 , maxPrimitiveCount = 1
                                 }
                          ]
        }
  (blasKey, blas)             <- withAccelerationStructureKHR' asci
  MemoryRequirements2 () reqs <- getAccelerationStructureMemoryRequirementsKHR'
    zero
      { type'                 =
        ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR
      , buildType             = ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR
      , accelerationStructure = blas
      }
  let -- a bounding box for the unit sphere
      box = AabbPositionsKHR (-1) (-1) (-1) 1 1 1
  withArray [box] $ \boxData -> do
    allocaBytesAligned (fromIntegral (size (reqs :: MemoryRequirements)))
                       (fromIntegral (alignment reqs))
      $ \scratch -> do
          let
            infos =
              [ SomeStruct zero
                  { type' = ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
                  , update                   = False
                  , dstAccelerationStructure = blas
                  , scratchData              = HostAddress scratch
                  , geometries               =
                    [ zero
                        { geometryType = GEOMETRY_TYPE_AABBS_KHR
                        , flags        = GEOMETRY_OPAQUE_BIT_KHR
                        , geometry     =
                          Aabbs $ AccelerationStructureGeometryAabbsDataKHR
                            { data'  = HostAddressConst (castPtr boxData)
                            , stride = 0
                            }
                        }
                    ]
                  }
              ]
            offsetInfos = [zero { primitiveCount = 1, primitiveOffset = 0 }]
          buildAccelerationStructureKHR' infos offsetInfos >>= \case
            SUCCESS -> pure ()
            _       -> error "oops"
          pure ()
