{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Structs where

import           Data.Bits
import qualified Language.C.Inline             as C
import           Test.Tasty.HUnit
import           VkCtx
import qualified Vulkan                        as Vk
import           Vulkan.Zero

C.context (C.baseCtx <> vkCtx)
C.include "<vulkan/vulkan.h>"
C.include "<string.h>"

unit_simple :: IO ()
unit_simple = do
  let e = Vk.Extent3D 2 3 5
  Vk.withCStruct e $ \p -> do
    [C.block| void {
      VkExtent3D *p = $(VkExtent3D *p);
      p->width *= 2;
      p->height *= 2;
      p->depth *= 2;
    } |]
    e' <- Vk.peekCStruct p
    e' @?= Vk.Extent3D 4 6 10

unit_bitfield :: IO ()
unit_bitfield = do
  let e = Vk.AccelerationStructureInstanceKHR
        { transform                      = zero
        , instanceCustomIndex            = 2
        , mask                           = 3
        , instanceShaderBindingTableRecordOffset = 5
        , flags = Vk.GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR
        , accelerationStructureReference = 11
        }
  Vk.withCStruct e $ \p -> do
    [C.block| void {
      VkAccelerationStructureInstanceKHR *p = $(VkAccelerationStructureInstanceKHR *p);
      p->instanceCustomIndex *= 2;
      p->mask *= 2;
      p->instanceShaderBindingTableRecordOffset *= 2;
      p->flags *= 2;
      p->accelerationStructureReference *= 2;
    } |]
    e' <- Vk.peekCStruct p
    show e' @?= show
      (Vk.AccelerationStructureInstanceKHR
        zero
        4
        6
        10
        (Vk.GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR `shiftL` 1)
        22
      )

unit_strings :: IO ()
unit_strings = do
  let e = Vk.ApplicationInfo { applicationName    = Just "Hello"
                             , applicationVersion = 2
                             , engineName         = Just "World"
                             , engineVersion      = 3
                             , apiVersion         = Vk.MAKE_API_VERSION 5 7 11
                             }
  Vk.withCStruct e $ \p -> do
    b <- [C.block| int {
      VkApplicationInfo *p = $(VkApplicationInfo *p);
      if(strcmp(p->pApplicationName, "Hello") || strcmp(p->pEngineName, "World")){
        return 0;
      }
      p->pApplicationName = "Foo";
      p->applicationVersion *= 2;
      p->pEngineName = "Bar";
      p->engineVersion *= 2;
      p->apiVersion = VK_MAKE_VERSION(13,17,19);
      return 1;
    } |]
    assertBool "Read strings on C side" (b /= 0)
    e' <- Vk.peekCStruct p
    show e' @?= show
      (Vk.ApplicationInfo (Just "Foo")
                          4
                          (Just "Bar")
                          6
                          (Vk.MAKE_API_VERSION 13 17 19)
      )

unit_lists :: IO ()
unit_lists = do
  let e = Vk.BindBufferMemoryDeviceGroupInfo [2, 3, 5]
  Vk.withCStruct e $ \p -> do
    [C.block| void {
      VkBindBufferMemoryDeviceGroupInfo *p = $(VkBindBufferMemoryDeviceGroupInfo *p);
      for(int i = 0; i < p->deviceIndexCount; ++i){
        ((uint32_t*)p->pDeviceIndices)[i] *= 2;
      }
    } |]
    e' <- Vk.peekCStruct p
    show e' @?= show (Vk.BindBufferMemoryDeviceGroupInfo [4, 6, 10])
