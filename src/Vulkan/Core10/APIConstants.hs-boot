{-# language CPP #-}
module Vulkan.Core10.APIConstants  ( LUID_SIZE
                                   , QUEUE_FAMILY_EXTERNAL
                                   , MAX_DEVICE_GROUP_SIZE
                                   , MAX_DRIVER_NAME_SIZE
                                   , MAX_DRIVER_INFO_SIZE
                                   , SHADER_UNUSED_KHR
                                   ) where



type LUID_SIZE = 8


type QUEUE_FAMILY_EXTERNAL = 0xfffffffe


type MAX_DEVICE_GROUP_SIZE = 32


type MAX_DRIVER_NAME_SIZE = 256


type MAX_DRIVER_INFO_SIZE = 256


type SHADER_UNUSED_KHR = 0xffffffff

