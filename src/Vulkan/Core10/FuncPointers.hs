{-# language CPP #-}
-- No documentation found for Chapter "FuncPointers"
module Vulkan.Core10.FuncPointers  ( PFN_vkInternalAllocationNotification
                                   , FN_vkInternalAllocationNotification
                                   , PFN_vkInternalFreeNotification
                                   , FN_vkInternalFreeNotification
                                   , PFN_vkReallocationFunction
                                   , FN_vkReallocationFunction
                                   , PFN_vkAllocationFunction
                                   , FN_vkAllocationFunction
                                   , PFN_vkFreeFunction
                                   , FN_vkFreeFunction
                                   , PFN_vkVoidFunction
                                   , FN_vkVoidFunction
                                   ) where

import Foreign.C.Types (CSize)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Enums.InternalAllocationType (InternalAllocationType)
import Vulkan.Core10.Enums.SystemAllocationScope (SystemAllocationScope)
type FN_vkInternalAllocationNotification = ("pUserData" ::: Ptr ()) -> CSize -> InternalAllocationType -> SystemAllocationScope -> IO ()
-- No documentation found for TopLevel "PFN_vkInternalAllocationNotification"
type PFN_vkInternalAllocationNotification = FunPtr FN_vkInternalAllocationNotification


type FN_vkInternalFreeNotification = ("pUserData" ::: Ptr ()) -> CSize -> InternalAllocationType -> SystemAllocationScope -> IO ()
-- No documentation found for TopLevel "PFN_vkInternalFreeNotification"
type PFN_vkInternalFreeNotification = FunPtr FN_vkInternalFreeNotification


type FN_vkReallocationFunction = ("pUserData" ::: Ptr ()) -> ("pOriginal" ::: Ptr ()) -> CSize -> ("alignment" ::: CSize) -> SystemAllocationScope -> IO (Ptr ())
-- No documentation found for TopLevel "PFN_vkReallocationFunction"
type PFN_vkReallocationFunction = FunPtr FN_vkReallocationFunction


type FN_vkAllocationFunction = ("pUserData" ::: Ptr ()) -> CSize -> ("alignment" ::: CSize) -> SystemAllocationScope -> IO (Ptr ())
-- No documentation found for TopLevel "PFN_vkAllocationFunction"
type PFN_vkAllocationFunction = FunPtr FN_vkAllocationFunction


type FN_vkFreeFunction = ("pUserData" ::: Ptr ()) -> ("pMemory" ::: Ptr ()) -> IO ()
-- No documentation found for TopLevel "PFN_vkFreeFunction"
type PFN_vkFreeFunction = FunPtr FN_vkFreeFunction


type FN_vkVoidFunction = () -> IO ()
-- No documentation found for TopLevel "PFN_vkVoidFunction"
type PFN_vkVoidFunction = FunPtr FN_vkVoidFunction

