{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}

module TestMarshal where

import           Control.Applicative
import           Control.Monad
import           Data.Bits                          (zeroBits)
import           Data.Function                      ((&))
import           Data.Int                           (Int64)
import           Data.Typeable
import           Foreign.Marshal.Utils              (maybePeek, maybeWith, with)
import           Foreign.Ptr
import           Graphics.Vulkan.Core10
import           Graphics.Vulkan.Marshal

import qualified Data.ByteString                    (length)

import           Data.ByteString                    (ByteString, packCString,
                                                     packCStringLen, take,
                                                     unpack, useAsCString)
import           Data.ByteString.Unsafe             (unsafeUseAsCString)

import           Control.Exception
import           Data.Vector                        (Vector, ifoldr)
import qualified Data.Vector                        (generateM, length, take)
import qualified Data.Vector.Generic                (Vector, cons, convert,
                                                     empty, fromList, length,
                                                     replicate, take, (++))
import qualified Data.Vector.Generic.Sized          (Vector, convert)
import qualified Data.Vector.Generic.Sized.Internal (Vector (Vector))
import qualified Data.Vector.Sized                  (Vector)
import qualified Data.Vector.Storable               (unsafeWith)
import qualified Data.Vector.Storable.Sized         (fromSized, unsafeIndex)
import           Data.Word                          (Word32, Word64, Word8)
import           Foreign.Storable

data VulkanException
  = UnknownStructureType
  deriving (Show, Typeable)

instance Exception VulkanException


test =
  let m = Offset3D 1 2 3
  in withCStruct m (print)

test2 =
  let m = SubmitInfo
        (Just (SomeVkStruct (SubmitInfo Nothing mempty mempty mempty mempty)))
        mempty
        mempty
        mempty
        mempty
  in do
    withCStructPtr m (print <=< fromCStructPtr @VkSubmitInfo . castPtr)
    withCStructPtr m $ \p -> do
      m' <- fromCStructPtr (castPtr p)
      print (m == m')

test3 =
  let ai = ApplicationInfo
        Nothing
        (Just "appname")
        12
        (Just "enginename")
        14
        11
      m = SubmitInfo
        (Just (SomeVkStruct ai))
        mempty
        mempty
        mempty
        mempty
  in do
    withCStructPtr m (print <=< fromCStructPtr @VkSubmitInfo . castPtr)
    withCStructPtr m $ \p -> do
      m' <- fromCStructPtr (castPtr p)
      print (m == m')

test4 =
  let ai = ApplicationInfo
        Nothing
        (Just "appname")
        12
        (Just "enginename")
        14
        11
      m = SubmitInfo
        (Just (SomeVkStruct ai))
        mempty
        mempty
        mempty
        mempty
  in do
    withCStructPtr m (print <=< peekVkStruct . castPtr)
    withCStructPtr m $ \p -> do
      s <- peekVkStruct (castPtr p)
      print (fromSomeVkStruct s :: Maybe SubmitInfo)

test5 =
  let ai =
        ApplicationInfo Nothing (Just "appname") 12 (Just "enginename") 14 11
      m' = SubmitInfo (Just (SomeVkStruct ai)) mempty mempty mempty mempty
      m  = SubmitInfo (Just (SomeVkStruct m')) mempty mempty mempty mempty
  in  do
        withCStructPtr m (print <=< peekVkStruct . castPtr)
        withCStructPtr m $ \p -> do
          s <- peekVkStruct (castPtr p)
          with () $ \p1 -> with () $ \p2 -> with () $ \p3 -> print (p1, p2, p3)
          with (0 :: Int64) $ \p1 -> with (0 :: Int64) $ \p2 -> with (0 :: Int64) $ \p3 -> print (p1, p2, p3)
          print (fromSomeVkStructChain s :: Maybe ApplicationInfo)

test6 =
  let pdmp = PhysicalDeviceMemoryProperties
        [MemoryType zeroBits 1, MemoryType zeroBits 3]
        [ MemoryHeap 0  zeroBits
        , MemoryHeap 1  zeroBits
        , MemoryHeap 2  zeroBits
        , MemoryHeap 3  zeroBits
        , MemoryHeap 4  zeroBits
        , MemoryHeap 5  zeroBits
        , MemoryHeap 6  zeroBits
        , MemoryHeap 7  zeroBits
        , MemoryHeap 8  zeroBits
        , MemoryHeap 9  zeroBits
        , MemoryHeap 10 zeroBits
        , MemoryHeap 11 zeroBits
        , MemoryHeap 12 zeroBits
        , MemoryHeap 13 zeroBits
        , MemoryHeap 14 zeroBits
        , MemoryHeap 15 zeroBits
        , MemoryHeap 16 zeroBits
        , MemoryHeap 17 zeroBits
        , MemoryHeap 18 zeroBits
        , MemoryHeap 19 zeroBits
        , MemoryHeap 20 zeroBits
        , MemoryHeap 21 zeroBits
        , MemoryHeap 22 zeroBits
        , MemoryHeap 23 zeroBits
        , MemoryHeap 24 zeroBits
        , MemoryHeap 25 zeroBits
        , MemoryHeap 26 zeroBits
        , MemoryHeap 27 zeroBits
        , MemoryHeap 28 zeroBits
        , MemoryHeap 29 zeroBits
        , MemoryHeap 30 zeroBits
        , MemoryHeap 31 zeroBits
        , MemoryHeap 32 zeroBits
        , MemoryHeap 33 zeroBits
        , MemoryHeap 34 zeroBits
        , MemoryHeap 35 zeroBits
        , MemoryHeap 36 zeroBits
        ]
  in  withCStructPtr pdmp (print <=< fromCStructPtr)


test7 =
  let ai = ApplicationInfo
        Nothing
        (Just "appname")
        12
        (Just "enginename")
        14
        11
      pdmp = DeviceQueueCreateInfo
        (Just (SomeVkStruct ai))
        zeroBits
        99
        [1.0, 2.0 .. 100]
  in do
    withCStructPtr pdmp (print <=< fromCStructPtr)
    withCStructPtr pdmp (print <=< peek)

