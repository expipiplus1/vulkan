{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module TestMarshal where

import Graphics.Vulkan.Marshal
import Control.Applicative
import Graphics.Vulkan.Core10
import Data.Function ((&))
import Data.Int(Int64)
import Foreign.Marshal.Utils(maybePeek, maybeWith, with)
import Data.Typeable
import Control.Monad
import Foreign.Ptr

import qualified Data.ByteString
  ( length
  )

import Data.ByteString
  ( ByteString
  , packCString
  , packCStringLen
  , take
  , unpack
  , useAsCString
  )
import Data.ByteString.Unsafe
  ( unsafeUseAsCString
  )

import Data.Word
  ( Word32
  , Word64
  , Word8
  )
import Foreign.Storable
import Control.Exception
import qualified Data.Vector
  ( generateM
  , length
  , take
  )
import qualified Data.Vector.Generic
  ( (++)
  , Vector
  , cons
  , convert
  , empty
  , fromList
  , length
  , replicate
  , take
  )
import qualified Data.Vector.Generic.Sized
  ( Vector
  , convert
  )
import qualified Data.Vector.Generic.Sized.Internal
  ( Vector(Vector)
  )
import qualified Data.Vector.Sized
  ( Vector
  )
import qualified Data.Vector.Storable
  ( unsafeWith
  )
import qualified Data.Vector.Storable.Sized
  ( fromSized
  , unsafeIndex
  )
import Data.Vector
  ( Vector
  , ifoldr
  )

data VulkanException
  = UnknownStructureType
  deriving (Show, Typeable)

instance Exception VulkanException


test =
  let m = Offset3D 1 2 3
  in withCStruct m (print)

test2 =
  let m = SubmitInfo
        (Just (SomeVkStruct' (SubmitInfo Nothing mempty mempty mempty mempty)))
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
        (Just (SomeVkStruct' ai))
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
        (Just (SomeVkStruct' ai))
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
      m' = SubmitInfo (Just (SomeVkStruct' ai)) mempty mempty mempty mempty
      m  = SubmitInfo (Just (SomeVkStruct' m')) mempty mempty mempty mempty
  in  do
        withCStructPtr m (print <=< peekVkStruct . castPtr)
        withCStructPtr m $ \p -> do
          s <- peekVkStruct (castPtr p)
          with () $ \p1 -> with () $ \p2 -> with () $ \p3 -> print (p1, p2, p3)
          with (0 :: Int64) $ \p1 -> with (0 :: Int64) $ \p2 -> with (0 :: Int64) $ \p3 -> print (p1, p2, p3)
          print (fromSomeVkStructChain s :: Maybe ApplicationInfo)

data SomeVkStruct' where
  SomeVkStruct'
    :: (ToCStruct a b, Storable b, Show a, Eq a, Typeable a, HasPNext a) => a -> SomeVkStruct'

instance HasPNext SomeVkStruct' where
  getPNext (SomeVkStruct' a) = getPNext a

fromSomeVkStruct :: Typeable a => SomeVkStruct' -> Maybe a
fromSomeVkStruct (SomeVkStruct' a) = cast a

fromSomeVkStructChain :: Typeable a => SomeVkStruct' -> Maybe a
fromSomeVkStructChain a =
  fromSomeVkStruct a <|> (getPNext a >>= fromSomeVkStructChain)

class HasPNext a where
  getPNext :: a -> Maybe SomeVkStruct'

instance Eq SomeVkStruct' where
  SomeVkStruct' (a :: a) == SomeVkStruct' (b :: b) = case eqT @a @b of
    Nothing   -> False
    Just Refl -> a == b

withSomeVkStruct :: SomeVkStruct' -> (Ptr () -> IO a) -> IO a
withSomeVkStruct (SomeVkStruct' a) f = withCStructPtr a (f . castPtr)

deriving instance Show SomeVkStruct'
-- deriving instance Eq SomeVkStruct'

peekVkStruct :: Ptr () -> IO SomeVkStruct'
peekVkStruct p = do
  peek (castPtr p :: Ptr VkStructureType) >>= \case
    VK_STRUCTURE_TYPE_SUBMIT_INFO ->
      SomeVkStruct' <$> fromCStructPtr (castPtr p :: Ptr VkSubmitInfo)
    VK_STRUCTURE_TYPE_APPLICATION_INFO ->
      SomeVkStruct' <$> fromCStructPtr (castPtr p :: Ptr VkApplicationInfo)
    _ -> throwIO UnknownStructureType



-- No documentation found for TopLevel "SubmitInfo"
data SubmitInfo = SubmitInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "SubmitInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct'
  -- Length valued member elided
  , -- No documentation found for Nested "SubmitInfo" "pWaitSemaphores"
  vkPWaitSemaphores :: Vector VkSemaphore
  , -- No documentation found for Nested "SubmitInfo" "pWaitDstStageMask"
  vkPWaitDstStageMask :: Vector VkPipelineStageFlags
  -- Length valued member elided
  , -- No documentation found for Nested "SubmitInfo" "pCommandBuffers"
  vkPCommandBuffers :: Vector VkCommandBuffer
  -- Length valued member elided
  , -- No documentation found for Nested "SubmitInfo" "pSignalSemaphores"
  vkPSignalSemaphores :: Vector VkSemaphore
  }
  deriving(Show, Eq)

instance ToCStruct SubmitInfo VkSubmitInfo where
  withCStruct :: SubmitInfo -> (VkSubmitInfo -> IO a) -> IO a
  withCStruct from cont = withVec
    (flip ($))
    (vkPSignalSemaphores (from :: SubmitInfo))
    (\pSignalSemaphores -> withVec
      (flip ($))
      (vkPCommandBuffers (from :: SubmitInfo))
      (\pCommandBuffers -> withVec
        (flip ($))
        (vkPWaitDstStageMask (from :: SubmitInfo))
        (\pWaitDstStageMask -> withVec
          (flip ($))
          (vkPWaitSemaphores (from :: SubmitInfo))
          (\pWaitSemaphores -> maybeWith withSomeVkStruct (vkPNext (from :: SubmitInfo))
            (\pPNext -> cont
              (VkSubmitInfo
                VK_STRUCTURE_TYPE_SUBMIT_INFO
                pPNext
                (fromIntegral
                  (minimum
                    ([ Data.Vector.length (vkPWaitSemaphores (from :: SubmitInfo))
                     , Data.Vector.length
                       (vkPWaitDstStageMask (from :: SubmitInfo))
                     ]
                    )
                  )
                )
                pWaitSemaphores
                pWaitDstStageMask
                (fromIntegral
                  (Data.Vector.length (vkPCommandBuffers (from :: SubmitInfo)))
                )
                pCommandBuffers
                (fromIntegral
                  (Data.Vector.length (vkPSignalSemaphores (from :: SubmitInfo)))
                )
                pSignalSemaphores
              )
            )
          )
        )
      )
    )
instance FromCStruct SubmitInfo VkSubmitInfo where
  fromCStruct :: VkSubmitInfo -> IO SubmitInfo
  fromCStruct c =
    SubmitInfo
      <$> -- Univalued Member elided
          maybePeek peekVkStruct (vkPNext (c :: VkSubmitInfo))
                             -- Length valued member elided
      <*> (Data.Vector.generateM
            (fromIntegral (vkWaitSemaphoreCount (c :: VkSubmitInfo)))
            (peekElemOff (vkPWaitSemaphores (c :: VkSubmitInfo)))
          )
      <*> (Data.Vector.generateM
            (fromIntegral (vkWaitSemaphoreCount (c :: VkSubmitInfo)))
            (peekElemOff (vkPWaitDstStageMask (c :: VkSubmitInfo)))
          )
                             -- Length valued member elided
      <*> (Data.Vector.generateM
            (fromIntegral (vkCommandBufferCount (c :: VkSubmitInfo)))
            (peekElemOff (vkPCommandBuffers (c :: VkSubmitInfo)))
          )
                             -- Length valued member elided
      <*> (Data.Vector.generateM
            (fromIntegral (vkSignalSemaphoreCount (c :: VkSubmitInfo)))
            (peekElemOff (vkPSignalSemaphores (c :: VkSubmitInfo)))
          )

instance HasPNext SubmitInfo where
  getPNext a = vkPNext (a :: SubmitInfo)
instance HasPNext ApplicationInfo where
  getPNext a = vkPNext (a :: ApplicationInfo)

-- No documentation found for TopLevel "ApplicationInfo"
data ApplicationInfo = ApplicationInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ApplicationInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct'
  , -- No documentation found for Nested "ApplicationInfo" "pApplicationName"
  vkPApplicationName :: Maybe ByteString
  , -- No documentation found for Nested "ApplicationInfo" "applicationVersion"
  vkApplicationVersion :: Word32
  , -- No documentation found for Nested "ApplicationInfo" "pEngineName"
  vkPEngineName :: Maybe ByteString
  , -- No documentation found for Nested "ApplicationInfo" "engineVersion"
  vkEngineVersion :: Word32
  , -- No documentation found for Nested "ApplicationInfo" "apiVersion"
  vkApiVersion :: Word32
  }
  deriving (Show, Eq)
instance ToCStruct ApplicationInfo VkApplicationInfo where
  withCStruct :: ApplicationInfo -> (VkApplicationInfo -> IO a) -> IO a
  withCStruct from cont = maybeWith
    useAsCString
    (vkPEngineName (from :: ApplicationInfo))
    (\pEngineName -> maybeWith
      useAsCString
      (vkPApplicationName (from :: ApplicationInfo))
      (\pApplicationName ->
          maybeWith withSomeVkStruct (vkPNext (from :: ApplicationInfo))
        (\pPNext -> cont
          (VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO
                             pPNext
                             pApplicationName
                             (vkApplicationVersion (from :: ApplicationInfo))
                             pEngineName
                             (vkEngineVersion (from :: ApplicationInfo))
                             (vkApiVersion (from :: ApplicationInfo))
          )
        )
      )
    )
instance FromCStruct ApplicationInfo VkApplicationInfo where
  fromCStruct :: VkApplicationInfo -> IO ApplicationInfo
  fromCStruct c =
    ApplicationInfo
      <$> -- Univalued Member elided
          maybePeek peekVkStruct (vkPNext (c :: VkApplicationInfo))
      <*> maybePeek packCString (vkPApplicationName (c :: VkApplicationInfo))
      <*> pure (vkApplicationVersion (c :: VkApplicationInfo))
      <*> maybePeek packCString (vkPEngineName (c :: VkApplicationInfo))
      <*> pure (vkEngineVersion (c :: VkApplicationInfo))
      <*> pure (vkApiVersion (c :: VkApplicationInfo))

