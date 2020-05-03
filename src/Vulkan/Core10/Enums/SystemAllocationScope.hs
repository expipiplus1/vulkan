{-# language CPP #-}
module Vulkan.Core10.Enums.SystemAllocationScope  (SystemAllocationScope( SYSTEM_ALLOCATION_SCOPE_COMMAND
                                                                        , SYSTEM_ALLOCATION_SCOPE_OBJECT
                                                                        , SYSTEM_ALLOCATION_SCOPE_CACHE
                                                                        , SYSTEM_ALLOCATION_SCOPE_DEVICE
                                                                        , SYSTEM_ALLOCATION_SCOPE_INSTANCE
                                                                        , ..
                                                                        )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkSystemAllocationScope - Allocation scope
--
-- = Description
--
-- -   'SYSTEM_ALLOCATION_SCOPE_COMMAND' specifies that the allocation is
--     scoped to the duration of the Vulkan command.
--
-- -   'SYSTEM_ALLOCATION_SCOPE_OBJECT' specifies that the allocation is
--     scoped to the lifetime of the Vulkan object that is being created or
--     used.
--
-- -   'SYSTEM_ALLOCATION_SCOPE_CACHE' specifies that the allocation is
--     scoped to the lifetime of a 'Vulkan.Core10.Handles.PipelineCache' or
--     'Vulkan.Extensions.Handles.ValidationCacheEXT' object.
--
-- -   'SYSTEM_ALLOCATION_SCOPE_DEVICE' specifies that the allocation is
--     scoped to the lifetime of the Vulkan device.
--
-- -   'SYSTEM_ALLOCATION_SCOPE_INSTANCE' specifies that the allocation is
--     scoped to the lifetime of the Vulkan instance.
--
-- Most Vulkan commands operate on a single object, or there is a sole
-- object that is being created or manipulated. When an allocation uses an
-- allocation scope of 'SYSTEM_ALLOCATION_SCOPE_OBJECT' or
-- 'SYSTEM_ALLOCATION_SCOPE_CACHE', the allocation is scoped to the object
-- being created or manipulated.
--
-- When an implementation requires host memory, it will make callbacks to
-- the application using the most specific allocator and allocation scope
-- available:
--
-- -   If an allocation is scoped to the duration of a command, the
--     allocator will use the 'SYSTEM_ALLOCATION_SCOPE_COMMAND' allocation
--     scope. The most specific allocator available is used: if the object
--     being created or manipulated has an allocator, that object’s
--     allocator will be used, else if the parent
--     'Vulkan.Core10.Handles.Device' has an allocator it will be used,
--     else if the parent 'Vulkan.Core10.Handles.Instance' has an allocator
--     it will be used. Else,
--
-- -   If an allocation is associated with a
--     'Vulkan.Extensions.Handles.ValidationCacheEXT' or
--     'Vulkan.Core10.Handles.PipelineCache' object, the allocator will use
--     the 'SYSTEM_ALLOCATION_SCOPE_CACHE' allocation scope. The most
--     specific allocator available is used (cache, else device, else
--     instance). Else,
--
-- -   If an allocation is scoped to the lifetime of an object, that object
--     is being created or manipulated by the command, and that object’s
--     type is not 'Vulkan.Core10.Handles.Device' or
--     'Vulkan.Core10.Handles.Instance', the allocator will use an
--     allocation scope of 'SYSTEM_ALLOCATION_SCOPE_OBJECT'. The most
--     specific allocator available is used (object, else device, else
--     instance). Else,
--
-- -   If an allocation is scoped to the lifetime of a device, the
--     allocator will use an allocation scope of
--     'SYSTEM_ALLOCATION_SCOPE_DEVICE'. The most specific allocator
--     available is used (device, else instance). Else,
--
-- -   If the allocation is scoped to the lifetime of an instance and the
--     instance has an allocator, its allocator will be used with an
--     allocation scope of 'SYSTEM_ALLOCATION_SCOPE_INSTANCE'.
--
-- -   Otherwise an implementation will allocate memory through an
--     alternative mechanism that is unspecified.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
newtype SystemAllocationScope = SystemAllocationScope Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
pattern SYSTEM_ALLOCATION_SCOPE_COMMAND = SystemAllocationScope 0
-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
pattern SYSTEM_ALLOCATION_SCOPE_OBJECT = SystemAllocationScope 1
-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
pattern SYSTEM_ALLOCATION_SCOPE_CACHE = SystemAllocationScope 2
-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
pattern SYSTEM_ALLOCATION_SCOPE_DEVICE = SystemAllocationScope 3
-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
pattern SYSTEM_ALLOCATION_SCOPE_INSTANCE = SystemAllocationScope 4
{-# complete SYSTEM_ALLOCATION_SCOPE_COMMAND,
             SYSTEM_ALLOCATION_SCOPE_OBJECT,
             SYSTEM_ALLOCATION_SCOPE_CACHE,
             SYSTEM_ALLOCATION_SCOPE_DEVICE,
             SYSTEM_ALLOCATION_SCOPE_INSTANCE :: SystemAllocationScope #-}

instance Show SystemAllocationScope where
  showsPrec p = \case
    SYSTEM_ALLOCATION_SCOPE_COMMAND -> showString "SYSTEM_ALLOCATION_SCOPE_COMMAND"
    SYSTEM_ALLOCATION_SCOPE_OBJECT -> showString "SYSTEM_ALLOCATION_SCOPE_OBJECT"
    SYSTEM_ALLOCATION_SCOPE_CACHE -> showString "SYSTEM_ALLOCATION_SCOPE_CACHE"
    SYSTEM_ALLOCATION_SCOPE_DEVICE -> showString "SYSTEM_ALLOCATION_SCOPE_DEVICE"
    SYSTEM_ALLOCATION_SCOPE_INSTANCE -> showString "SYSTEM_ALLOCATION_SCOPE_INSTANCE"
    SystemAllocationScope x -> showParen (p >= 11) (showString "SystemAllocationScope " . showsPrec 11 x)

instance Read SystemAllocationScope where
  readPrec = parens (choose [("SYSTEM_ALLOCATION_SCOPE_COMMAND", pure SYSTEM_ALLOCATION_SCOPE_COMMAND)
                            , ("SYSTEM_ALLOCATION_SCOPE_OBJECT", pure SYSTEM_ALLOCATION_SCOPE_OBJECT)
                            , ("SYSTEM_ALLOCATION_SCOPE_CACHE", pure SYSTEM_ALLOCATION_SCOPE_CACHE)
                            , ("SYSTEM_ALLOCATION_SCOPE_DEVICE", pure SYSTEM_ALLOCATION_SCOPE_DEVICE)
                            , ("SYSTEM_ALLOCATION_SCOPE_INSTANCE", pure SYSTEM_ALLOCATION_SCOPE_INSTANCE)]
                     +++
                     prec 10 (do
                       expectP (Ident "SystemAllocationScope")
                       v <- step readPrec
                       pure (SystemAllocationScope v)))

