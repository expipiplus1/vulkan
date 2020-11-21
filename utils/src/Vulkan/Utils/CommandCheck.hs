{-# language TemplateHaskell #-}
{-# language NoMonadComprehensions #-}
{-# language MultiWayIf #-}
{-# language QuasiQuotes #-}

module Vulkan.Utils.CommandCheck
  ( checkCommandsExp
  ) where

import           Control.Applicative            ( (<|>) )
import           Control.Arrow                  ( (&&&) )
import           Data.Char
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( isPrefixOf
                                                , isSuffixOf
                                                , nub
                                                )
import           Data.List.Extra                ( dropEnd )
import           Data.Maybe                     ( catMaybes )
import           Foreign.Ptr
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Vulkan.Core10 (Instance(..), Device(..))
import           Vulkan.Dynamic

-- | Create an expression which checks the function pointers for all the Vulkan
-- commands depended upon by the specified list of function names.
--
-- It returns a list of function names corresponding to those functions with
-- null pointers.
--
-- Your program can use this function to fail early if a command couldn't be
-- loaded for some reason (missing extension or layer for example).
--
-- One can create a function called @checkCommands@ with the following:
-- @
-- [d| checkCommands = $(checkCommandsExp ['withInstance, 'cmdDraw, ...]) |]
-- @
--
-- It has the type @IsString a => Instance -> Device -> [a]@
--
-- It looks basically like
--
-- @
-- \inst dev ->
--   [ name
--   | True <- [ nullFunPtr == pVkCreateDevice inst
--             , nullFunPtr == pVkCreateFence dev
--               ..
--             ]
--   | name <- [ "vkCreateDevice"
--             , "vkCreateFence"
--               ..
--             ]
--   ]
-- @
checkCommandsExp
  :: [Name]
  -- ^ The names of functions from the @vulkan@ package. Unknown commands are
  -- ignored
  -> Q Exp
checkCommandsExp requestedCommands = do
  instAccessors   <- accessorNames ''InstanceCmds
  deviceAccessors <- accessorNames ''DeviceCmds
  let vkCommandNames =
        nub . commandNames instAccessors deviceAccessors =<< requestedCommands
  inst   <- newName "inst"
  device <- newName "device"
  let isNull = \case
        InstanceCmd i -> [|nullFunPtr == $(varE i) $(varE inst)|]
        DeviceCmd   i -> [|nullFunPtr == $(varE i) $(varE device)|]
  [| \(Instance _ $(varP inst)) (Device _ $(varP device)) ->
      [ name
      | (True, name) <- zip
          $(listE (isNull <$> vkCommandNames))
          $(lift (commandString <$> vkCommandNames))
      ]
    |]

-- | Given instance and device accessors and a function, find the function
-- pointer accessor names which it depends on
--
-- >>> commandNames ['pVkCreateDevice, 'pVkDestroyDevice] ['pVkCreateFence] (mkName "withDevice")
-- [InstanceCmd Vulkan.Dynamic.pVkCreateDevice,InstanceCmd Vulkan.Dynamic.pVkDestroyDevice]
commandNames :: [Name] -> [Name] -> Name -> [DeviceOrInstanceCommand]
commandNames instAccessors deviceAccessors =
  let instNames   = (nameBase &&& id) <$> instAccessors
      deviceNames = (nameBase &&& id) <$> deviceAccessors
      findCommand :: String -> Maybe DeviceOrInstanceCommand
      findCommand command =
        (InstanceCmd <$> lookup command instNames)
          <|> (DeviceCmd <$> lookup command deviceNames)
  in  \n ->
        let candidates = commandCandidates (nameBase n)
        in  catMaybes $ findCommand <$> candidates

data DeviceOrInstanceCommand
  = DeviceCmd Name
  | InstanceCmd Name
  deriving (Eq, Show)

-- | Get the C name of a function
--
-- >>> commandString (DeviceCmd (mkName "pVkCreateInstance"))
-- "vkCreateInstance"
commandString :: DeviceOrInstanceCommand -> String
commandString = unPtrName . nameBase . \case
  InstanceCmd n -> n
  DeviceCmd   n -> n

-- | A list of potential sets of vulkan commands this name depends on, not all
-- of them will be valid names.
--
-- >>> commandCandidates "withDevice"
-- ["pVkAllocateDevice","pVkFreeDevice","pVkCreateDevice","pVkDestroyDevice"]
--
-- >>> commandCandidates "waitSemaphoresSafe"
-- ["pVkWaitSemaphores"]
--
-- >>> commandCandidates "useCmdBuffer"
-- ["pVkBeginCmdBuffer","pVkEndCmdBuffer"]
--
-- >>> commandCandidates "withSemaphore"
-- ["pVkAllocateSemaphore","pVkFreeSemaphore","pVkCreateSemaphore","pVkDestroySemaphore"]
commandCandidates :: String -> [String]
commandCandidates n = if
  | "Safe" `isSuffixOf` n
  -> commandCandidates (dropEnd 4 n)
  | Just u <- stripPrefix "with"
  -> (<> u) <$> ["pVkAllocate", "pVkFree", "pVkCreate", "pVkDestroy"]
  | Just u <- stripPrefix "withMapped"
  -> (<> u) <$> ["pVkMap", "pVkUnmap"]
  | Just u <- stripPrefix "use"
  -> (<> u) <$> ["pVkBegin", "pVkEnd"]
  | Just u <- stripPrefix "cmdUse"
  -> (<> u) <$> ["pVkCmdBegin", "pVkCmdEnd"]
  | otherwise
  -> ["pVk" <> upperCaseFirst n]
 where
  stripPrefix p = if p `isPrefixOf` n
    then Just (upperCaseFirst (drop (length p) n))
    else Nothing

-- | Get the record accessors of a type
--
-- >>> $(lift . fmap show =<< accessorNames ''Device)
-- ["Vulkan.Core10.Handles.deviceHandle","Vulkan.Core10.Handles.deviceCmds"]
accessorNames :: Name -> Q [Name]
accessorNames record = reify record <&> \case
  TyConI (DataD _ _ _ _ [con] _)
    | RecC _ vars <- con       -> firstOfThree <$> vars
    | RecGadtC _ vars _ <- con -> firstOfThree <$> vars
  _ -> fail "Name wasn't a TyConI"
  where firstOfThree (a, _, _) = a

unPtrName :: String -> String
unPtrName = \case
  'p' : 'V' : xs -> 'v' : xs
  s              -> s

upperCaseFirst :: String -> String
upperCaseFirst = \case
  x:xs -> toUpper x : xs
  [] -> []
