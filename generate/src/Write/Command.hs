{-# LANGUAGE QuasiQuotes #-}

module Write.Command
  ( writeCommand
  ) where

import           Language.C.Types
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Syntax  as Hs hiding (ModuleName)
import           Spec.Command
import           Spec.Type                     (CType)
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.TypeConverter
import           Write.Utils
import           Write.WriteMonad

writeCommand :: Command -> Bool -> Write Doc
writeCommand c builtin = do
  commandType <- writeCommandType c
  let symbol = cName c
      name = cName c
      typeString = prettyPrint commandType
  if not builtin then do
    (wrappedType, isDeviceFn, hasExtraParam) <- convertFunction commandType
    let wrappedTypeString = prettyPrint wrappedType
        getProcAddr = if isDeviceFn then "vkGetDeviceProcAddr" else "vkGetInstanceProcAddr"
        parentObjParam = if isDeviceFn then "d" else "i"
    tellRequiredName (ExternalName (ModuleName "Foreign.Ptr") "FunPtr")
    tellRequiredName (ExternalName (ModuleName "Foreign.Ptr") "castFunPtr")
    tellRequiredName (ExternalName (ModuleName "Foreign.C.String") "withCString")
    tellRequiredName (ExternalName (ModuleName "System.IO.Unsafe") "unsafePerformIO")
    tellRequiredName (ExternalName (ModuleName "Graphics.Vulkan.DeviceInitialization") getProcAddr)
    -- TODO: look for instance/device/other commands and fetch appropriately
    pure [qc|-- ** {name}
foreign import ccall "dynamic" mk{symbol} :: FunPtr ({typeString}) -> ({typeString})
{name} :: {wrappedTypeString}
{name} {parentObjParam} = (mk{symbol} $ castFunPtr $ procAddr) {if hasExtraParam then mempty else parentObjParam}
  where procAddr = unsafePerformIO $ withCString "{symbol}" $ {getProcAddr} {parentObjParam}
|]
    else pure [qc|-- ** {name}
foreign import ccall "{symbol}" {name} ::
  {typeString}
|]

convertFunction :: Hs.Type -> Write (Hs.Type, Bool, Bool)
convertFunction t@(TyFun (TyCon (UnQual (Ident "VkInstance"))) _) = pure (t, False, False)
convertFunction t@(TyFun (TyCon (UnQual (Ident "VkDevice"))) _) = pure (t, True, False)
convertFunction t@(TyFun (TyCon (UnQual (Ident n))) _)
  | n `elem` ["VkCommandBuffer"] = do
      tellRequiredName (ExternalName (ModuleName "Graphics.Vulkan.Device") "VkDevice")
      pure (TyFun (TyCon (UnQual (Ident "VkDevice"))) t, True, True)
convertFunction t = error $ "Unsure how to get ProcAddr for function of type " ++ prettyPrint t

writeCommandType :: Command -> Write Hs.Type
writeCommandType c = do
  hsReturnType <- (simpleCon "IO" `TyApp`) <$> cTypeToHsType (cReturnType c)
  hsParameterTypes <- traverse (cTypeToHsType . lowerArrayToPointer)
                              (pType <$> cParameters c)
  let hsType = foldr TyFun hsReturnType hsParameterTypes
  pure hsType

lowerArrayToPointer :: CType -> CType
lowerArrayToPointer cType =
  case cType of
    Array _ t -> Ptr [] t
    t -> t
