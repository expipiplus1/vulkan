module VMA.Bracket
  ( brackets
  ) where

import           Relude                  hiding ( Handle
                                                , Type
                                                )
import           Data.Vector                    ( Vector )
import qualified Data.Map                      as Map
import qualified Data.Text.Extra               as T

import           Spec.Name
import           Bracket
import           Render.Element
import           Render.Names
import           Render.SpecInfo
import           Error
import           Marshal.Command
import           Render.Utils

brackets
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r, HasRenderedNames r)
  => Vector MarshaledCommand
  -> Sem r (Vector (CName, CName, RenderElement))
brackets marshaledCommands = context "brackets" $ do
  let getMarshaledCommand =
        let mcMap = Map.fromList
              [ (mcName, m)
              | m@MarshaledCommand {..} <- toList marshaledCommands
              ]
        in \c ->  note ("Unable to find marshaled command " <> show c) . (`Map.lookup` mcMap) $ c
      autoBracket' :: BracketType -> CName -> CName -> CName -> Sem r Bracket
      autoBracket' bracketType create destroy with = do
        create'  <- getMarshaledCommand create
        destroy' <- getMarshaledCommand destroy
        autoBracket bracketType create' destroy' with
  bs <- sequenceV
    [ autoBracket' BracketCPS "vmaCreateAllocator" "vmaDestroyAllocator" "vmaWithAllocator"
    , autoBracket' BracketCPS "vmaCreatePool"      "vmaDestroyPool"      "vmaWithPool"
    , autoBracket' BracketCPS "vmaAllocateMemory"  "vmaFreeMemory"       "vmaWithMemory"
    , autoBracket' BracketCPS "vmaAllocateMemoryForBuffer" "vmaFreeMemory" "vmaWithMemoryForBuffer"
    , autoBracket' BracketCPS "vmaAllocateMemoryForImage" "vmaFreeMemory" "vmaWithMemoryForImage"
    , autoBracket' BracketCPS "vmaAllocateMemoryPages" "vmaFreeMemoryPages" "vmaWithMemoryPages"
    , autoBracket' BracketCPS "vmaMapMemory" "vmaUnmapMemory" "vmaWithMappedMemory"
    , autoBracket' BracketCPS "vmaBeginDefragmentation" "vmaEndDefragmentation" "vmaWithDefragmentation"
    , autoBracket' BracketBookend "vmaBeginDefragmentationPass" "vmaEndDefragmentationPass" "vmaUseDefragmentationPass"
    , autoBracket' BracketCPS "vmaCreateBuffer" "vmaDestroyBuffer" "vmaWithBuffer"
    , autoBracket' BracketCPS "vmaCreateImage"  "vmaDestroyImage"  "vmaWithImage"
    , autoBracket' BracketCPS "vmaCreateVirtualBlock"  "vmaDestroyVirtualBlock"  "vmaWithVirtualBlock"
    , autoBracket' BracketCPS "vmaVirtualAllocate"  "vmaVirtualFree"       "vmaWithVirtualAllocation"
    ]
  fromList <$> traverseV (renderBracket paramName) bs

paramName :: Text -> Text
paramName = unReservedWord . T.lowerCaseFirst . dropVma

dropVma :: Text -> Text
dropVma t = if "vma" `T.isPrefixOf` T.toLower t
  then T.dropWhile (== '_') . T.drop 2 $ t
  else t
