module XR.Bracket
  ( brackets
  ) where

import           Data.List                      ( (\\) )
import qualified Data.Map                      as Map
import qualified Data.Text.Extra               as T
import           Data.Vector                    ( Vector )
import           Polysemy
import           Relude                  hiding ( Handle
                                                , Type
                                                )

import           Bracket
import           CType
import           Error
import           Marshal.Command
import           Marshal.Scheme
import           Render.Element
import           Render.Names
import           Render.SpecInfo
import           Render.Utils
import           Spec.Parse

brackets
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r, HasRenderedNames r)
  => Vector MarshaledCommand
  -> Vector Handle
  -> Sem r (Vector (CName, CName, RenderElement))
  -- ^ (Creating command, Bracket command, RenderElem)
brackets marshaledCommands handles = context "brackets" $ do
  let getMarshaledCommand =
        let mcMap = Map.fromList
              [ (mcName, m)
              | m@MarshaledCommand {..} <- toList marshaledCommands
              ]
        in  \n ->
              note ("Unable to find marshaled command " <> show n)
                . (`Map.lookup` mcMap)
                $ n
      autoBracket' bracketType create destroy with = do
        create'  <- getMarshaledCommand create
        destroy' <- getMarshaledCommand destroy
        autoBracket bracketType create' destroy' with
      cdBracket h = autoBracket' BracketCPS
                                 (CName ("xrCreate" <> h))
                                 (CName ("xrDestroy" <> h))
                                 (CName ("xrWith" <> h))
      cdBracketWithDestructor h d = autoBracket' BracketCPS
                                                 (CName ("xrCreate" <> h))
                                                 (CName ("xrDestroy" <> d))
                                                 (CName ("xrWith" <> h))
      cdBracketSpace h = cdBracketWithDestructor h "Space"
      beBracket h = autoBracket' BracketBookend
                                 (CName ("xrBegin" <> h))
                                 (CName ("xrEnd" <> h))
                                 (CName ("xrUse" <> h))

  -- TODO: Missing functions here should be warnings, because we might be
  -- generating a different version of the spec.
  bs <- sequenceV
    [ cdBracket "Instance"
    , cdBracket "Session"
    , cdBracketSpace "ReferenceSpace"
    , cdBracketSpace "ActionSpace"
    , cdBracket "Swapchain"
    , cdBracket "ActionSet"
    , cdBracket "Action"
    , cdBracketWithDestructor "SwapchainAndroidSurfaceKHR" "Swapchain"
    , cdBracket "DebugUtilsMessengerEXT"
    , cdBracket "SpatialAnchorMSFT"
    , cdBracketSpace "SpatialAnchorSpaceMSFT"
    , cdBracketSpace "SpatialGraphNodeSpaceMSFT"
    , cdBracket "HandTrackerEXT"
    , cdBracketSpace "HandMeshSpaceMSFT"
    , cdBracketWithDestructor "SpatialAnchorFromPerceptionAnchorMSFT"
                              "SpatialAnchorMSFT"
    -- TODO:
    -- , cdBracket "VulkanInstanceKHR"
    -- , cdBracket "VulkanDeviceKHR"
    , beBracket "Session"
    , beBracket "Frame"
    ]

  --
  -- Check that we can generate all the handles
  --
  let ignoredHandles = []
      handleNames    = hName <$> handles
      -- A crude way of getting all the type names we generate
      createdBracketNames =
        [ n
        | Bracket {..} <- bs
        , TypeName n   <-
          [ t | Normal t <- bInnerTypes ]
          <> [ t | Vector _ (Normal t) <- bInnerTypes ]
        ]
      unhandledHandles =
        toList handleNames \\ (createdBracketNames ++ ignoredHandles)
  unless (null unhandledHandles)
    $ throw ("Unbracketed handles: " <> show unhandledHandles)

  fromList <$> traverseV (renderBracket paramName) bs

dropXr :: Text -> Text
dropXr t = if "xr" `T.isPrefixOf` T.toLower t
  then T.dropWhile (== '_') . T.drop 2 $ t
  else t

paramName :: Text -> Text
paramName = unReservedWord . T.lowerCaseFirst . dropXr
