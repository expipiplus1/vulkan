{-# language CPP #-}
-- No documentation found for Chapter "SessionState"
module OpenXR.Core10.Enums.SessionState  (SessionState( SESSION_STATE_UNKNOWN
                                                      , SESSION_STATE_IDLE
                                                      , SESSION_STATE_READY
                                                      , SESSION_STATE_SYNCHRONIZED
                                                      , SESSION_STATE_VISIBLE
                                                      , SESSION_STATE_FOCUSED
                                                      , SESSION_STATE_STOPPING
                                                      , SESSION_STATE_LOSS_PENDING
                                                      , SESSION_STATE_EXITING
                                                      , ..
                                                      )) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Zero (Zero)
-- | XrSessionState - Session lifecycle state
--
-- == Enumerant Descriptions
--
-- The 'SESSION_STATE_UNKNOWN' state /must/ not be returned by the runtime,
-- and is only defined to avoid @0@ being a valid state.
--
-- Receiving the 'SESSION_STATE_IDLE' state indicates that the runtime
-- considers the session is idle. Applications in this state /should/
-- minimize resource consumption but continue to call
-- 'OpenXR.Core10.Instance.pollEvent' at some reasonable cadence.
--
-- Receiving the 'SESSION_STATE_READY' state indicates that the runtime
-- desires the application to prepare rendering resources, begin its
-- session and synchronize its frame loop with the runtime.
-- #sync_frame_loop# The application does this by successfully calling
-- 'OpenXR.Core10.Session.beginSession' and then running its frame loop by
-- calling 'OpenXR.Core10.DisplayTiming.waitFrame',
-- 'OpenXR.Core10.DisplayTiming.beginFrame' and
-- 'OpenXR.Core10.DisplayTiming.endFrame' in a loop. If the runtime wishes
-- to return the session to the 'SESSION_STATE_IDLE' state, it /must/ wait
-- until the application calls 'OpenXR.Core10.Session.beginSession'. After
-- returning from the 'OpenXR.Core10.Session.beginSession' call, the
-- runtime may then immediately transition forward through the
-- 'SESSION_STATE_SYNCHRONIZED' state to the 'SESSION_STATE_STOPPING'
-- state, to request that the application end this session. If the system
-- supports a user engagement sensor and runtime is in 'SESSION_STATE_IDLE'
-- state, the runtime /should/ not transition to the 'SESSION_STATE_READY'
-- state until the user starts engaging with the device.
--
-- Receiving the 'SESSION_STATE_SYNCHRONIZED' state indicates that the
-- application has
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#sync_frame_loop synchronized its frame loop with the runtime>,
-- but its frames are not visible to the user. The application /should/
-- continue running its frame loop by calling
-- 'OpenXR.Core10.DisplayTiming.waitFrame',
-- 'OpenXR.Core10.DisplayTiming.beginFrame' and
-- 'OpenXR.Core10.DisplayTiming.endFrame', although it should avoid heavy
-- GPU work so that other visible applications can take CPU and GPU
-- precedence. The application can save resources here by skipping
-- rendering and not submitting any composition layers until
-- 'OpenXR.Core10.DisplayTiming.waitFrame' returns an
-- 'OpenXR.Core10.DisplayTiming.FrameState' with @shouldRender@ set to
-- true. A runtime /may/ use this frame synchronization to facilitate
-- seamless switching from a previous XR application to this application on
-- a frame boundary.
--
-- Receiving the 'SESSION_STATE_VISIBLE' state indicates that the
-- application has
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#sync_frame_loop synchronized its frame loop with the runtime>,
-- and the session’s frames will be visible to the user, but the session is
-- not eligible to receive XR input. An application may be visible but not
-- have focus, for example when the runtime is composing a modal pop-up on
-- top of the application’s rendered frames. The application /should/
-- continue running its frame loop, rendering and submitting its
-- composition layers, although it may wish to pause its experience, as
-- users cannot interact with the application at this time. It is important
-- for applications to continue rendering when visible, even when they do
-- not have focus, so the user continues to see something reasonable
-- underneath modal pop-ups. Runtimes /should/ make input actions inactive
-- while the application is unfocused, and applications should react to an
-- inactive input action by skipping rendering of that action’s input
-- avatar (depictions of hands or other tracked objects controlled by the
-- user).
--
-- Receiving the 'SESSION_STATE_FOCUSED' state indicates that the
-- application has
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#sync_frame_loop synchronized its frame loop with the runtime>,
-- the session’s frames will be visible to the user, and the session is
-- eligible to receive XR input. The runtime /should/ only give one session
-- XR input focus at any given time. The application /should/ be running
-- its frame loop, rendering and submitting composition layers, including
-- input avatars (depictions of hands or other tracked objects controlled
-- by the user) for any input actions that are active. The runtime /should/
-- avoid rendering its own input avatars when an application is focused,
-- unless input from a given source is being captured by the runtime at the
-- moment.
--
-- Receiving the 'SESSION_STATE_STOPPING' state indicates that the runtime
-- has determined that the application should halt its rendering loop.
-- Applications /should/ exit their rendering loop and call
-- 'OpenXR.Core10.Session.endSession' when in this state. A possible reason
-- for this would be to minimize contention between multiple applications.
-- If the system supports a user engagement sensor and the session is
-- running, the runtime /should/ transition to the 'SESSION_STATE_STOPPING'
-- state when the user stops engaging with the device.
--
-- Receiving the 'SESSION_STATE_EXITING' state indicates the runtime wishes
-- the application to terminate its XR experience, typically due to a user
-- request via a runtime user interface. Applications /should/ gracefully
-- end their process when in this state if they do not have a non-XR user
-- experience.
--
-- Receiving the 'SESSION_STATE_LOSS_PENDING' state indicates the runtime
-- is no longer able to operate with the current session, for example due
-- to the loss of a display hardware connection. An application /should/
-- call 'OpenXR.Core10.Device.destroySession' and /may/ end its process or
-- decide to poll 'OpenXR.Core10.Device.getSystem' at some reasonable
-- cadence to get a new
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- and re-initialize all graphics resources related to the new system, and
-- then create a new session using 'OpenXR.Core10.Device.createSession'.
-- After the event is queued, subsequent calls to functions that accept
-- 'OpenXR.Core10.Handles.Session' parameters /must/ no longer return any
-- success code other than
-- 'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING' for the given
-- 'OpenXR.Core10.Handles.Session' handle. The
-- 'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING' success result is
-- returned for an unspecified grace period of time, and the functions that
-- return it simulate success in their behavior. If the runtime has no
-- reasonable way to successfully complete a given function (e.g.
-- 'OpenXR.Core10.Image.createSwapchain') when a lost session is pending,
-- or if the runtime is not able to provide the application a grace period,
-- the runtime /may/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'. Thereafter, functions
-- which accept 'OpenXR.Core10.Handles.Session' parameters for the lost
-- session /may/ return 'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST' to
-- indicate that the function failed and the given session was lost. The
-- 'OpenXR.Core10.Handles.Session' handle and child handles are henceforth
-- unusable and /should/ be destroyed by the application in order to
-- immediately free up resources associated with those handles.
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged',
-- 'OpenXR.Core10.Instance.pollEvent'
newtype SessionState = SessionState Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SESSION_STATE_UNKNOWN'. An unknown state. The runtime /must/ not return
-- this value in an 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged'
-- event.
pattern SESSION_STATE_UNKNOWN      = SessionState 0
-- | 'SESSION_STATE_IDLE'. The initial state after calling
-- 'OpenXR.Core10.Device.createSession' or returned to after calling
-- 'OpenXR.Core10.Session.endSession'.
pattern SESSION_STATE_IDLE         = SessionState 1
-- | 'SESSION_STATE_READY'. The application is ready to call
-- 'OpenXR.Core10.Session.beginSession' and
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#sync_frame_loop sync its frame loop with the runtime.>
pattern SESSION_STATE_READY        = SessionState 2
-- | 'SESSION_STATE_SYNCHRONIZED'. The application has synced its frame loop
-- with the runtime but is not visible to the user.
pattern SESSION_STATE_SYNCHRONIZED = SessionState 3
-- | 'SESSION_STATE_VISIBLE'. The application has
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#sync_frame_loop synced its frame loop with the runtime>
-- and is visible to the user but cannot receive XR input.
pattern SESSION_STATE_VISIBLE      = SessionState 4
-- | 'SESSION_STATE_FOCUSED'. The application has
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#sync_frame_loop synced its frame loop with the runtime>,
-- is visible to the user and can receive XR input.
pattern SESSION_STATE_FOCUSED      = SessionState 5
-- | 'SESSION_STATE_STOPPING'. The application should exit its frame loop and
-- call 'OpenXR.Core10.Session.endSession'.
pattern SESSION_STATE_STOPPING     = SessionState 6
-- | 'SESSION_STATE_LOSS_PENDING'. The session is in the process of being
-- lost. The application should destroy the current session and can
-- optionally recreate it.
pattern SESSION_STATE_LOSS_PENDING = SessionState 7
-- | 'SESSION_STATE_EXITING'. The application should end its XR experience
-- and not automatically restart it.
pattern SESSION_STATE_EXITING      = SessionState 8
{-# complete SESSION_STATE_UNKNOWN,
             SESSION_STATE_IDLE,
             SESSION_STATE_READY,
             SESSION_STATE_SYNCHRONIZED,
             SESSION_STATE_VISIBLE,
             SESSION_STATE_FOCUSED,
             SESSION_STATE_STOPPING,
             SESSION_STATE_LOSS_PENDING,
             SESSION_STATE_EXITING :: SessionState #-}

conNameSessionState :: String
conNameSessionState = "SessionState"

enumPrefixSessionState :: String
enumPrefixSessionState = "SESSION_STATE_"

showTableSessionState :: [(SessionState, String)]
showTableSessionState =
  [ (SESSION_STATE_UNKNOWN     , "UNKNOWN")
  , (SESSION_STATE_IDLE        , "IDLE")
  , (SESSION_STATE_READY       , "READY")
  , (SESSION_STATE_SYNCHRONIZED, "SYNCHRONIZED")
  , (SESSION_STATE_VISIBLE     , "VISIBLE")
  , (SESSION_STATE_FOCUSED     , "FOCUSED")
  , (SESSION_STATE_STOPPING    , "STOPPING")
  , (SESSION_STATE_LOSS_PENDING, "LOSS_PENDING")
  , (SESSION_STATE_EXITING     , "EXITING")
  ]

instance Show SessionState where
  showsPrec = enumShowsPrec enumPrefixSessionState
                            showTableSessionState
                            conNameSessionState
                            (\(SessionState x) -> x)
                            (showsPrec 11)

instance Read SessionState where
  readPrec = enumReadPrec enumPrefixSessionState showTableSessionState conNameSessionState SessionState

