module Main (main) where

import Data.List (sort)
import Data.Word (Word64)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Fragr qualified as FG
import Vulkan.Utils.FrameGraph.Aliasing (Candidate (..), happensBefore, planAliases, scheduleOf)

main :: IO ()
main = defaultMain (testGroup "vulkan-utils-framegraph" [ordering, aliasing])

-- A pass on a queue, signalling a value, waiting on foreign queues.
pass :: Int -> Int -> Word64 -> [(Int, Word64)] -> FG.PassSync
pass pid queue signal waits =
  FG.PassSync
    { FG.passId = pid
    , FG.name = "p"
    , FG.queue = FG.QueueId queue
    , FG.waits = [FG.Wait{FG.queue = FG.QueueId q, FG.value = v, FG.covers = []} | (q, v) <- waits]
    , FG.signal = signal
    , FG.waitEvents = []
    , FG.signalEvents = []
    , FG.acquires = []
    , FG.releases = []
    }

groupIds :: [[Candidate]] -> [[Int]]
groupIds = sort . map (map (.entryId))

ordering :: TestTree
ordering =
  testGroup
    "happensBefore"
    [ testCase "submission order on one queue" do
        let s = scheduleOf [pass 0 0 1 [], pass 1 0 2 []]
        assertBool "0 before 1" (happensBefore s 0 1)
        assertBool "1 not before 0" (not (happensBefore s 1 0))
    , testCase "a pass is not before itself" do
        let s = scheduleOf [pass 0 0 1 []]
        assertBool "irreflexive" (not (happensBefore s 0 0))
    , -- The case positions alone get wrong: two queues, no wait between them.
      -- Position 0 precedes position 1, but they run concurrently.
      testCase "concurrent queues are unordered despite positions" do
        let s = scheduleOf [pass 0 0 1 [], pass 1 1 1 []]
        assertBool "0 not before 1" (not (happensBefore s 0 1))
        assertBool "1 not before 0" (not (happensBefore s 1 0))
    , testCase "a wait orders across queues" do
        -- q1's pass waits for q0's value 1.
        let s = scheduleOf [pass 0 0 1 [], pass 1 1 1 [(0, 1)]]
        assertBool "0 before 1" (happensBefore s 0 1)
        assertBool "1 not before 0" (not (happensBefore s 1 0))
    , testCase "ordering is transitive through a third queue" do
        -- q0:p0 -> q1:p1 (waits q0) -> q2:p2 (waits q1). p0 must precede p2.
        let s = scheduleOf [pass 0 0 1 [], pass 1 1 1 [(0, 1)], pass 2 2 1 [(1, 1)]]
        assertBool "0 before 2 transitively" (happensBefore s 0 2)
        assertBool "2 not before 0" (not (happensBefore s 2 0))
    , testCase "a wait on a later value still orders the earlier pass" do
        -- Watermarked waits name a value, not a pass: waiting q0's 2 must
        -- also observe q0's pass that signalled 1.
        let s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 1 1 [(0, 2)]]
        assertBool "0 before 2" (happensBefore s 0 2)
        assertBool "1 before 2" (happensBefore s 1 2)
    , testCase "a wait does not order passes after the signal" do
        -- q1 waits q0's value 1; q0's *later* pass (value 2) is not ordered.
        let s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 1 1 [(0, 1)]]
        assertBool "1 not before 2" (not (happensBefore s 1 2))
    ]

aliasing :: TestTree
aliasing =
  testGroup
    "planAliases"
    [ testCase "sequential lifetimes on one queue share a block" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 0 3 []]
          cs = [Candidate 10 (0, 0), Candidate 11 (1, 2)]
        groupIds (planAliases s cs) @?= [[10, 11]]
    , testCase "overlapping lifetimes do not" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 0 2 []]
          cs = [Candidate 10 (0, 1), Candidate 11 (1, 1)]
        groupIds (planAliases s cs) @?= [[10], [11]]
    , -- The silent-corruption case: disjoint positions, concurrent queues.
      testCase "disjoint positions on concurrent queues do NOT share" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 1 1 []]
          cs = [Candidate 10 (0, 0), Candidate 11 (1, 1)]
        groupIds (planAliases s cs) @?= [[10], [11]]
    , testCase "a wait makes the same two safe to share" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 1 1 [(0, 1)]]
          cs = [Candidate 10 (0, 0), Candidate 11 (1, 1)]
        groupIds (planAliases s cs) @?= [[10, 11]]
    , testCase "a group admits a third only if ordered against all of it" do
        -- p0,p1,p2 on q0 (ordered); p3 on q1, unordered with everything.
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 0 3 [], pass 3 1 1 []]
          cs = [Candidate 10 (0, 0), Candidate 11 (1, 1), Candidate 12 (2, 2), Candidate 13 (3, 3)]
        groupIds (planAliases s cs) @?= [[10, 11, 12], [13]]
    , testCase "groups come back in takeover order" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 0 3 []]
          cs = [Candidate 12 (2, 2), Candidate 10 (0, 0), Candidate 11 (1, 1)]
        map (map (.entryId)) (planAliases s cs) @?= [[10, 11, 12]]
    ]
