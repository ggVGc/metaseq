

import Test.Hspec
import Debug.Trace
import Sequencer  
import SequenceRender
import Misc
import qualified Data.IntMap.Strict as M
import Control.Lens
-- import Test.QuickCheck

main :: IO ()
main = hspec spec


test= it



spec :: Spec
spec =
  describe "SequenceRender" $ do
    test "simple track entry" $
      let
        ent = defaultTrackEntry
        defEnt = TriggerEntry defaultTriggerData
        trigs =
          initTriggerEntries
          & trigSeqVariation (0,0) %~ (setTrigSeqEntry 5 ((ent^.teLength)-1) defEnt)

        entries = (M.insert 0 ent M.empty)
        out = renderTrackEntries entries trigs
        correctOut =
          M.empty
          & M.insert ((ent^.teLength)-1) [LaneTriggerBegin (5, defEnt)]
          & M.insert (ent^.teLength) [LaneTriggerEnd (5, defEnt)]
      in
        out `shouldBe` correctOut



    test "track entry transition, different notes" $
      let
        ent1 = defaultTrackEntry
        ent2 = defaultTrackEntry&seqId .~ (1,0)
        defEnt = TriggerEntry defaultTriggerData
        trigs =
          initTriggerEntries
          & trigSeqVariation (0,0) %~ (setTrigSeqEntry 5 ((ent1^.teLength)-1) defEnt)
          & trigSeqVariation (1,0) %~ (setTrigSeqEntry 10 0 defEnt)

        entries =
          (M.insert (ent1^.teLength) ent2 (M.insert 0 ent1 M.empty))

        out = renderTrackEntries entries trigs
        correctOut =
          M.empty
          & M.insert 63 [LaneTriggerBegin (5, defEnt)]
          & M.insert 64 [LaneTriggerBegin (10, defEnt), LaneTriggerEnd (5, defEnt)]
          & M.insert 65 [LaneTriggerEnd (10, defEnt)]
      in
        out `shouldBe` correctOut




