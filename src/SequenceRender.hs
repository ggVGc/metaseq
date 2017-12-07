{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module SequenceRender where

import SeqTypes
import Sequencer(curNoteMapping, indexTrigSeqs)
import LaneActions(expandLane)
import Data.Maybe
import Control.Lens
import Data.List
import Data.Foldable(toList)
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as M
import Misc

type NoteOutData = (Int, Int, Int) -- key, vel, delay

data NoteOut =
  NoteBegin NoteOutData
  | NoteEnd NoteOutData
  deriving(Show,Eq)

type TrackNotes = M.IntMap [NoteOut] -- Song position <-> [events]
type SongEvents = [(Int, TrackNotes)] -- (channel, track content)

data LaneTriggerOut =
  LaneTriggerBegin (Int, TriggerEntry)
  | LaneTriggerEnd (Int, TriggerEntry)
  deriving (Show, Eq)

type LaneTriggerOutSequence = M.IntMap [LaneTriggerOut]

makePrisms ''LaneTriggerOut


renderSeqModel :: SharedModel -> SongEvents
renderSeqModel m = fromMaybe mempty $ do
  trackList <-  m^?arrangements.ix 0
  let trackRender = renderTrack  (m&curNoteMapping) (m^.triggerSeqGroups)
  return $
    trackList
    & toList
    & map (\track ->
        (track^.channel, trackRender track)
      )

renderTrack :: [Int] -> S.Seq TriggerSeqVariations -> ArrTrack -> TrackNotes
renderTrack noteMap triggerList track =
  renderTriggerOuts trigOuts
  where
    trigOuts =
      renderTrackEntries (track^.trackEntries) triggerList



renderTrigLane :: RootNoteMap -> Int -> (Int, TrigSeqLane) -> M.IntMap [LaneTriggerOut]
renderTrigLane offsets maxLen (laneInd, lane) =
  [0..(maxLen-1)]
  & foldl' f (0, M.empty)
  & snd
  where
    f (lastOffset, entMap) trigInd =
      let
        lastEnt =
          ents^?ix (trigInd-1)
          & maybe [] (\le ->
              [LaneTriggerEnd (laneInd+lastOffset, le)]
            )
        mbCurEnt = ents^?ix trigInd
      in
        mbCurEnt&maybe
          (if null lastEnt then (lastOffset, entMap) else (lastOffset, entMap&M.insert trigInd lastEnt))
          (\e ->
            let offs = case e of
                  ContinuationEntry -> lastOffset
                  TriggerEntry _ -> getOffset trigInd
            in
              (offs, entMap&M.insert trigInd (
                (case e of
                  ContinuationEntry -> []
                  TriggerEntry x -> lastEnt
                )
                ++[ LaneTriggerBegin (laneInd+offs, e) ]
              )))

    ents :: M.IntMap TriggerEntry
    ents =
      expandLane maxLen lane

    getOffset ind = fromMaybe 0 $ do
      (_,v) <- M.lookupLE ind offsets
      return v


renderTriggerOuts :: LaneTriggerOutSequence -> TrackNotes
renderTriggerOuts trigOutsList =
  trigOutsList
  & M.mapWithKey f
  where
    f ind trigOuts =
      let
        -- rootNote = rootNoteSeq^?ix ind&fromMaybe defaultNoteEntry
        -- lastRootNote = rootNoteSeq^?ix (ind-1)&fromMaybe defaultNoteEntry
      in
        trigOuts&mapMaybe (\case
            LaneTriggerEnd (note, trigEntry) ->
              case trigEntry of
                ContinuationEntry ->
                  Just $ NoteEnd (note, 64, 0)
                TriggerEntry t ->
                  Just $ NoteEnd (note, 64, t^.delay)
            LaneTriggerBegin (note, trigEntry) ->
                case trigEntry of
                  ContinuationEntry -> Nothing
                  TriggerEntry t ->
                    Just $ NoteBegin (note, (t^.vel), t^.delay)
          )


trigStartsToEnds :: [LaneTriggerOut] -> [LaneTriggerOut]
trigStartsToEnds lst =
  lst&mapMaybe(\e -> case e of
    LaneTriggerBegin x -> Just . LaneTriggerEnd $ x
    _ -> Nothing
  )



renderTrackEntries ::
  M.IntMap TrackEntry ->
  S.Seq TriggerSeqVariations ->
  LaneTriggerOutSequence
renderTrackEntries entries triggerList =
  rendEnts
  & flip M.foldlWithKey' M.empty (\outMap teStartPos (teLen, (ents, seqLen)) ->
      let
        allEnts =
          ents
          & replicate 10 -- TODO: Remove this arbitrary number.. Calulate how many maps we need instead
          & sequenceMaps seqLen
          & M.filterWithKey (\k _ ->
            k < teLen
          )

        (_, lastEnts) =
          allEnts
          & M.lookupLE teLen
          & fromMaybe (0,[])
      in
        M.unionWith (++) (allEnts & M.mapKeys (+ teStartPos)) outMap
        & (
            let ends = trigStartsToEnds lastEnts
            in
              if null ends then id
              else
                M.insert (teStartPos+teLen) ends -- TODO: This should be update and not insert
          )

  )
  where
    rendEnts =
      entries&M.map (\te -> (te^.teLength, renderTrackEntry triggerList te))



-- TODO: This is useless
applyNoteOffsets :: M.IntMap Int -> M.IntMap TriggerEntry-> M.IntMap TriggerEntry
applyNoteOffsets offsets noteGroups = noteGroups
  -- noteGroups&M.mapWithKey(\ind trigs ->
  --   trigs&map(\case
  --     ContinuationEntry -> ContinuationEntry
  --     TriggerEntry t -> TriggerEntry $ t
  --   )
  -- )
  -- where
  --   applyOffset ind (n,trig) =
  --     let offs = fromMaybe 0 $ M.lookup ind offsets
  --     in (n+offs,trig)
  --

renderTrackEntry :: S.Seq TriggerSeqVariations -> TrackEntry -> (LaneTriggerOutSequence, Int)
renderTrackEntry triggerList trackEnt = fromMaybe (M.empty, 0) $ do
  (trSeq, trVariations) <- triggerList&indexTrigSeqs (trackEnt^.seqId)
  let
    seqLen = trSeq&getSeqLength

    renderedLanes :: [M.IntMap [LaneTriggerOut]]
    renderedLanes =
      trSeq^.trigEntryLanes
      & M.assocs
      & map (renderTrigLane (trackEnt^.rootNoteSeq) seqLen)

    songMap = [0..seqLen]
      & foldl' (\resMap songPos ->
          let
            trigs =
              renderedLanes
              & mapMaybe(\laneRends -> laneRends^?ix songPos)
              & concat
          in
            if null trigs then
              resMap
            else
              resMap&adjustMapWithDefault [] songPos (const trigs)
        ) M.empty
  return (songMap, seqLen)



