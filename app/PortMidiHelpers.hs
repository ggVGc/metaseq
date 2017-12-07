{-# LANGUAGE LambdaCase #-}
module PortMidiHelpers where

import Sound.PortMidi
import Control.Lens
import Data.List
import ZMidi.Core
import Data.Bits (shiftR, (.|.), (.&.), complement)
import Misc

getAllDevices :: IO ([(Int, DeviceInfo)], [(Int, DeviceInfo)])
getAllDevices = do
  n <- countDevices
  deviceInfos <- mapM getDeviceInfo [0..n-1]
  let devs = zip [0..n-1] deviceInfos
  return ([ (d, i) | (d,i) <- devs, input  i],
          [ (d, i) | (d,i) <- devs, output i])

printAllDevices = do
  (inDevs, outDevs) <- getAllDevices
  print "Inputs:"
  inDevs&mapM_ (\(_, d) -> print $ "  "++name d)
  print "Outputs:"
  outDevs&mapM_ (\(_, d) -> print $ "  "++name d)


getInputs :: IO [(Int, DeviceInfo)]
getInputs =
  fmap fst getAllDevices

getOutputs :: IO [(Int, DeviceInfo)]
getOutputs =
  fmap snd getAllDevices

getOutputIds :: [Char] -> IO [Int]
getOutputIds searchName = do
  outs <- getOutputs
  return $ findDevIds outs searchName

getInputIds :: [Char] -> IO [Int]
getInputIds searchName = do
  ins <- getInputs
  return $ findDevIds ins searchName

findDevIds :: [(a, DeviceInfo)] -> [Char] -> [a]
findDevIds devs searchName =
  devs
    &filter(\(_, x) -> isInfixOf searchName (name x))
    &fmap fst


evToPm :: MidiVoiceEvent -> PMMsg
evToPm (NoteOff c p v)         = PMMsg (128 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
evToPm (NoteOn c p v)          = PMMsg (144 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
evToPm (Controller c cn cv) = PMMsg (176 .|. (fromIntegral c .&. 0xF)) (fromIntegral cn) (fromIntegral cv)
evToPm x = PMMsg (128 .|. 0) 0 0 `debug` ("evToPm - Unhandled event: "++show x)
{-
   evToPm (KeyPressure c p pr)    = Just $ PMMsg (160 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral pr)
   evToPm (ProgramChange c pn)    = Just $ PMMsg (192 .|. (fromIntegral c .&. 0xF)) (fromIntegral pn) 0
   evToPm (ChannelPressure c pr)  = Just $ PMMsg (208 .|. (fromIntegral c .&. 0xF)) (fromIntegral pr) 0
   evToPm (PitchWheel c pb)       = Just $ PMMsg (224 .|. (fromIntegral c .&. 0xF)) (fromIntegral lo) (fromIntegral hi)
    where (hi,lo) = (pb `shiftR` 8, pb .&. 0xFF)
   evToPm _ = Nothing 
-}

evToPm' :: MidiEvent -> PMMsg
evToPm' ev = case ev of
  VoiceEvent _ v -> evToPm v
  SysCommonEvent(SongPosPointer high low) ->
    PMMsg 0xF2 (fromIntegral high) (fromIntegral low)
  MidiEventOther (MidiDataOther tag) ->
    PMMsg (fromIntegral tag) 0 0
  SysRealTimeEvent TimingClock ->
    PMMsg 0xF8 0 0
  _ -> error "evToPm': Unhandled midi event"


pmIsNoteOn :: PMMsg -> Bool
pmIsNoteOn (PMMsg stat _ _) =
  ((stat .&. 0xF0) `shiftR` 4) == 0x9

pmSetChannel chan msg =
  (msg .&. (complement 0x0F)) .|. (chan .&. 0x0F)


pmToEv :: PMMsg ->  MidiEvent
pmToEv msg =
  let
    (PMMsg m d1 d2) = msg
    k = (m .&. 0xF0) `shiftR` 4
    c = fromIntegral (m .&. 0x0F)
    unhandled = VoiceEvent RS_OFF $ NoteOff 0 0 0 `debug` ("pmToEv - Unhandled message: "++show msg)
 in
  case m of
    0xF2 -> SysCommonEvent $ SongPosPointer (fromIntegral d1) (fromIntegral d2)
    0xF8 -> SysRealTimeEvent TimingClock
    0xFc -> (SysRealTimeEvent StopSequence)
    0xFB -> (SysRealTimeEvent ContinueSequence)
    -- 0xF1 -> SysCommonEvent QuarterFrame
    0xFA -> (SysRealTimeEvent StartSequence)
    _ ->
      case k of
       0x8 -> VoiceEvent RS_OFF $ NoteOff c (fromIntegral d1) (fromIntegral d2)
       0x9 -> VoiceEvent RS_OFF $ NoteOn  c (fromIntegral d1) (fromIntegral d2)
       0xB -> VoiceEvent RS_OFF $ Controller c (fromIntegral d1) (fromIntegral d2)
       x -> unhandled
   {-
      0xA -> Just $ KeyPressure c (fromIntegral d1) (fromIntegral d2)
      0xC -> Just $ ProgramChange c (fromIntegral d1)
      0xD -> Just $ ChannelPressure c (fromIntegral d1)
      0xE -> Just $ PitchWheel c (fromIntegral (d1 + d2 `shiftL` 8))
      0xF -> Nothing -- SysEx event not handled
      _   -> Nothing
   -}

