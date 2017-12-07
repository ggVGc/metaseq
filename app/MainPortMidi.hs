{-# LANGUAGE LambdaCase #-}
module Main where

import Application
import Player

import Control.Concurrent
import ZMidi.Core (MidiSysRealTimeEvent(..), MidiEvent(..), MidiVoiceEvent(..), MidiRunningStatus(RS_OFF))
import Control.Arrow
import Input
import AppMode
import MainCommon
import Control.Lens
import SequenceRender
import Data.Maybe
import System.Environment
import Sequencer
import Sound.PortMidi
import PortMidiHelpers
import Launchpad
import ApcMini
import Misc
import Control.Monad
import StateFileIO
import System.Clock
import ClockExtra
import Data.List
import ControllerFunctions(transposeLaunchpad)
import Text.Printf
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Transport
import Config(singleController)
import qualified Config as Conf


type ControllerVarIn = MVar [MidiEvent]
type ControllerVarOut = MVar [MidiEvent]



mainPortMidi :: IO ()
mainPortMidi = do
  printAllDevices
  args <- getArgs
  let stateDir = args^?ix 0&fromMaybe (error "Please supply a state directory")
  savedModel <- readStateFile stateDir
  let appModel = defaultAppModel&seqModels.~[savedModel]

  playOutStream <- openOutStream 0 Conf.outPortName 0


  -- syncInStream <- openInStream "loopMIDI Port 2" 0
  syncInStream <- openInStream "DUMMY" 0
  mutex <- newMVar 0

  sessions <- Conf.sessions & mapM (\(conf, portName, portIndex) ->
      makeSession mutex conf portName portIndex
    ) :: IO [(ControllerSessionConfig (),
                           PortMidiSessionIn,
                           PortMidiSessionOut)]
  let
    sessConns = map (view _1) sessions
    inSessions = map (view _2) sessions
    outSessions = map (view _3) sessions


  portMidiLoop mutex 50000
    (if singleController then [head inSessions] else inSessions)
    (if singleController then [head outSessions] else outSessions)

  mbTransportOutStream <- openOutStream 1 Conf.syncOutName 0
  syncFeed <- makeSyncFeeder mbTransportOutStream

  noteInStream <- openInStream Conf.noteInName 0
  print $ "noteInStream: "++(noteInStream&maybe "None" (const "Connected"))
  -- noteInStream2 <- openInStream "loopMIDI Port 4" 0

  let
    syncer = syncInStream&maybe syncFeed (getSyncEv mutex)
    notePlayer = playOutStream&maybe (\_ -> return ()) (playNotes mutex)
    transportRunner =
      mbTransportOutStream
      & maybe (\_ -> return ()) (\stream -> runTransportAction (playMidiEvents stream mutex))

    inNotes = fromMaybe (\_ -> return []) (do
        getNotesStream <- noteInStream
        return $ noteInGetter mutex playOutStream getNotesStream
      )

  genericAppLoop stateDir inNotes syncer notePlayer (\_->return()) appModel transportRunner (
    if singleController then [head sessConns] else sessConns)
  _ <- getLine
  return ()


launchpadInput eventVar state = do
  inEvs <- takeMVar eventVar
  let evs = inEvs & mapMaybe(\case
              VoiceEvent _ ev -> Just ev
              x -> Nothing `debug` ("Dropped launchpad event"++show x)
            )
  if null evs then
    return Nothing
  else
    return $ Just (evs, state)

launchpadRender :: ControllerVarOut-> ControllerRender
launchpadRender eventVar inEvs = do
  _ <- putMVar eventVar inEvs
  return ()


makeLaunchpadSession inEventVar outEventVar controlFuncs =
  ControllerSessionConfig (launchpadInput inEventVar) (launchpadRender outEventVar)  controlFuncs ()


getSyncEv' mutex inStream = do
  _ <- takeMVar mutex
  rawInEvs <- readEvents inStream
  _ <- putMVar mutex 0
  let inEvs = case rawInEvs of
          Left evs ->
            Just (
                evs, evs
                & reverse -- TODO: This is a bit fishy? Is it just reaper sending events in the wrong order?
                & map message
                & map decodeMsg
                & map pmToEv
              )
          Right err ->
            case err of
              NoError -> Nothing
              _ -> Nothing `debug` ("Read error: "++show err)
  return $ inEvs&fromMaybe ([],[])

getSyncEv mutex inStream =
  getSyncEv' mutex inStream
  \> fmap snd

-- playNotes _ [] = return ()
playNotes mutex outStream notes = do
  let evs =
        notes
        & sortOn (\case
            NoteOn _ _ _ -> 1
            _ -> -1
          )
        -- & map (\x -> traceThisM "OUT: " x)
        & map evToPm
        & map(\x -> PMEvent (encodeMsg x) 0)

  unless(null evs)(do
      _ <- takeMVar mutex
      err <- writeEvents outStream evs
      case err of
        NoError -> return ()
        x -> putStrLn ("Some error when writing: " ++ show x)
      _ <- putMVar mutex 0
      return ()
    )


openStream idGetter opener devName devNum = do
  inds <- idGetter devName
  let mbOpenerInd = inds&drop devNum&listToMaybe
  mbOpenerInd&mapM (\ind -> do
      opened <- opener ind
      case opened of
        Left stream ->
          return stream
        Right err ->
          error ("Failed opening stream: "++show (devName, devNum))
    )

playMidiEvents :: PMStream -> MVar Int -> [MidiEvent] -> IO ()
playMidiEvents outStream mutex evs = do
  let
    outEvs =
      evs
      & map evToPm'
      & map(\x -> PMEvent (encodeMsg x) 0)

  _ <- takeMVar mutex
  err <- writeEvents outStream outEvs
  _ <- putMVar mutex 0
  case err of
    NoError -> return ()
    x -> putStrLn ("Some error when writing: " ++ show x)

openOutStream latency =
  openStream getOutputIds (\x -> openOutput x latency)


openInStream =
  openStream getInputIds openInput


makeSession mutex controlFuncs devName devNum = do
  inStream <- openInStream devName devNum
  outStream <- openOutStream 0 devName devNum

  inEventVar <- newMVar []
  outEventVar <- newMVar []
  let
    s = makeLaunchpadSession inEventVar outEventVar controlFuncs
  return $ (s, (inEventVar, fromJust inStream), (outEventVar, fromJust outStream))


-- (InVar, OutVar, InStream, OutStream)
type PortMidiSessionIn = (ControllerVarIn,  PMStream)
type PortMidiSessionOut = (ControllerVarOut, PMStream)


portMidiLoop :: MVar Int -> Int -> [PortMidiSessionIn] -> [PortMidiSessionOut] -> IO ()
portMidiLoop mutex delay inSessions outSessions = do
  unless (null inSessions) (do
      _ <- forkIO $ processInputs inSessions mutex delay
      return ()
    )
  unless (null outSessions) (do
      _ <- forkIO $ processOutputs outSessions mutex
      return ()
    )

processInputs inSessions mutex delay = do
    threadDelay delay
    inSessions&mapM_(\(inVar, inStream) -> do
        _ <- takeMVar mutex
        rawInEvs <- readEvents inStream
        _ <- putMVar mutex 0
        let inEvs = case rawInEvs of
              Left evs ->
                evs
                & reverse -- TODO: This is a bit fishy? Is it just reaper sending events in the wrong order?
                & map message
                & map decodeMsg
                & map pmToEv
                & Just
              Right err ->
                case err of
                  NoError -> Nothing
                  _ -> Nothing `debug` ("Read error: "++show err)
        inEvs&mapM_ (\evs -> putMVar inVar evs)
        return ()
      )
    processInputs inSessions mutex delay


processOutputs outSessions mutex = do
    threadDelay 100
    outSessions&mapM_(\(outVar, outStream) -> do
        rawOutEvs <- takeMVar outVar
        let
          outEvs =
            rawOutEvs
            & map(\x -> case x of
              VoiceEvent _ ev -> ev
              x -> error ("Tried writing invalid event: "++show x)
            )
            & map evToPm
            & map(\x -> PMEvent (encodeMsg x) 0)

        _ <- takeMVar mutex
        err <- writeEvents outStream outEvs
        _ <- putMVar mutex 0
        case err of
          NoError -> return ()
          x -> putStrLn ("Some error when writing: "++(show x))
        return ()
      )
    processOutputs outSessions mutex



syncFeeder :: Maybe PMStream ->  TimeSpec -> MVar TimeSpec -> IO [MidiEvent]
syncFeeder mbTransportOutStream startTime lastTimeVar = do
  lastTime <- readMVar lastTimeVar
  curTime <- getTime Monotonic
  let
    delta = diffTimeSpec curTime lastTime
    startDelta = fromIntegral $ (toNanoSecs (diffTimeSpec curTime startTime)) `div` 1000000
  if toNanoSecs delta > 20000000 then do
    -- print delta
    _ <- swapMVar lastTimeVar curTime
    mbTransportOutStream&mapM_(\out -> do
        err <- writeEvents out [PMEvent ( encodeMsg $ PMMsg 0xF8 0 0) startDelta]
        case err of
          NoError -> return ()
          x -> putStrLn ("noteInGetter: Some error when writing: "++(show x))
      )
    return [SysRealTimeEvent TimingClock]
  else
    return []



noteInGetter :: Num a => MVar a -> Maybe PMStream -> PMStream -> Int -> IO [MidiVoiceEvent]
noteInGetter mutex mbOutStream inStream chan = do
  (rawInEvs,inEvs) <- getSyncEv' mutex inStream
  mbOutStream&mapM_(\outStream ->
    when(rawInEvs /= []) (do
        _ <- takeMVar mutex
        err <- writeEvents outStream (
            rawInEvs
            \> map(\ev -> case ev of
                    PMEvent m t -> PMEvent (pmSetChannel (fromIntegral chan) m) t
                  )
                )
        _ <- putMVar mutex 0
        case err of
          NoError -> return ()
          x -> putStrLn ("noteInGetter: Some error when writing: "++(show x))
      )
    )


  return $ inEvs&mapMaybe(\e -> case e of
          VoiceEvent _ ev -> Just ev
          x -> Nothing `debug` ("Dropped note in event"++show x)
        )



printMessages inStream = do
  threadDelay 10000
  rawInEvs <- readEvents inStream
  case rawInEvs of
    Left evs -> do
        -- print evs
        evs
          & map message
          & map decodeMsg
          & mapM_ (\(PMMsg msg d1 d2)->
                putStrLn $ "0x"++showHex msg ""
                -- print $ "msg: "++(show $  showHex msg)++" d1: "++show d1++" d2: "++show d2
            )
    Right err ->
      case err of
        NoError -> return ()
        _ -> return () `debug` ("Read error: "++show err)
  printMessages inStream


makeSyncFeeder mbTransportOutStream = do
  curTime <- getTime Monotonic
  let timestamp = 0 -- fromIntegral $ (toNanoSecs curTime) `div` 1000000
  mbTransportOutStream&mapM_(\out -> do
      print "Sending start event"
      writeEvents out [ PMEvent (encodeMsg (PMMsg 0xF2 0 0)) timestamp
       ,PMEvent (encodeMsg (PMMsg 0xFC 0 0)) timestamp
       ,PMEvent (encodeMsg (PMMsg 0xF2 0 0)) timestamp
       ,PMEvent (encodeMsg (PMMsg 0xFB 0 0)) timestamp]
    )
  timeVar <- newMVar curTime
  return $ syncFeeder mbTransportOutStream curTime timeVar



-- testGen = do
--   printAllDevices
--   feeder <- makeSyncFeeder "loopMIDI Port 5"
--   let
--     foo = do
--       threadDelay 500
--       _<- feeder
--       foo
--   foo
--

--
-- testListen = do
--   printAllDevices
--   print "Listening for midi events"
--   inStream <- openInStream "loopMIDI Port 5" 0
--   inStream&mapM_ printMessages
--   return ()
--

main = mainPortMidi
-- main = testGen
-- main = testListen
