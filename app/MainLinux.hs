module Main where

-- -- import Launchpad
-- import System.IO
-- import Launchpad
-- import ApcMini
-- import qualified Data.ByteString.Lazy as BL
-- import ZMidi.Core as Z
-- import Application
-- import Data.Binary.Get
-- import Data.Binary.Put ( runPut )
-- import Control.Concurrent
-- import MainCommon
-- import Control.Lens
-- import System.Environment
-- import Data.Maybe
-- import Sequencer
-- import Data.Word (Word8)
-- import StateFileIO


{-
   getSyncEv :: MVar [t] -> IO [t]
   getSyncEv evVar = swapMVar evVar []
   
   playNotes :: Show t =>  [t] -> IO ()
   -- playNotes notes = print notes
   playNotes _ = return ()
   
   
   mainLinux:: IO ()
   mainLinux = do
     args <- getArgs
     let stateDir = args^?ix 0&fromMaybe (error "Please supply a state directory")
     savedModel <- readStateFile stateDir
     evVar <- newMVar []
     let appModel = defaultAppModel&seqModels.~[savedModel]
     let loopFun = genericAppLoop stateDir (getSyncEv evVar) playNotes (\_ -> return ()) (\_ -> return []) appModel
   
     _ <- forkIO $ player evVar
     -- loopFun []
     apcMain loopFun
   
   
   apcMain :: ([ControllerSession' (Word8, BL.ByteString)] -> IO a) -> IO ()
   apcMain loopFun =
     let
       -- apcName1 = "/dev/midi3"
       -- apcName2 = "/dev/midi4"
       launchpadName = "/dev/midi2"
     in do
       s3 <- makeSession launchpadName apcMiniFunctions
       _ <- loopFun [s3]
       _ <- getLine
       return ()
   
   
   makeSession fileName funcs = do
     h <- openBinaryFile fileName ReadMode
     bytes <- BL.hGetContents h
     return (getLaunchpadInput, (renderLaunchpadEvents fileName), defaultControllerState,  funcs, (0, bytes))
   
   
   parseLaunchpadMessage runStatus = do
     noteOrState <- getWord8
     if noteOrState >= 0x80 then do
       note <- getWord8
       velocity <- getWord8
       return ((note,velocity, runStatus), noteOrState)
     else do
       velocity <- getWord8
       return ((noteOrState, velocity, runStatus), runStatus)
   
   getLaunchpadInput :: (Word8, BL.ByteString) -> IO (Maybe (Either String [MidiVoiceEvent], (Word8, BL.ByteString)))
   getLaunchpadInput (oldState, bytes) =
     return $ Just (res, (newState, remainingBytes))
     where
       (res, newState, remainingBytes) =
         case runGetOrFail (parseLaunchpadMessage oldState) bytes of
           Left (remBytes, _, msg) -> (Left msg, 0, remBytes)
           Right (remBytes, _, ((k,v,_), newState')) ->
             (Right e, newState', remBytes)
             where
               e = if newState' == 144 then
                     [Z.NoteOn 0 k v]
                    else
                     [Z.Controller 0 k v]
   
   renderLaunchpadEvents :: FilePath -> [MidiEvent] -> IO ()
   renderLaunchpadEvents outHandleName events =
     withBinaryFile outHandleName WriteMode $ \outHandle -> -- do
       sequence_ $ sendToDevice outHandle events
   
   
   sendToDevice :: Handle -> [Z.MidiEvent] -> [IO ()]
   sendToDevice handle events =
     case splitAt 1 events of
       ([],[]) -> []
       (evs,rest) ->
         BL.hPut handle (runPut $ mapM_  Z.putEvent evs)
         : hFlush handle
         : threadDelay 5000
         : sendToDevice handle rest
   
   
   
   writeToFile :: [Z.MidiMessage] -> IO ()
   writeToFile trackContent =
     writeMidi outMidiPath  mfile
     where
   
   
       mfile = MidiFile { mf_header = MidiHeader { hdr_format = MF1
                                , num_tracks    = 2
                                , time_division = TPB 480
                                }
       , mf_tracks = [ meta_track, sound_track ]
       }
       meta_track  = MidiTrack [ (0, MetaEvent $ TextEvent SEQUENCE_NAME "Track 0")
                               , (0, MetaEvent EndOfTrack)
                               ]
       sound_track = MidiTrack $ concat [trackHead, trackContent, [(0, MetaEvent EndOfTrack) ]]
       trackHead = [ (0, MetaEvent $ TextEvent SEQUENCE_NAME "Track 1") , (0, MetaEvent $ SetTempo 500000)]
   
   
   player evVar = do
     threadDelay 10000
     _ <- swapMVar evVar [SysRealTimeEvent TimingClock]
     player evVar
-}


main :: IO ()
main = return ()
