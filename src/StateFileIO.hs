module StateFileIO where

import System.IO.Error
import SeqTypes
import System.Clock
import Control.Lens ((&))
import Sequencer(defaultSharedModel)
import Data.Binary
import Data.ByteString.Lazy as B (writeFile)

curDate =  getTime Realtime&fmap(\t -> t&sec)


readStateFile :: [Char] -> IO SharedModel
readStateFile stateDirPath = (
      decodeFile $ stateDirPath++"/state_cur"
    ) `catchIOError` (const $ return defaultSharedModel)

writeStateFile :: SharedModel -> String -> IO()
writeStateFile m stateDirPath = do
    d <- curDate
    let bytes = encode m
    B.writeFile (stateDirPath++"/state_cur") bytes
    B.writeFile (stateDirPath++"/"++show d) bytes
    B.writeFile "last_state" bytes

