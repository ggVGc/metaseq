module Config where

import ControllerFunctions
import ApcMini
import Launchpad



singleController = False

syncOutName = "loopMIDI Port 2"

noteInName = "K49"

outPortName = "loopMIDI Port 1"
-- outPortName = "ZynAddSubFX"


sessions :: [(ControllerFunctions, String, Int)]
sessions = [
    ((transposeLaunchpad launchpadFunctions), "Launchpad", 0)
    ,(apcMiniFunctions, "APC", 0)
    ,(apcMiniFunctions, "APC", 1)
  ]
