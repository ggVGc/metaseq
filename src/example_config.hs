module Config where

import ControllerFunctions
import ApcMini
import Launchpad



singleController = True

outPortName = "ZynAddSubFX"
syncOutName = "loopMIDI Port 2"

sessions :: [(ControllerFunctions, String, Int)]
sessions = [
    (apcMiniFunctions, "APC", 0)
    ,(apcMiniFunctions, "APC", 1)
    ,((transposeLaunchpad launchpadFunctions), "Launchpad", 0)
  ]
