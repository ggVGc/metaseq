#!/usr/bin/env bash

ls **/*.hs | entr -r -c bash -c " (notify-send -t 2000 'Building' && stack build --flag mseq:usePortMidi && notify-send -t 2000 'Success' && stack exec mseq states) || notify-send -t 2000 'Errors'"
