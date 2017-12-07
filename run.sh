#!/usr/bin/env bash
thisDir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd -P )
stack exec --stack-yaml "$thisDir/stack.yaml" -- mseq +RTS -p -xc "$1" 
