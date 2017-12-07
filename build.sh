#!/usr/bin/env bash
stack build --executable-profiling --library-profiling --flag mseq:usePortMidi --ghc-options="-fprof-auto -fprof-cafs"
