stack build --flag mseq:usePortMidi --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts -auto-all -caf-all -fforce-recomp" && stack exec -- mseq states +RTS -p -hc
