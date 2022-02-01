#!/bin/sh
set -e

FLAGS="-cpp -i./src -I./src -XNamedFieldPuns -XTypeSynonymInstances -XFlexibleInstances -XLambdaCase -XMultiParamTypeClasses -XDeriveFunctor -prof -fprof-auto -fprof-cafs"
ENTRY="./src/Main.hs"

while [ $# -gt 0 ]; do
    case $1 in
        interpret)
            runghc $FLAGS $ENTRY
            ;;
        build)
            ghc $FLAGS $ENTRY -o ./bin/Oxide
            ;;
        run)
            ./bin/Oxide
            ;;
        *)
            echo "ERROR: Unknown flag $1"
            exit 1
            ;;
    esac
    shift
done
