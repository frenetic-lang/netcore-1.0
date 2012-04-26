#/bin/bash

controller=repeater_controller
top=$(pwd)/$(dirname $0)
sandbox=$top/cabal-dev
frenetic=../../../

STD_ERR=2

# Build the controller
cabal-dev install --sandbox=$sandbox $top $frenetic \
  && cp $sandbox/bin/$controller $top/$controller

if [ $? -ne 0 ]; then
    echo "ERROR: failed to build controller." > $STD_ERR
fi

