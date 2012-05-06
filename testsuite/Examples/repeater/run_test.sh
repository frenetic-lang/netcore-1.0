#/bin/bash


### Set up some environment-related variables.
target=repeater

controller=${target}_controller
topo=${target}_topo

if [ "$(dirname $0)" = "." ]; then
    top=$(pwd)
else
    top=$(pwd)/$(dirname $0)
fi
sandbox=$top/cabal-dev
frenetic=../../../

controller_log=$top/$controller.log

STD_ERR=2


### Build the controller.
echo "Building the controller..."
(cabal-dev install --sandbox=$sandbox $top $frenetic \
  && cp $sandbox/bin/$controller $top/$controller) > /dev/null

if [ $? -ne 0 ]; then
    echo "ERROR: failed to build controller." > $STD_ERR
    exit 1
fi


### Start the controller in the background.
echo "Starting the controller..." | tee $controller_log
echo `date` >> $controller_log
$top/$controller 2>> $controller_log &
controller_job=$!

sleep 2


### Run Mininet with the pingall test.
echo "Starting Mininet and running test: pingall..."
sudo mn --controller=remote --port=6633 --test=pingall --custom $top/../topologies/$topo.py --topo $topo


### Kill the controller.
kill $controller_job || echo "Controller terminated prematurely.  See $controller_log for details."

