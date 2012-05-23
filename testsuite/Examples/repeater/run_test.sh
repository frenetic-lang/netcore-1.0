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
frenetic=$top/../../../
topologies=$top/../topologies
policies=$top/../policies

controller_log=$top/$controller.log

# PROFILING_OPTIONS="--enable-library-profiling --enable-executable-profiling --ghc-options=-auto-all"
PROFILING_OPTIONS=

STD_ERR=2


### Build the controller.
function build_controller {
echo "Building the controller..."
(cabal-dev install $PROFILING_OPTIONS --sandbox=$sandbox $top $frenetic $topologies $policies \
  && cp $sandbox/bin/$controller $top/$controller) > /dev/null

if [ $? -ne 0 ]; then
    echo "ERROR: failed to build controller." > $STD_ERR
    exit 1
fi
}

### Start the controller in the background.
function start_controller {
echo "Starting the controller..." | tee $controller_log
echo `date` >> $controller_log
$top/$controller +RHS -tc -RHS 2>> $controller_log &
controller_job=$!

sleep 2
}


### Run Mininet with the pingall test.
function start_mininet {
echo "Starting Mininet and running test: pingall..."
TEMP=`getopt m $@`
tst="--test=pingall"
set -- $TEMP
for i; do
    case "$i" in
        -m) tst= ;;
    esac
done
sudo mn --controller=remote --port=6633 $tst --custom $top/../topologies/$topo.py --topo $topo
}


### Kill the controller.
function kill_controller {
kill $controller_job || echo "Controller terminated prematurely.  See $controller_log for details."
}

TEMP=`getopt bm $@`
set -- $TEMP
justBuild=0
for i; do
    case "$i" in
        -m) tst=-m;;
        -b) justBuild=1;;
    esac
done

build_controller
if [ $justBuild -eq 1 ]; then 
    exit; 
fi

start_controller
start_mininet $tst
kill_controller

