#!/bin/bash
P=`dirname $0`
EXEC=./dist/build/frenetic-example-suite/frenetic-example-suite

cd $P
#rm -f demo.log
#touch demo.log

xterm -e $P/scripts/monitor_console.sh &
$EXEC --verbosity=DEBUG --sink 2>> demo.log &
tail -f demo.log | grep "INFO\\|NOTICE"