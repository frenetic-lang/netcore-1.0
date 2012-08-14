#!/bin/bash
P=`dirname $0`
tail -f $P/../demo.log | grep monitor
