#!/bin/bash

if [ $# -ne 2 ]; then
        echo "Usage: $0 <processName> <interval>"
        exit 1
fi
processName=$1
#interval=$2
min=60
let "interval=$2*$min"
n=1
fileName=$1.txt

OutputStr='Monitoring '$processName' mem util every '$2' minutes'
echo $OutputStr >> $fileName

while [ $n -gt 0 ]
do
        memUtil=`phstatus.sh | grep phRuleWorker | awk '{print $5}'`
	curDate=`date`
	value=$memUtil,$curDate
	echo $value >> $fileName
        sleep $interval
done
