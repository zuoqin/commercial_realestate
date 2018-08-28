#!/bin/bash
#make-run.sh
#make sure a process is always running.

export DISPLAY=:0 #needed if you are running a simple gui app.

process="commerce_web"

if ps ax | grep -v grep | grep $process > /dev/null
then
    exit
else
    cd /mnt/admin && java -Djava.awt.headless=true -jar commerce_web.jar 10557
fi

exit
