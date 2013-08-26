#! /bin/bash

for i in `seq $1 $2`;
do
    acsStartContainer -java javaContainer$i > logs/javaContainer$i.log 2>&1 &
    sleep 3
done    
        
