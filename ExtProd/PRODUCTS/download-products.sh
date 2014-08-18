#!/bin/bash

DOWNLOAD_LIST=extprods.links.txt

for item in `cat $DOWNLOAD_LIST`; do 
	p=`echo $item |cut -d':' -f1`
  	if [ $p = "http" ]; then 
		wget -c $item 
	elif [ $p = "https" ]; then
		wget -c --no-check-certificate $item
	fi 
done
