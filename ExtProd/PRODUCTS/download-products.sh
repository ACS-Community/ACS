#!/bin/bash

DOWNLOAD_LIST=extprods.links.txt

for item in `cat $DOWNLOAD_LIST`; do 
	p=`echo $item |cut -d':' -f1`
  	if [ $p = "http" ]; then 
		wget $item 
	elif [ $p = "https" ]; then
		wget --no-check-certificate $item
	fi 
done
