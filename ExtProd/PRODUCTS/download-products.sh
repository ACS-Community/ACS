#!/bin/bash

DOWNLOAD_LIST=extprods.links.txt

for item in `cat $DOWNLOAD_LIST`; do 
	p=`echo $item |cut -d':' -f1`
  	if [ $p = "http" ]; then 
		wget -c -O $PACKAGE_NAME $item 
	elif [ $p = "https" ]; then
		wget -c -O $PACKAGE_NAME --no-check-certificate $item
	else
		PACKAGE_NAME=$item
	fi 
done
