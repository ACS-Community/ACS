#!/bin/bash

DOWNLOAD_LIST=extprods.links.txt
sufix='-x86_64'
arch=`uname -m`
for item in `cat $DOWNLOAD_LIST`; do
        if [[ "$item" == *"eclipse"* ]]
        then
                echo $arch
                if [[ "$arch" == *"i"* ]]
                then
                        item=${item/-x86_64/}
                fi
        fi
        p=`echo $item |cut -d':' -f1`
        if [ $p = "http" ]; then
                wget -c -O $PACKAGE_NAME $item
        elif [ $p = "https" ]; then
                wget -c -O $PACKAGE_NAME --no-check-certificate $item
        else
                PACKAGE_NAME=$item
        fi
done

