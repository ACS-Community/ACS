#!/bin/sh

LIST=$INTROOT:$INTLIST:$ACSROOT

echo "Checking all xml and xsd files with encode different from ISO-8859-1"

echo $LIST |
 {
    read line
    IFS=:
    array=( $line )

    for path in ${array[@]}
    do
      if [ X"$path" != X ]; then
        if [ -d $path/idl ]; then
            grep -i encoding $path/idl/*.xsd | grep -v "ISO-8859-1"
            grep -i encoding $path/idl/*.xml | grep -v "ISO-8859-1"
        fi
      fi
      done
} 

echo "---------o0o-----------"
