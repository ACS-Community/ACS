#! /bin/bash
#
#Auxiliar program that
#finds all files that contains the string from the given directory
#
# Example:
# %>./acsSearchReplaceEncoding.sh UTF-8 $PWD/CDB
 
encodeFrom=$2
directoryFind=$1

grep -R -l $2 $1 | while read file1 ; do
    iconv --from-code=$encodeFrom --to-code=ISO-8859-1 $file1 > $file1.bk
    mv $file1.bk $file1
    acsReplace -nobackup $encodeFrom ISO-8859-1 $file1
done
