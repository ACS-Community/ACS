#! /bin/bash
. acsstartupAcsPorts

CL_SLEEP=0
CL_ERROR=
CL_HELP=

LONGOPTS=help
SHORTOPTS=hs:e:

function printUsage {
   echo "Bad"
}

export POSIXLY_CORRECT=1

getopt -n `basename $0` -Q -u -a -l $LONGOPTS $SHORTOPTS "$@" || {
   printUsage
	exit 43;
}

set -- `getopt -u -a -l $LONGOPTS $SHORTOPTS "$@"`

while : 
do
	case "$1" in
	-s)                 CL_SLEEP=$2 ; shift ;;
	-e)                 CL_ERROR=$2 ; shift ;;
	--help)             CL_HELP=true ;; 
	-h)                 CL_HELP=true ;; 
	--) break ;;
	esac
	shift
done
shift

# restore 
export POSIXLY_CORRECT=
unset POSIXLY_CORRECT

if [ "$CL_HELP" ] ; then
   printUsage
   exit 0
fi

sleep $CL_SLEEP

if [ "$CL_ERROR" = "HANG" ]
then
    echo "$@"
    while [ "1" ]
    do
      sleep 1
    done

elif [ "$CL_ERROR" != "" ]
then
    echo "$CL_ERROR"
    exit 1

else
    echo "$@"
fi