#! /bin/bash
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2009 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id$"
#
# who         when      what
# --------    --------  ----------------------------------------------
# rbourtem  2013-04-22  created
#

# getopt example:
# http://www.linuxquestions.org/questions/programming-9/bash-how-to-handle-options-479162/

usage()
{
    echo "Usage: `basename $0` [options]"
    echo
    echo "Options:"
    echo "-h, --help               	Show this help message and exit"
	echo "-d, --dummymsg            Dummy message that is printed by the script while waiting for the end message"
	echo "-e, --endmsg              Message printed at the end after the sleeping time has expired"
	echo "-s, --sleep               Sleep time before to display the endMsg"

}

SCRIPT_NAME=`basename $0`
TEMP=`getopt -o hs:d:e: --long help,sleep:,dummymsg:,endmsg: -n $SCRIPT_NAME -- "$@"`

if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

# Note the quotes around `$TEMP': they are essential!
eval set -- "$TEMP"

SLEEP_TIME=0
DUMMY_MSG=""
END_MSG=""

while true ; do
    case "$1" in
        -h|--help)
            #echo "help option"
            usage
            exit 0
            shift;;
        -s|--sleep)
            SLEEP_TIME=$2
            shift 2;;
		-d|--dummy)
			DUMMY_MSG=$2
			shift 2;;
		-e|--endmsg)
			END_MSG=$2
			shift 2;;
		--)
            shift ; break;;
        *)
            echo "Wrong option specified"
            usage
            exit 1 ;;
    esac
done

while [ "$SLEEP_TIME" -gt "0" ]
do
	sleep 1
	ret=$?
	# Check whether the sleep was interrupted or not
	# We assume that if it was interrupted, it means that someone tried to kill it (acsKillProc?)
	# So we should not display anything in that case (ret != 0)
	if [ "$ret" -eq "0" ]
	then
		echo $DUMMY_MSG
	fi
	let SLEEP_TIME--
	
done

if [ "$ret" -eq "0" ]
then
	echo $END_MSG
fi

exit 0
