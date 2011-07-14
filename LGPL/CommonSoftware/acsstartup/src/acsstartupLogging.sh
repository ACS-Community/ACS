. acsstartupConstants
#------------------------------------------------------------------------------------
#--THIS SCRIPT IS NOT SAFE FOR USE OUTSIDE ACS!
#--It contains functions used to do rudiamentary logging from bash. These functions
#--in turn write the logs to $ACS_TMP or $ACS_TMP/ACS_INSTANCE.$ACS_INSTANCE
#--or $ACSDATA/tmp/ or /tmp. The other thing to take note of is that the set of functions
#--ending with an ACS logging priority (e.g., ACS_LOG_DEBUG) will only send the message
#--to standard out if $ACS_LOG_STDOUT is less than their logging priority.
#------------------------------------------------------------------------------------

function getLogPath
{
local OUTPUT_PATH

#determine where the file whould be stored
if [ "X$ACS_TMP" != "X" ] && [ -w $ACS_TMP ]
then
    OUTPUT_PATH=$ACS_TMP
    if [ ! -e $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE ]
    then
        if ! mkdir $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE 2> /dev/null
        then
            if [ ! -d $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE ]
            then
                echo "Cannot create $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE (getLogPath function)"
                exit $EC_CANNOTCREATE
            else
                echo "Diagnostic Message(getLogPath): $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE exists (OK)" >&2
            fi
        else
            chmod 775 $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE 
        fi
    fi

    if [ -d $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE ] && [ -w $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE ]
    then
        OUTPUT_PATH=$OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE
    fi
elif [ "$ACSDATA/tmp" != "/tmp" ] && [ -w $ACSDATA/tmp ]
then
    #give it an OK default value

    if [ "$OSYSTEM" = "$CYGWIN_VER" ]
    then
        OUTPUT_PATH=$ACSDATA/tmp/`hostname`
    else
        OUTPUT_PATH=$ACSDATA/tmp/`hostname -s`
    fi

    if [ ! -e $OUTPUT_PATH ]
    then
        if ! mkdir $OUTPUT_PATH 2> /dev/null
        then
            if [ ! -d $OUTPUT_PATH ]
            then
                echo "Cannot create $OUTPUT_PATH (getLogPath function)"
                exit $EC_CANNOTCREATE
            else
                echo "Diagnostic Message(getLogPath): $OUTPUT_PATH exists (OK)" >&2
            fi
        else
            chmod 775 $OUTPUT_PATH
        fi
    fi

    if [ ! -e $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE ]
    then
        if ! mkdir $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE 2> /dev/null
        then
            if [ ! -d $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE ]
            then
                echo "Cannot create $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE (getLogPath function)"
                exit $EC_CANNOTCREATE
            else
                echo "Diagnostic Message(getLogPath): $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE exists (OK)" >&2
            fi
        else
            chmod 775 $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE 
        fi
    fi

    if [ -d $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE ] && [ -w $OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE ]
    then
        OUTPUT_PATH=$OUTPUT_PATH/ACS_INSTANCE.$ACS_INSTANCE
    fi

else
    OUTPUT_PATH=/tmp
fi
echo $OUTPUT_PATH
}

export getLogPath

#--Used to access and perhaps create the appropriate log file.
#--Full file name of the log file is printed to stdout.
#--Name of the log file will be .NameOfThisScript.ProcessID.log
function getLogFile
{
local OUTPUT_FILE

#full filename consists of
OUTPUT_FILE=`getLogPath`/.`basename $0`.$$.log

#create the file if it does not already exist.
if [ ! -e $OUTPUT_FILE ]
then
	touch $OUTPUT_FILE
	chmod 774 $OUTPUT_FILE 2> /dev/null

elif [ ! -w $OUTPUT_FILE ]
then
	echo "SEVERE ERROR - cannot log anything with no write permissions on $OUTPUT_FILE!"
	exit $EC_CANNOTUSE
fi

echo $OUTPUT_FILE
}

export getLogFile

#------------------------------------------------------------------------------------
#--Simple function which prints out the current timestamp in ISO-8601 format (e.g. 2011-01-13T11:43:08.164)
#--No parameters.
function getTimeStamp
{
local TS
TS=`date --utc +%Y-%m-%dT%H:%M:%S.%N | cut -c1-23`
echo "$TS"
}

export getTimeStamp

#------------------------------------------------------------------------------------
#--Function designed to log bash messages to an ACS designated area in an ACS designated
#--format.
#--Assumes $ACS_INSTANCES_DIR/ exists and is writeable.
#--Arguements consist of the entire log message.
function ACS_LOG
{
local OUTPUT_FILE
OUTPUT_FILE=`getLogFile`

#write out the message
echo $@ >> "$OUTPUT_FILE"

return $EC_OK
}

export ACS_LOG

#------------------------------------------------------------------------------------
#--Function which logs a message silently. This means the log message is never sent
#--to standard out or standard error and instead goes directly to a file (see
#--getLogFile). This type of behavior is useful when one wants to record certain
#--data such as environment variables which can be used to debug the system later.
#--
#--First argument is the priority of the message (i.e., "DEBUG", "INFO", etc) and
#--this is optional - it's nice if you use it but will not harm anything
#--if you do not.
#--Following arguments are the message itself.

#--Sample Usaged could be:
#--    ACS_SILENT_LOG "ERROR some stuff"
function ACS_SILENT_LOG
{
local TS
TS=`getTimeStamp`

ACS_LOG $TS $@
}

export ACS_SILENT_LOG

#------------------------------------------------------------------------------------
#--Function which does the opposite of ACS_SILENT_LOG - sends all messages to stdout
#--and a file. See ACS_SILENT_LOG for usage.
function ACS_LOG_FORCED
{
local TS
TS=`getTimeStamp`
PRIORITY=$2
PROGRAM_NAME=$1
shift
shift
echo "$TS $PRIORITY [$PROGRAM_NAME] $@"

ACS_LOG "$@"
}

export ACS_LOG_FORCED

#------------------------------------------------------------------------------------
#--Function which logs a message of DEBUG priority. This means the log message is 
#--sent to standard out IFF $ACS_LOG_STDOUT is less than DEBUG priority.
#--In any event, the message will be sent to disk (see getLogFile). 
#--
#--The only arguement to this funciton is the message itself. 
function ACS_LOG_DEBUG
{
local TS
TS=`getTimeStamp`
PROGRAM_NAME=$1
shift

if [ "X$ACS_LOG_STDOUT" != "X" ] && [ $ACS_LOG_STDOUT -lt $ACS_DEBUG_PRIORITY ]
then
	echo "$TS DEBUG [$PROGRAM_NAME] $@"
fi

ACS_LOG $TS DEBUG $@
}

export ACS_LOG_DEBUG

#-----------------------------------------------------------------------------------
#--Function which logs a message of INFO priority. This means the log message is 
#--sent to standard out IFF $ACS_LOG_STDOUT is less than INFO priority.
#--In any event, the message will be sent to disk (see getLogFile). 
#--
#--The only arguement to this funciton is the message itself. 
function ACS_LOG_INFO
{
local TS
TS=`getTimeStamp`
PROGRAM_NAME=$1
shift

if [ "X$ACS_LOG_STDOUT" = "X" ] || [ $ACS_LOG_STDOUT -lt $ACS_INFO_PRIORITY ]
then
	echo "$TS INFO [$PROGRAM_NAME] $@"
fi

ACS_LOG $TS INFO $@
}

export ACS_LOG_INFO

#------------------------------------------------------------------------------------
#--Function which logs a message of ERROR priority. This means the log message is 
#--sent to standard out IFF $ACS_LOG_STDOUT is less than ERROR priority.
#--In any event, the message will be sent to disk (see getLogFile). 
#--
#--The only arguement to this funciton is the message itself. 
function ACS_LOG_ERROR
{
local TS
TS=`getTimeStamp`
PROGRAM_NAME=$1
shift

if [ "X$ACS_LOG_STDOUT" = "X" ] || [ $ACS_LOG_STDOUT -lt $ACS_ERROR_PRIORITY ]
then
	echo "$TS ERROR [$PROGRAM_NAME] $@" >&2
fi

ACS_LOG $TS ERROR $@
}

export ACS_LOG_ERROR

#------------------------------------------------------------------------------------
#--Logs a user command. 
#--No parameters.
#--Requires that $ACS_COMMAND_HISTORY_FILE be writeable if it exists
function ACS_LOG_COMMAND
{
local TS   #timestamp
local CMD  #command which was run
local PID  #process ID
local HOST #hostname
local MSG  #entire message

local LOGDIR
local TMPDIR

TS=`getTimeStamp`
CMD=`basename $0`
PID=$$
HOST=$HOSTNAME

#log the time, user, command, process ID, acs instance, host
MSG="Time:$TS; User=$USER; Host=$HOST; Command:$CMD $@; Process ID:$PID; ACS_INSTANCE=$ACS_INSTANCE; ACS_TMP=$ACS_TMP"

LOGDIR=`getLogPath`
if [ ! -e $LOGDIR/$ACS_COMMAND_HISTORY_FILE ]
then
	if ! touch $LOGDIR/$ACS_COMMAND_HISTORY_FILE 2> /dev/null
	then
		if [ ! -d $LOGDIR ]
		then
			echo "$LOGDIR does not exist (ACS_LOG_COMMAND)"
		else
			echo "Cannot create the file $LOGDIR/$ACS_COMMAND_HISTORY_FILE (ACS_LOG_COMMAND)"
		fi
		exit $EC_CANNOTUSE
	fi
	chmod 666 $LOGDIR/$ACS_COMMAND_HISTORY_FILE
fi
echo $MSG >> "$LOGDIR/$ACS_COMMAND_HISTORY_FILE"
}

export ACS_LOG_COMMAND
