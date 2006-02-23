. acsstartupConstants
#------------------------------------------------------------------------------------
#--THIS SCRIPT IS NOT SAFE FOR USE OUTSIDE ACS!
#--It contains functions used to do rudiamentary logging from bash. These functions
#--in turn write the logs to $ACS_TMP or $ACSDATA/tmp/ACS_INSTANCE.$ACS_INSTANCE
#--or $ACSDATA/tmp/ or /tmp. The other thing to take note of is that the set of functions
#--ending with an ACS logging priority (e.g., ACS_LOG_DEBUG) will only send the message
#--to standard out if $ACS_LOG_STDOUT is less than their logging priority.
#------------------------------------------------------------------------------------
#--Used to access and perhaps create the appropriate log file.
#--Full file name of the log file is printed to stdout.
#--Name of the log file will be .NameOfThisScript.ProcessID.log
function getLogFile
{
local OUTPUT_FILE

#determine where the file whould be stored
if [ "X$ACS_TMP" != "X" ] && [ -w $ACS_TMP ]
then
	OUTPUT_FILE=$ACS_TMP

elif [ "$ACSDATA/tmp" != "/tmp" ] && [ -w $ACSDATA/tmp ]
then
    #give it an OK default value
    OUTPUT_FILE=$ACS_INSTANCES_DIR

    if [ -d $OUTPUT_FILE/ACS_INSTANCE.$ACS_INSTANCE ] && [ -w $OUTPUT_FILE/ACS_INSTANCE.$ACS_INSTANCE ]
    then
	OUTPUT_FILE=$ACS_INSTANCES_DIR/ACS_INSTANCE.$ACS_INSTANCE
    fi

else
	OUTPUT_FILE=/tmp
fi
#full filename consists of
OUTPUT_FILE=$OUTPUT_FILE/.`basename $0`.$$.log

#create the file if it does not already exist.
if [ ! -e $OUTPUT_FILE ]
then
	touch $OUTPUT_FILE
	chmod 774 $OUTPUT_FILE

elif [ ! -w $OUTPUT_FILE ]
then
	echo "SEVERE ERROR - cannot log anything with no write permissions on $OUTPUT_FILE!"
	exit $EC_CANNOTUSE
fi

echo $OUTPUT_FILE
}

export getLogFile

#------------------------------------------------------------------------------------
#--Simple function which prints out the current timestamp in ISO-8601 format
#--No parameters.
function getTimeStamp
{
local TS
TS=`date --iso-8601=seconds`

echo $TS
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
echo $@ >> $OUTPUT_FILE

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

echo "$@"

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


if [ "X$ACS_LOG_STDOUT" != "X" ] && [ $ACS_LOG_STDOUT -lt $ACS_DEBUG_PRIORITY ]
then
	echo "DEBUG - $@"
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

if [ "X$ACS_LOG_STDOUT" = "X" ] || [ $ACS_LOG_STDOUT -lt $ACS_INFO_PRIORITY ]
then
	echo "INFO - $@"
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

if [ "X$ACS_LOG_STDOUT" = "X" ] || [ $ACS_LOG_STDOUT -lt $ACS_ERROR_PRIORITY ]
then
	echo "ERROR - $@" >&2
fi

ACS_LOG $TS ERROR $@
}

export ACS_LOG_ERROR

#------------------------------------------------------------------------------------
