/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.formatters;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Date;
import java.util.Map;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

import org.omg.CORBA.Any;

import alma.ACSLoggingLog.LogBinaryRecord;
import alma.ACSLoggingLog.LogBinaryRecordHelper;
import alma.ACSLoggingLog.NameValue;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogRecord;
import alma.acs.logging.LogParameterUtil;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.IsoDateFormat;


/**
 * @author cparedes
 *
 * Class that is responsible for formatting the log records/elements of different levels 
 * as well as assigning the right values to their attributes. 
 */
public class AcsBinLogFormatter extends AcsLogFormatter 
{
    /**
     * This is the method used by ACS, for all AcsLogFormatters.
     */
    public Any formatAny( Any anyLogRecord, LogRecord logRecord){
        LogBinaryRecord binLogRecord = formatBinary(logRecord);
        LogBinaryRecordHelper.insert(anyLogRecord,binLogRecord);
        return anyLogRecord;
    }   
    
    LogBinaryRecord formatBinary(LogRecord logRecord){
        LogBinaryRecord rLog = new LogBinaryRecord();
        
		// log level 
		AcsLogLevel acsLevel = AcsLogLevel.getNativeLevel(logRecord.getLevel());
		if (acsLevel == null) {
			return null;
		}		
		final AcsLogLevelDefinition acsCoreLevel = acsLevel.getAcsLevel();

		// get date
		Date date = new Date(logRecord.getMillis());
		String TimeStamp = IsoDateFormat.formatDate(date);
		
		LogParameterUtil logParamUtil = new LogParameterUtil(logRecord);

		rLog.type = (short) acsCoreLevel.value;
        rLog.TimeStamp = TimeStamp;

		String file = logRecord.getSourceClassName();
		if (file == null)
		{
			if (acsCoreLevel == AcsLogLevelDefinition.DEBUG)
				rLog.File = "unknown";
		}
		else
			rLog.File = file;

		long line = logParamUtil.extractLongProperty(LogParameterUtil.PARAM_LINE, -1);
		if (line < 0)
		{
			if (acsCoreLevel == AcsLogLevelDefinition.TRACE || acsCoreLevel == AcsLogLevelDefinition.DEBUG)
		        rLog.Line = 0;		
		}
		else
		    rLog.Line = (int)line;		

		String Routine = logRecord.getSourceMethodName();
		if (Routine == null) {
			if (acsCoreLevel == AcsLogLevelDefinition.TRACE) {
	            rLog.Routine = "unknown";			
			}
		}
		else {
	        rLog.Routine = Routine;			
		}

        // host name: may be different from local host if ErrorTrace gets logged
        String hostName = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_HOSTNAME, null);
        if (hostName == null || hostName.length() == 0) {
            hostName = getLocalHostName();
        }
		rLog.Host = hostName;

//		String process = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_PROCESSNAME, null);
//		if (process != null)
//            rLog.Process = process;
//        else{
            String process = logRecord.getLoggerName();
            if(process != null)  rLog.Process = "LoggerName: " + process;
            else rLog.Process = "";
  //      }
		// source object: the container name or component name
		String sourceObject = logRecord.getLoggerName();
		if (sourceObject != null) {
			rLog.SourceObject = sourceObject;
        }else rLog.SourceObject = "";

		// add thread ID, or name if given		
		String threadName = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_THREAD_NAME, null);
		if (threadName != null && threadName.length() > 0)
			rLog.Thread = threadName;
		else if (logRecord.getThreadID() >= 0)
			rLog.Thread = ""+logRecord.getThreadID();
        else rLog.Thread = "";
		// add context		
		String context = logParamUtil.extractStringProperty("Context", null);
		if (context != null)
			rLog.LogContext = context;
        else rLog.LogContext = "";

		// add stack info
        rLog.StackId="";
        rLog.StackLevel = 0;
		if (acsCoreLevel.compareTo(AcsLogLevelDefinition.WARNING) >= 0) {
			// add stack id
			String stackId = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_STACK_ID, null);
			if (stackId == null)
				rLog.StackId="unknown";
			else
				rLog.StackId=stackId;

			// add stack idlevel
			long stackLevel = logParamUtil.extractLongProperty(LogParameterUtil.PARAM_STACK_LEVEL, -1);
			if (stackLevel > 0)
		        rLog.StackLevel = (int)stackLevel;		
		}

		// add log id		
		long logId = logRecord.getSequenceNumber();
		if (logId >= 0)
            rLog.LogId = "" +logId;
        else rLog.LogId = "";
		// add URI		
		String uri = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_URI, null);
		if (uri != null)
            rLog.Uri = uri;
        else rLog.Uri = "";
		// add priority
		// to be written only different as entry priority		
		long priority = logParamUtil.extractLongProperty(LogParameterUtil.PARAM_PRIORITY, acsCoreLevel.value);
		if (priority != acsCoreLevel.value)
            rLog.Priority = (int)priority;
        else rLog.Priority = -1;

		// the log message becomes the text in our XML record
		if (logRecord.getMessage() != null)
		{
            rLog.MsgData = logRecord.getMessage();
		}else rLog.MsgData = "";

        //add Audience, Array and Antenna, if applicable
        if (logRecord instanceof AcsLogRecord) {
            rLog.Audience = ((AcsLogRecord) logRecord).getAudience();
            rLog.Array = ((AcsLogRecord) logRecord).getArray();
            rLog.Antenna = ((AcsLogRecord) logRecord).getAntenna();
        }
		// <Data> elements: logged exception or error trace, and log parameters
		
        //NameValue a;
		//rLog.log_data = new NameValue[propertiesMap.size() + 1];
		//TODO: see why there are not attributes here
        rLog.attributes = new NameValue[0];
		NameValue [] log_data = new NameValue[257];
		//rLog.log_data = new NameValue[257];
        int i = 0;
        try {
			// logged exception
			Throwable loggedThrowable = logRecord.getThrown();
			if (loggedThrowable != null) {
			    StringWriter exWriter = new StringWriter();
			    loggedThrowable.printStackTrace(new PrintWriter(exWriter));
                log_data[i++] = new NameValue("LoggedException", exWriter.toString());
            }
			// log parameters (except for the special properties which were used already to set specific fields)
			for (Object param : logParamUtil.getNonSpecialPropertiesMapParameters()) {
                if(i>=255) break;
				if (param instanceof Map) {
					// any map that is not the special properties map we interpret as name-value pairs.
					Map propertiesMap = (Map) param;
					for (Object keyName : propertiesMap.keySet()) {
						String value = propertiesMap.get(keyName).toString();
                        log_data[i++] = new NameValue(keyName.toString(), value);
					}					
				}
				else {
					// a single parameter was logged, but we have to fit it into our name-value scheme using a fake name
					String value = param.toString();
                    log_data[i++] = new NameValue("LoggedParameter", value);
				}
			}
		}
		catch (Exception e) {
			// expected not to happen often at all, thus no try blocks inside every loop, so we may lose some <Data>
            String value = e.toString();
            log_data[i++] = new NameValue("DataConstructionError", value);
		}

        rLog.log_data = new NameValue[i];
        for(int j=0; j<i;j++) {
        	rLog.log_data[j] = log_data[j];
        }

		// end tag of BIN record

		return rLog;

    }

	/**
	 * This method should never be called. We only add it to support the inheritance of
	 * {@link AcsLogFormatter} from {@link Formatter} which is useful for text-based subclasses.
	 */
	public String format(LogRecord record) {		
		return "Error: only binary format available!";
	}
	
}
