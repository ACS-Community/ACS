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
import java.util.logging.LogRecord;

import org.omg.CORBA.Any;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogRecord;
import alma.acs.logging.LogParameterUtil;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.XmlNormalizer;

/**
 * @author rgeorgie
 *
 * Class that is responsible for formatting the log records/elements of different levels 
 * as well as assigning the right values to their attributes. 
 */
public class AcsXMLLogFormatter extends AcsLogFormatter 
{
    public Any formatAny( Any anyLogRecord, LogRecord logRecord){
        String xmlLogRecord = format(logRecord);
        anyLogRecord.insert_string(xmlLogRecord);
        return anyLogRecord;
    }   
 
	/**
	 * Constructs the XML log message that can be sent to the ACS logging service.
	 * @see java.util.logging.Formatter#format(java.util.logging.LogRecord)
	 */
	public String format(LogRecord logRecord) {
		// log level 
		AcsLogLevel acsLevel = AcsLogLevel.getNativeLevel(logRecord.getLevel());
		if (acsLevel == null) {
			return "";
		}
		final AcsLogLevelDefinition acsCoreLevel = acsLevel.getAcsLevel();
		final String levelName = acsLevel.getEntryName();

		// get date
		String TimeStamp = IsoDateFormat.formatDate(new Date(logRecord.getMillis()));

		LogParameterUtil logParamUtil = new LogParameterUtil(logRecord);

		StringBuffer sb = new StringBuffer("");

		sb.append("<");
		sb.append(levelName);
		sb.append(" ");

		sb.append("TimeStamp=\"" + TimeStamp + "\" ");

		String file = logRecord.getSourceClassName();
		if (file == null) {
			if (acsCoreLevel == AcsLogLevelDefinition.DEBUG)
				sb.append("File=\"unknown\" ");
		}
		else {
			sb.append("File=\"" + file + "\"  ");
		}

		long line = logParamUtil.extractLongProperty(LogParameterUtil.PARAM_LINE, -1);
		if (line < 0) {
			if (acsCoreLevel == AcsLogLevelDefinition.TRACE || acsCoreLevel == AcsLogLevelDefinition.DEBUG)
				sb.append("Line=\"0\" ");
		}
		else {
			sb.append("Line=\"" + line + "\" ");
		}

		String Routine = logRecord.getSourceMethodName();
		if (Routine == null) {
			if (acsCoreLevel == AcsLogLevelDefinition.TRACE)
				sb.append("Routine=\"unknown\" ");
		}
		else {
			sb.append("Routine=\"" + maskAttribute(Routine) + "\" ");
		}

		// host name: may be different from local host if ErrorTrace gets logged
		String hostName = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_HOSTNAME, null);
		if (hostName == null || hostName.length() == 0) {
			hostName = this.getLocalHostName();
		}
            //hostName = getLocalHostName();
		sb.append("Host=\"" + hostName + "\" ");

		String process = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_PROCESSNAME, null);
		if (process != null) {
			sb.append("Process=\"" + process + "\" ");
		}
		else {
			process = logRecord.getLoggerName();
			if (process != null) {
				sb.append("Process=\"" + process + "\" ");
			}
		}
		String sourceObject = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_SOURCEOBJECT, null);
		if (sourceObject != null) {
			sb.append("SourceObject=\"" + sourceObject + "\" ");
		}
		else {
			sourceObject = logRecord.getLoggerName();
			if (sourceObject != null) {
				sb.append("SourceObject=\"" + sourceObject + "\" ");
			}
		}

		// add thread ID, or name if given		
		String threadName = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_THREAD_NAME, null);
		if (threadName != null && threadName.length() > 0) {
			sb.append("Thread=\"" + threadName + "\" ");
		}
		else if (logRecord.getThreadID() >= 0) {
			sb.append("Thread=\"" + logRecord.getThreadID() + "\" ");
		}

		// add context		
		String context = logParamUtil.extractStringProperty("Context", null);
		if (context != null) {
			sb.append("Context=\"" + context + "\" ");
		}
		
		// add stack info
		if (acsCoreLevel.compareTo(AcsLogLevelDefinition.WARNING) >= 0) {
			// add stack id
			String stackId = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_STACK_ID, null);
			if (stackId == null)
				sb.append("StackId=\"unknown\" ");
			else
				sb.append("StackId=\"" + stackId + "\" ");

			// add stack idlevel
			long stackLevel = logParamUtil.extractLongProperty(LogParameterUtil.PARAM_STACK_LEVEL, -1);
			if (stackLevel < 0)
				sb.append("StackLevel=\"0\" ");
			else
				sb.append("StackLevel=\"" + stackLevel + "\" ");
		}

		// add log id		
		long logId = logRecord.getSequenceNumber();
		if (logId >= 0) {
			sb.append("LogId=\"" + logId + "\" ");
		}

		// add URI		
		String uri = logParamUtil.extractStringProperty(LogParameterUtil.PARAM_URI, null);
		if (uri != null) {
			sb.append("Uri=\"" + uri + "\" ");
		}

		// add priority
		// to be written only different as entry priority		
		long priority = logParamUtil.extractLongProperty(LogParameterUtil.PARAM_PRIORITY, acsCoreLevel.value);
		if (priority != acsCoreLevel.value) {
			sb.append("Priority=\"" + priority + "\" ");
		}

                //add Audience, if applicable(for typeSafeLogs/Operator logs)
                if (logRecord instanceof AcsLogRecord) {
                        String audience = ((AcsLogRecord) logRecord).getAudience();
                        if(!audience.equals(""))
                                sb.append("Audience=\"" + audience + "\" ");
                        //add Array
                        String array = ((AcsLogRecord) logRecord).getArray();
                        if(!array.equals(""))
                                sb.append("Array=\"" + array + "\" ");
                        //add Antenna
                        String antenna = ((AcsLogRecord) logRecord).getAntenna();
                        if(!antenna.equals(""))
                                sb.append("Antenna=\"" + antenna + "\" ");
                }
		sb.setCharAt(sb.lastIndexOf("") - 1, '>');

		// the log message becomes the text in our XML record
		if (logRecord.getMessage() != null) {
			sb.append(maskMessage(logRecord.getMessage()));
		}

		// <Data> elements: logged exception or error trace, and log parameters

		try {
			// logged exception
			Throwable loggedThrowable = logRecord.getThrown();
			if (loggedThrowable != null) {
				StringWriter exWriter = new StringWriter();
				loggedThrowable.printStackTrace(new PrintWriter(exWriter));
				sb.append("<Data Name=\"LoggedException\">" + maskMessage(exWriter.toString()) + "</Data>");
			}
			// log parameters (except for the special properties which were used already to set specific fields)
			for (Object param : logParamUtil.getNonSpecialPropertiesMapParameters()) {
				if (param instanceof Map) {
					// any map that is not the special properties map we interpret as name-value pairs.
					Map propertiesMap = (Map) param;
					for (Object keyName : propertiesMap.keySet()) {
						String value = maskEmptyDataContent(propertiesMap.get(keyName).toString());
						sb.append("<Data Name=\"" + keyName.toString() + "\">" + maskMessage(value) + "</Data>");
					}
				}
				else {
					// a single parameter was logged, but we have to fit it into our name-value scheme using a fake name
					String value = maskEmptyDataContent(param.toString());
					sb.append("<Data Name=\"LoggedParameter\">" + maskMessage(value) + "</Data>");
				}
			}
		}
		catch (Exception e) {
			// expected not to happen often at all, thus no try blocks inside every loop, so we may lose some <Data>
			sb.append("<Data Name=\"DataConstructionError\">" + maskMessage(e.toString()) + "</Data>");
		}

		// end tag of XML record
		sb.append("</" + levelName + ">");

		String Log = sb.toString();
		//		System.err.println("Logging XML log entry " + Log);
		return Log;
	}

	/**
	 * Escapes characters in the log message which would make the surrounding XML invalid.
	 * Embeds the message text in a <code>&lt;![CDATA[..]]&gt;</code> block.
	 * @return the masked message
	 */
	private String maskMessage(String message) {
		String maskedMessage = "<![CDATA[" + message + "]]>";
		return maskedMessage;
	}

	/**
	 * Escapes characters in a log record attribute which would make the surrounding XML invalid.
	 * Since XML attributes can't use <code>&lt;![CDATA[..]]&gt;</code>, illegal characters in <code>attributeValue</code>
	 * are replaced with the corresponding masked XML notation.
	 * @return the masked attribute value 
	 * @see XmlNormalizer#normalize(java.lang.String)  
	 */
	private String maskAttribute(String attributeValue) {
		return XmlNormalizer.normalize(attributeValue);
	}

	/**
	 * If a Data element has empty content (resulting in <Data></Data>), 
	 * then the xerces parser would throw an exception. 
	 * As a workaround, we replace the empty string with "N/A". 
	 */
	private String maskEmptyDataContent(String content) {
		if (content == null || content.length() == 0) {
			return "N/A";
		}
		return content;
	}

}
