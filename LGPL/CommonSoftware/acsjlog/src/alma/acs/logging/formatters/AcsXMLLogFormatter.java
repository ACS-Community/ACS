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
import java.net.InetAddress;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

import alma.acs.logging.ACSCoreLevel;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.XmlNormalizer;

/**
 * @author rgeorgie
 *
 * Class that is responsible for formatting the log records/elements of different levels 
 * as well as assigning the right values to their attributes. 
 */
public class AcsXMLLogFormatter extends Formatter implements ACSCoreLevel
{

	private static final SimpleDateFormat df = new SimpleDateFormat("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSS");

	private static String localHostName;
    
    
	/**
	 * Constructs the XML log message that can be sent to the ACS logging service.
	 * @see java.util.logging.Formatter#format(java.util.logging.LogRecord)
	 */
	public String format(LogRecord logRecord)
	{
		// log level 
		AcsLogLevel acsLevel = AcsLogLevel.getNativeLevel(logRecord.getLevel());
		if (acsLevel == null) {
			return "";
		}		
		final int acsCoreLevel = acsLevel.getAcsLevel();
		final String levelName = acsLevel.getEntryName();


		// get date
		Date date = new Date(logRecord.getMillis());
		String TimeStamp = df.format(date);
		
		LogParameterExtractor logParamExtractor = new LogParameterExtractor();
		logParamExtractor.setCurrentLogRecord(logRecord);		


		StringBuffer sb = new StringBuffer("");

		sb.append("<");
		sb.append(levelName);
		sb.append(" ");

		sb.append("TimeStamp=\"" + TimeStamp + "\" ");

		String file = logRecord.getSourceClassName();
		if (file == null)
		{
			if (acsCoreLevel == ACS_LEVEL_DEBUG)
				sb.append("File=\"unknown\" ");
		}
		else
			sb.append("File=\"" + file + "\"  ");

		long line = logParamExtractor.extractLongProperty(LogParameterExtractor.PARAM_LINE, -1);
		if (line < 0)
		{
			if (acsCoreLevel == ACS_LEVEL_TRACE || acsCoreLevel == ACS_LEVEL_DEBUG)
				sb.append("Line=\"0\" ");
		}
		else
			sb.append("Line=\"" + line + "\" ");

		String Routine = logRecord.getSourceMethodName();
		if (Routine == null)
		{
			if (acsCoreLevel == ACS_LEVEL_TRACE)
				sb.append("Routine=\"unknown\" ");
		}
		else {
			sb.append("Routine=\"" + maskAttribute(Routine) + "\" ");
		}

        // host name: may be different from local host if ErrorTrace gets logged
        String hostName = logParamExtractor.extractStringProperty("HostName", null);
        if (hostName == null || hostName.length() == 0) {
            hostName = this.getLocalHostName();
        }
		sb.append("Host=\"" + hostName + "\" ");

		String process = logRecord.getLoggerName();
		if (process != null)
			sb.append("Process=\"LoggerName: " + process + "\" ");


		// source object: the container name or component name
		String sourceObject = ClientLogManager.stripKnownLoggerNamespacePrefix(logRecord.getLoggerName());
		if (sourceObject != null) {
			sb.append("SourceObject=\"" + sourceObject + "\" ");
        }
		// add thread ID, or name if given		
		String threadName = logParamExtractor.extractStringProperty(LogParameterExtractor.PARAM_THREAD_NAME, null);
		if (threadName != null && threadName.length() > 0)
			sb.append("Thread=\"" + threadName + "\" ");
		else if (logRecord.getThreadID() >= 0)
			sb.append("Thread=\"" + logRecord.getThreadID() + "\" ");

		// add context		
		String context = logParamExtractor.extractStringProperty("Context", null);
		if (context != null)
			sb.append("Context=\"" + context + "\" ");

		// add stack info
		if (acsCoreLevel >= ACS_LEVEL_WARNING)
		{
			// add stack id
			String stackId = logParamExtractor.extractStringProperty("StackId", null);
			if (stackId == null)
				sb.append("StackId=\"unknown\" ");
			else
				sb.append("StackId=\"" + stackId + "\" ");

			// add stack idlevel
			long stackLevel = logParamExtractor.extractLongProperty("StackLevel", -1);
			if (stackLevel < 0)
				sb.append("StackLevel=\"0\" ");
			else
				sb.append("StackLevel=\"" + stackLevel + "\" ");
		}

		// add log id		
		long logId = logRecord.getSequenceNumber();
		if (logId >= 0)
			sb.append("LogId=\"" + logId + "\" ");

		// add URI		
		String uri = logParamExtractor.extractStringProperty("Uri", null);
		if (uri != null)
			sb.append("Uri=\"" + uri + "\" ");

		// add priority
		// to be written only different as entry priority		
		long priority = logParamExtractor.extractLongProperty("Priority", acsCoreLevel);
		if (priority != acsCoreLevel)
			sb.append("Priority=\"" + priority + "\" ");

		// add additional attributes here
		// e.g. name="value"
		Map attributes = logParamExtractor.extractMapProperty("Attributes", null);
		if (attributes != null)
		{
			Iterator iter = attributes.keySet().iterator();
			while (iter.hasNext())
			{
				Object key = iter.next();
				sb.append(key + "=\"" + attributes.get(key) + "\" ");
			}
		}

		sb.setCharAt(sb.lastIndexOf("") - 1, '>');

		// the log message becomes the text in our XML record
		if (logRecord.getMessage() != null)
		{
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
			// error trace free-format properties from AcsJException#log (see module acserr)
			Map errorTraceProperties = logParamExtractor.extractMapProperty("ErrorTraceProperties", null);
			if (errorTraceProperties != null) {
				for (Iterator iter = errorTraceProperties.keySet().iterator(); iter.hasNext();) {
					String key = (String) iter.next();
					String value = maskEmptyDataContent((String) errorTraceProperties.get(key));
				    sb.append("<Data Name=\"" + key + "\">" + maskMessage(value) + "</Data>");
				}
			}
			// other
			Object[] params = logParamExtractor.getNonPropertiesMapParameters();
			for (int i = 0; i < params.length; i++) {
				String value = maskEmptyDataContent(params[i].toString());
			    sb.append("<Data Name=\"LoggedParameter\">" + maskMessage(value) + "</Data>");
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
	 * @return the masked atttribute value 
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
    
	/**
	 * Method getHost. Returns the host on which the system is run.
	 * @return String
	 */
	private String getLocalHostName() {
        if (localHostName == null) {
            try {
                localHostName = InetAddress.getLocalHost().getHostName();
            } catch (Exception e) {
                localHostName = "localHost(unknown)";
            }
        }
        return localHostName;
    }
}
