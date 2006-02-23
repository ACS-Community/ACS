/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.logging;

import java.net.InetAddress;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;
import alma.acs.util.ACSPorts;

/**
 * Efficient ACS XML logging formatter.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class LoggingFormatter extends Formatter implements ACSCoreLevel
{
	/**
	 * ISO 8601 date formatter.
	 */
	private static final SimpleDateFormat timeFormatter =
		new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");

	/**
	 * Name of the localhost;
	 */
	private static String host = null;

	/**
	 * XML CDATA prefix string.
	 */
	private static final String CDATA_PREFIX = "<![CDATA["; 

	/**
	 * XML CDATA postfix string.
	 */
	private static final String CDATA_POSTFIX = "]]>";

	/**
	 * Constructor (init) method name (Java specific).
	 */
	private static final String INIT_METHOD_NAME = "<init>";

	/**
	 * Constructor (init) method name (Java specific) send by this formatter,
	 * since '<' cannot appear in attribute value.
	 */
	private static final String INIT_METHOD_NAME_REPLACEMENT = "&lt;init&gt;";

	/**
	 * Constructor for LoggingFormatter.
	 */
	public LoggingFormatter()
	{
		super();

		// cache
		getHost();
	}

	/**
	 * Returns the name of the locahost.
	 * @return		the name of the locahost
	 */
	public String getHost()
	{
		if (host == null)
		{
			try
			{
				host = InetAddress.getLocalHost().getHostName();
			} catch (Exception ex)
			{
				// guards against security exception (applet sandbox)
				host = ACSPorts.getIP();
			}
		}
		return host;
	}
	
	/**
	 * Extract property with specified name of type long from the <code>Map</code>
	 * contained in the object of type <code>java.lang.Long</code>.
	 * @param name			name of the property
	 * @param properties	map of prdoperties
	 * @param defaultValue value returned if failed to obtain property value
	 * @return				value, <code>defaultValue</code> on failure
	 */
	private long extractLongProperty(String name, Map properties, long defaultValue)
	{
	
		long retVal = defaultValue;
		if (properties != null)
		{ 
			Object lv = properties.get(name);
			if (lv instanceof Long)
				retVal = ((Long)lv).longValue();
		}
		
		return retVal;
	}

	/**
	 * Extract property with specified name of type String from the <code>Map</code>
	 * contained in the object of type <code>java.lang.String</code>.
	 * @param name			name of the property
	 * @param properties	map of prdoperties
	 * @param defaultValue value returned if failed to obtain property value
	 * @return				value, <code>defaultValue</code> on failure
	 */
	private String extractStringProperty(String name, Map properties, String defaultValue)
	{
	
		String retVal = defaultValue;
		if (properties != null)
		{ 
			Object sv = properties.get(name);
			if (sv != null)
				retVal = sv.toString();
		}
		
		return retVal;
	}

	/**
	 * Extract property with specified name of type Map from the <code>Map</code>
	 * contained in the object of type <code>java.util.Map</code>.
	 * @param name			name of the property
	 * @param properties	map of prdoperties
	 * @param defaultValue value returned if failed to obtain property value
	 * @return				value, <code>defaultValue</code> on failure
	 */
	private Map extractMapProperty(String name, Map properties, Map defaultValue)
	{
	
		Map retVal = defaultValue;
		if (properties != null)
		{ 
			Object mv = properties.get(name);
			if (mv != null)
				retVal = (Map)mv;
		}
		
		return retVal;
	}

	/**
	 * Formats <code>record</code> to ACS logging XML string.
	 * 
	 * Additional parameters should be stored as <code>java.util.Map</code> object in
	 * record parameters, obtained using <code>java.util.logging.LogRecord#getParameters()</code>
	 * method.
	 * Suppoted key-value pairs are:
	 * <ul>
	 * 	<li><b>Line</b>			- <code>java.lang.Long</code>
	 * 	<li><b>Context</b>		- <code>java.lang.String</code>
	 * 	<li><b>StackId</b>		- <code>java.lang.String</code>
	 * 	<li><b>StackLevel</b>	- <code>java.lang.Long</code>
	 * 	<li><b>Uri</b>			- <code>java.lang.String</code>
	 * 	<li><b>Priority</b>		- <code>java.lang.Long</code>
	 * 	<li><b>Data</b>			- <code>java.util.Map</code>
	 * 	<li><b>Attributes</b>	- <code>java.util.Map</code>
	 * 	<li><b>ThreadName</b>	- <code>java.lang.String</code>
	 * </ul>
	 * 
	 * See 'Logging and Archiving' document for details.
	 * 
	 * @see java.util.logging.LogRecord#getParameters()
	 * @see java.util.logging.Formatter#format(LogRecord)
	 */
	public String format(LogRecord record)
	{

		// obtain native level
		LoggingLevel nativeLevel = LoggingLevel.getNativeLevel(record.getLevel());
		if (nativeLevel == null)
			return null;
			
		int acsLevel = nativeLevel.getAcsLevel();
		
		// get ISO 8601 date 
		String timestamp = timeFormatter.format(new Date(record.getMillis()));

		// obtain properties, we support only properties containing Map object
		// find and take first
		Map properties = null;
		Object[] parameters = record.getParameters();
		if (parameters!=null)
			for (int i=0; i<parameters.length && properties==null; i++)
				if (parameters[i] instanceof Map)
					properties = (Map)parameters[i];
		
		// the average size of the log is somewhere 200+ chars,
		// so we will use buffer of 250
		StringBuffer sb = new StringBuffer(250);

		// add entry
		sb.append("<");
		sb.append(nativeLevel.getEntryName());
		
		// add timestamp
		sb.append(" TimeStamp=\"" + timestamp + "\" ");


		// add file if specified, it is necessary for ACS_LEVEL_TRACE and ACS_LEVEL_DEBUG
		String file = record.getSourceClassName();
		if (file == null)
		{
			if (acsLevel == ACS_LEVEL_TRACE ||
				acsLevel == ACS_LEVEL_DEBUG)
				sb.append("File=\"unknown\" ");
		}
		else
			sb.append("File=\"" + file + "\" ");


		// add line number if specified, it is necessary for ACS_LEVEL_TRACE and ACS_LEVEL_DEBUG
		long line = extractLongProperty("Line", properties, -1);
		if (line < 0)
		{
			if (acsLevel == ACS_LEVEL_TRACE ||
				acsLevel == ACS_LEVEL_DEBUG)
				sb.append("Line=\"0\" ");
		}
		else
			sb.append("Line=\"" + line + "\" ");


		// add routine name if specified, it is necessary for ACS_LEVEL_TRACE and ACS_LEVEL_DEBUG
		String routine = record.getSourceMethodName();
		if (routine == null)
		{
			if (acsLevel == ACS_LEVEL_TRACE ||
				acsLevel == ACS_LEVEL_DEBUG)
				sb.append("Routine=\"unknown\" ");
		}
		else if (routine.equals(INIT_METHOD_NAME))
			sb.append("Routine=\"" + INIT_METHOD_NAME_REPLACEMENT + "\" ");
		else
			sb.append("Routine=\"" + routine + "\" ");


		// add host name
		sb.append("Host=\"" + getHost() + "\" ");

		// add process name
		String process = record.getLoggerName();
		if (process != null)
			sb.append("Process=\"" + process + "\" ");
		
		// add thread ID, or name if given
		String threadName = extractStringProperty("ThreadName", properties, null);
		if (threadName != null && threadName.length() > 0)
			sb.append("Thread=\"" + threadName + "\" ");
		else if (record.getThreadID() >= 0)
			sb.append("Thread=\"" + record.getThreadID() + "\" ");
		
		// add context
		String context = extractStringProperty("Context", properties, null);
		if (context != null)
			sb.append("Context=\"" + context + "\" ");

		// add stack info
		if (acsLevel >= ACS_LEVEL_WARNING)
		{
			// add stack id
			String stackId = extractStringProperty("StackId", properties, null);
			if (stackId == null)
				sb.append("StackId=\"unknown\" ");
			else
				sb.append("StackId=\"" + stackId + "\" ");

			// add stack idlevel
			long stackLevel = extractLongProperty("StackLevel", properties, -1);
			if (stackLevel < 0)
				sb.append("StackLevel=\"0\" ");
			else
				sb.append("StackLevel=\"" + stackLevel + "\" ");
		}

		// add log id
		long logId = record.getSequenceNumber();
		if (logId >= 0)
			sb.append("LogId=\"" + logId + "\" ");

		// add URI
		String uri = extractStringProperty("Uri", properties, null);
		if (uri != null)
			sb.append("Uri=\"" + uri + "\" ");

		// add priority
		// to be written only different as entry priority		
		long priority = extractLongProperty("Priority", properties, acsLevel);
		if (priority != acsLevel)
			sb.append("Priority=\"" + priority + "\" ");

		// add additional attributes here
		// e.g. name="value"
		Map attributes = extractMapProperty("Attributes", properties, null);
		if (attributes != null)
		{
			Iterator iter = attributes.keySet().iterator();
			while (iter.hasNext())
			{
				Object key = iter.next();
				sb.append(key + "=\"" + attributes.get(key) + "\" ");
			}
		}

		// override last space and terminate entry
		sb.setCharAt(sb.lastIndexOf("") - 1, '>');

		// and message
		String message = record.getMessage();
		if (message != null) {
			
			// check for '<', '>', and '&' characters, and add it to CDATA block if necessary
		    // note: only the characters '<' and '&' are strictly illegal in XML
			if ((message.indexOf('<') != -1 || message.indexOf('>') != -1 || message.indexOf('&') != -1) &&
				!message.startsWith(CDATA_PREFIX))
			{
				sb.append(CDATA_PREFIX);
				sb.append(message);
				sb.append(CDATA_POSTFIX);
			}
			else
				sb.append(message);
		}
			
		// add 'Data' entries here
		// e.g. <Data Name="name">value</Data>
		Map data = extractMapProperty("Data", properties, null);
		if (data != null)
		{
			Iterator iter = data.keySet().iterator();
			while (iter.hasNext())
			{
				Object key = iter.next();
				sb.append("<Data Name=\"" + key + "\">" + data.get(key) + "</Data>");
			}
		}

		// finalize element
		sb.append("</" + nativeLevel.getEntryName() + ">");
		
		return sb.toString();
	}

}
