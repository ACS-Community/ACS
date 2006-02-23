/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.logging;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

/**
 * An implementation of <code>java.util.logging.Formatter</code>.
 * Produces single line log reports meant to go to the console.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ConsoleLoggingFormatter extends Formatter
{
	/**
	 * ISO 8601 date formatter.
	 */
	private static SimpleDateFormat timeFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
	
	/**
	 * Date object (used not to recreate it every time).
	 */
	private static Date date = new Date();

	/**
	 * Formats the log record according to the rules outlined in this class' javadoc.
	 * 
	 * @param record	the log record to format, may be of <code>MessageLogEntry</code> type
	 * 					to provide additional information, non-<code>null</code>
	 * @return			the log record formatted as a string
	 */
	public String format(LogRecord record)
	{
		assert (record != null);
		
		return createShortReport(record);
	}

	
	/**
	 * Formats the log record. This method is declared static so that it may be used
	 * by other classes, for instance <code>MessageLogEntry.toString()</code>.
	 * 
	 * @param entry	the entry to format
	 * @return			entry formatted to a string
	 */
	public static String createShortReport(LogRecord entry) 
	{
		StringBuffer sb = new StringBuffer(128);
	
		synchronized (date)
		{
			date.setTime(entry.getMillis());
			sb.append(timeFormatter.format(date));
		}
		
		sb.append(' ');
		sb.append(entry.getMessage());

		return new String(sb);
	}
	
	/**
	 * Returns a short summary about this instance.
	 * 
	 * @return internal state of this
	 */
	public String toString()
	{
		return "ConsoleLoggingFormatter";
	}
}
