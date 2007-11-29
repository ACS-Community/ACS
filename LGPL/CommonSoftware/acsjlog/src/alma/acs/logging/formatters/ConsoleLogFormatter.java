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
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

/**
 * An implementation of <code>java.util.logging.Formatter</code>.
 * Produces single line log reports meant to go to the console.
 * @author	Matej Sekoranja (matej.sekoranjaATcosylab.com)
 */
public class ConsoleLogFormatter extends Formatter
{
	/**
	 * Line separator string. 
	 */
	private static final String lineSeparator = System.getProperty("line.separator");

	/**
	 * ISO 8601 date formatter.
	 */
	private final SimpleDateFormat timeFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
	
	/**
	 * Date object (used not to recreate it every time).
	 */
	private final Date date = new Date();

	/**
	 * Format the given LogRecord.
	 * @param record the log record to be formatted.
	 * @return a formatted log record
	 */
	public String format(LogRecord record)
	{
		StringBuffer sb = new StringBuffer(128);
	
		synchronized (date)
		{
			date.setTime(record.getMillis());
			sb.append(timeFormatter.format(date));
		}
		
		// GCH: 
		// Commented out automatically adding 
		// class and method name.
		// This is not conformant to the logging format for the 
		// other languages and logs formatted int this way can be 
		// confusing for users that are not developers.
		// But uncommented the log level. Java developers are used to 
		// that and it is often convenient

		sb.append(' ');
		sb.append(record.getLevel().getLocalizedName());
		
		// hso 2005-10-14: C++ logs source object and class and method name since a few months.
		// These names are often long in Java, but at least the source object (e.g. component name) should be there.
		// C++ sample: 2005-10-14T12:45:32.924 [GlobalLogger - baci::BACIComponent::startAllThreads] My log message...
		String sourceObject = record.getLoggerName();
		if (sourceObject != null) {
			sb.append(" [").append(sourceObject).append("] ");
        }
		sb.append(record.getMessage());


		// source
		// sb.append('[');
		// if (record.getSourceClassName() != null) 	
		//	sb.append(record.getSourceClassName());
	
		// method name
		// if (record.getSourceMethodName() != null)
		// {	
		// 	sb.append('#');
		//	sb.append(record.getSourceMethodName());
		// }
		// sb.append(']');
		
		sb.append(lineSeparator);


		// exceptions
		if (record.getThrown() != null)
		{
			try
			{
				StringWriter sw = new StringWriter();
				PrintWriter pw = new PrintWriter(sw);
				record.getThrown().printStackTrace(pw);
				pw.close();
				sb.append(sw.toString());
			} catch (Exception ex) {
				// just ignore it
			}
		}

		return new String(sb);
	}

}
