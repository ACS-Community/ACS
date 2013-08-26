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

import alma.acs.logging.LogParameterUtil;
import alma.acs.util.IsoDateFormat;

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
			sb.append(IsoDateFormat.formatDate(date));
		}
		
		sb.append(' ');
		sb.append(record.getLevel().getLocalizedName());
		
		String sourceObject = record.getLoggerName();
		if (sourceObject != null) {
			sb.append(" [").append(sourceObject).append("] ");
        }
		sb.append(record.getMessage());
				
		// log parameters (except for the special properties which were used already to set specific fields)
		// @TODO check if analyzing the parameters both here and in AcsXMLLogFormatter uses lots of CPU and thus should be unified.
		//       This would probably require us to either use only one log handler for remote and stdout log,
		//       or to no longer stuff all parameters and other extra data into maps that become log parameters,
		//       which allows us to deal with standard LogRecords and not just custom AcsLogRecords.
		LogParameterUtil logParamUtil = new LogParameterUtil(record);
		String paramString = "";
		for (Object param : logParamUtil.getNonSpecialPropertiesMapParameters()) {
			if (param instanceof Map) {
				// any map that is not the special properties map we interpret as name-value pairs.
				Map propertiesMap = (Map) param;
				for (Object keyName : propertiesMap.keySet()) {
					String value = propertiesMap.get(keyName).toString();
					paramString += keyName + "='" + value + "' "; 
				}
			}
			else {
				// a single parameter was logged, but we have to fit it into our name-value scheme using a fake name
				String value = param.toString();
				paramString += "LoggedParameter" + "='" + value + "' "; 
			}
		}
		if (!paramString.isEmpty()) {
			sb.append(" [ " + paramString + "]");
		}
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
