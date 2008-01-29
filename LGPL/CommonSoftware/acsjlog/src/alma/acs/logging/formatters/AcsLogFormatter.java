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

import java.net.InetAddress;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

import org.omg.CORBA.Any;

/**
 * @author rgeorgie
 *
 * Class that is responsible for formatting the log records/elements of different levels 
 * as well as assigning the right values to their attributes. 
 */
public abstract class AcsLogFormatter extends Formatter 
{
	protected static String localHostName;
    
    abstract public Any formatAny(Any anyLogRecord, LogRecord logRecord);   
    
	/**
	 * Method getHost. Returns the host on which the system is run.
	 * @return String
	 */
	protected String getLocalHostName() {
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
