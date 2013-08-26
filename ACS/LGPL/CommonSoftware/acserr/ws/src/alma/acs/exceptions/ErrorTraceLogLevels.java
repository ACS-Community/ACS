/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.exceptions;

import java.util.logging.Level;

import alma.ACSErr.Severity;

/**
 * Provides JDK logger style level objects for the severity levels in {@link alma.ACSErr.Severity}.
 * <p>
 * Currently the module acserr builds before acsjlog, so <code>alma.acs.logging.AcsLogLevel</code> is not available.
 * Todo: Try to refactor this and remove duplication of error level classes.  
 * 
 * @author hsommer
 * created Apr 12, 2005 5:36:27 PM
 */
public class ErrorTraceLogLevels extends Level {

    public static Level ERROR = new ErrorTraceLogLevels("ERROR", 901);
    public static Level CRITICAL = new ErrorTraceLogLevels("CRITICAL", 902);
    public static Level ALERT = new ErrorTraceLogLevels("ALERT", 903);
    public static Level EMERGENCY = new ErrorTraceLogLevels("ERROR", 1000);
    
    protected ErrorTraceLogLevels(String name, int value) {
        super(name, value);
    }

    static Level mapErrorLevelToLogLevel(Severity errorTraceSeverity) {
        
        Level logLevel = null;
        switch (errorTraceSeverity.value()) {
        case Severity._Error:
            logLevel = ERROR;
            break;
        case Severity._Critical:
            logLevel = CRITICAL;
            break;
        case Severity._Alert:
            logLevel = ALERT;
            break;
        case Severity._Emergency:
            logLevel = EMERGENCY;
            break;
        default:
            logLevel = ERROR;
        }
        return logLevel;
    }
}
