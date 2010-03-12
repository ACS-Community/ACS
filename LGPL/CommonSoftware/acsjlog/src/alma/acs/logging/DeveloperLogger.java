// @(#) $Id: DeveloperLogger.java,v 1.1 2010/03/12 22:34:31 javarias Exp $
//
// ALMA - Atacama Large Millimeter Array
// (c) Associated Universities Inc., 2010
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
//

package alma.acs.logging;

import alma.acs.logging.AcsLogger;
import alma.log_audience.DEVELOPER;

public class DeveloperLogger extends AudienceLogger {

    public DeveloperLogger(AcsLogger logger) {
        super(logger, DEVELOPER.value);
    }

    public static void severe(String msg, AcsLogger logger) {
        AudienceLogger.severe(msg, DEVELOPER.value, logger);
    }

    public static void warning(String msg, AcsLogger logger) {
        AudienceLogger.warning(msg, DEVELOPER.value, logger);
    }

    public static void info(String msg, AcsLogger logger) {
        AudienceLogger.info(msg, DEVELOPER.value, logger);
    }

    public static void config(String msg, AcsLogger logger) {
        AudienceLogger.config(msg, DEVELOPER.value, logger);
    }

    public static void fine(String msg, AcsLogger logger) {
        AudienceLogger.fine(msg, DEVELOPER.value, logger);
    }

    public static void finer(String msg, AcsLogger logger) {
        AudienceLogger.finer(msg, DEVELOPER.value, logger);
    }

    public static void finest(String msg, AcsLogger logger) {
        AudienceLogger.finest(msg, DEVELOPER.value, logger);
    }
}
