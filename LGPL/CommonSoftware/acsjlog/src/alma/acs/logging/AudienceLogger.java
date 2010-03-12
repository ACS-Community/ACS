// @(#) $Id: AudienceLogger.java,v 1.1 2010/03/12 22:34:31 javarias Exp $
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

import java.util.logging.Level;

public class AudienceLogger {

    // The internal logger  must be an AcsLogger
    private AcsLogger logger;
    
    // Audience name
    private String audience;
    
    public AudienceLogger(AcsLogger logger, String audience) {
        this.logger = logger;
        this.audience = audience;
    }

    public AcsLogger getLogger() {
        return logger;
    }
    
    public void severe(String msg) {
        logger.logToAudience(Level.SEVERE, msg, audience);
    }
 
    public void warning(String msg) {
        logger.logToAudience(Level.WARNING, msg, audience);
    }
    
    public void info(String msg) {
        logger.logToAudience(Level.INFO, msg, audience);
    }
    
    public void config(String msg) {
        logger.logToAudience(Level.CONFIG, msg, audience);
    }
    
    public void fine(String msg) {
        logger.logToAudience(Level.FINE, msg, audience);
    }
    
    public void finer(String msg) {
        logger.logToAudience(Level.FINER, msg, audience);
    }
    
    public void finest(String msg) {
        logger.logToAudience(Level.FINEST, msg, audience);
    }

    public static void severe(String msg, String audience, AcsLogger logger) {
        logger.logToAudience(Level.SEVERE, msg, audience);
    }
    
    public static void warning(String msg, String audience, AcsLogger logger) {
        logger.logToAudience(Level.WARNING, msg, audience);
    }

    public static void info(String msg, String audience, AcsLogger logger) {
        logger.logToAudience(Level.INFO, msg, audience);
    }
    
    public static void config(String msg, String audience, AcsLogger logger) {
        logger.logToAudience(Level.CONFIG, msg, audience);
    }
    
    public static void fine(String msg, String audience, AcsLogger logger) {
        logger.logToAudience(Level.FINE, msg, audience);
    }

    public static void finer(String msg, String audience, AcsLogger logger) {
        logger.logToAudience(Level.FINER, msg, audience);
    }

    public static void finest(String msg, String audience, AcsLogger logger) {
        logger.logToAudience(Level.FINEST, msg, audience);
    }
}
