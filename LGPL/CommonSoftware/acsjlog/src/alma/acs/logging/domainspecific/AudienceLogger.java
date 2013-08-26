/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/

package alma.acs.logging.domainspecific;

import java.util.logging.Level;

import alma.acs.logging.AcsLogger;

/**
 * Base class for the various fixed-audience loggers.
 * As of ACS 9.0, the following audience types are defined in acscommon.idl:
 *  const string OPERATOR = "Operator"; <br>
 *  const string DEVELOPER = "Developer"; <br>
 *  const string SYSADMIN = "Sysadmin"; <br>
 *  const string DBA = "DBA"; <br>
 *  const string SCILOG = "SciLog"; <br>
 *  const string NO_AUDIENCE = ""; <br>
 *  but we only provide the convenience wrappers for the most popular audiences: Operator, Developer, SciLog.
 *  For other audiences, the static method {@link #log(Level, String, Throwable, AcsLogger, Audience)}
 *  can be used, or {@link AcsLogger#logToAudience(Level, String, String)} with a generic audience string
 *  which is especially useful when a new audience type gets introduced and ACS has not added support for it yet.
 * <p>
 * The audience loggers are wrappers around an AcsLogger, calling 
 * {@link AcsLogger#logToAudience(Level, String, String)}
 * with a fixed audience string that corresponds to an {@link Audience} enum.
 * The goal is to keep the "audience" concept generic in AcsLogger, and confine
 * the usage of concrete alma audience types to this subpackage, e.g. {@link ScienceLogger}.
 * <p>
 * See http://jira.alma.cl/browse/COMP-3130 about adopting this code 
 * from CONTROL/Common/Define/src/alma/Control/Common/*Logger.java to ACS.
 */
public abstract class AudienceLogger {

	public static enum Audience {
		
		// update this list when a new audience type gets added in alma 
		OPERATOR(alma.log_audience.OPERATOR.value),
		DEVELOPER(alma.log_audience.DEVELOPER.value),
		SYSADMIN(alma.log_audience.SYSADMIN.value),
		DBA(alma.log_audience.DBA.value),
		SCILOG(alma.log_audience.SCILOG.value),
		NO_AUDIENCE(alma.log_audience.NO_AUDIENCE.value);
		
		private final String idlName;

		private Audience(String idlName) {
			this.idlName = idlName;
		}
		
		public String getIdlName() {
			return idlName;
		}
	}	
	
    // The internal logger  must be an AcsLogger
    private final AcsLogger delegateLogger;
    
    // Audience name
    private final Audience audience;
    
    protected AudienceLogger(AcsLogger logger, Audience audience) {
        this.delegateLogger = logger;
        this.audience = audience;
        logger.addLoggerClass(AudienceLogger.class);
    }

    public AcsLogger getLogger() {
        return delegateLogger;
    }

    /**
     * Static log method, useful for rare audience types that don't have their own subclass (yet).
     */
    public static void log(Level level, String msg, Throwable thr, AcsLogger acsLogger, Audience audience) {
    	acsLogger.addLoggerClass(AudienceLogger.class);
    	acsLogger.logToAudience(level, msg, thr, audience.getIdlName());
    }
    
    /**
     * To be called by static methods of subclasses only.
     */
    protected static void log(Level level, String msg, Throwable thr, AcsLogger acsLogger, Audience audience, Class<? extends AudienceLogger> staticLoggerClass) {
    	acsLogger.addLoggerClass(staticLoggerClass);
    	acsLogger.addLoggerClass(AudienceLogger.class);
    	acsLogger.logToAudience(level, msg, thr, audience.getIdlName());
    }

    /**
     * Log with throwable
     */
    public void log(Level level, String msg, Throwable thr) {
    	delegateLogger.logToAudience(level, msg, thr, audience.getIdlName());
    }
    
    public void severe(String msg) {
        delegateLogger.logToAudience(Level.SEVERE, msg, audience.getIdlName());
    }
 
    public void warning(String msg) {
        delegateLogger.logToAudience(Level.WARNING, msg, audience.getIdlName());
    }
    
    public void info(String msg) {
        delegateLogger.logToAudience(Level.INFO, msg, audience.getIdlName());
    }
    
    public void config(String msg) {
        delegateLogger.logToAudience(Level.CONFIG, msg, audience.getIdlName());
    }
    
    public void fine(String msg) {
        delegateLogger.logToAudience(Level.FINE, msg, audience.getIdlName());
    }
    
    public void finer(String msg) {
        delegateLogger.logToAudience(Level.FINER, msg, audience.getIdlName());
    }
    
    public void finest(String msg) {
        delegateLogger.logToAudience(Level.FINEST, msg, audience.getIdlName());
    }

}
