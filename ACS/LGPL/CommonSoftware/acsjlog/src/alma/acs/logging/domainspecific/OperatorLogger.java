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
import alma.acs.logging.domainspecific.AudienceLogger.Audience;

/**
 * An {@link AudienceLogger} using the fixed audience string "Operator",
 * as defined in IDL (acscommon.idl, compiled to {@link alma.log_audience.OPERATOR.value})
 * and mirrored in enum {@link Audience#OPERATOR}.
 * <p>
 * Usage examples: 
 * <ul>
 *   <li><em>as wrapper object:</em> <br>
 *       OperatorLogger opLogger = new OperatorLogger(myAcsLogger); <br>
 *       opLogger.info("some log");<br> 
 *       opLogger.log("log with exception", Level.INFO, myEx)</li>
 *   <li><em>static call:</em> <br>
 *       OperatorLogger("some other log", myAcsLogger); </li>
 * </ul>
 * <p>
 * See http://jira.alma.cl/browse/COMP-3130 about adopting this code 
 * from CONTROL/Common/Define/src/alma/Control/Common/*Logger.java to ACS.
 */
public class OperatorLogger extends AudienceLogger {

	public static final Audience staticAudience = Audience.OPERATOR;
	
    public OperatorLogger(AcsLogger logger) {
        super(logger, staticAudience);
    }

    public static void log(Level level, String msg, Throwable thr, AcsLogger acsLogger) {
    	AudienceLogger.log(level, msg, thr, acsLogger, staticAudience, OperatorLogger.class);
    }
    
    public static void severe(String msg, AcsLogger logger) {
        AudienceLogger.log(Level.SEVERE, msg, null, logger, staticAudience, OperatorLogger.class);
    }

    public static void warning(String msg, AcsLogger logger) {
    	AudienceLogger.log(Level.WARNING, msg, null, logger, staticAudience, OperatorLogger.class);
    }

    public static void info(String msg, AcsLogger logger) {
    	AudienceLogger.log(Level.INFO, msg, null, logger, staticAudience, OperatorLogger.class);
    }

    public static void config(String msg, AcsLogger logger) {
    	AudienceLogger.log(Level.CONFIG, msg, null, logger, staticAudience, OperatorLogger.class);
    }

    public static void fine(String msg, AcsLogger logger) {
    	AudienceLogger.log(Level.FINE, msg, null, logger, staticAudience, OperatorLogger.class);
    }

    public static void finer(String msg, AcsLogger logger) {
    	AudienceLogger.log(Level.FINER, msg, null, logger, staticAudience, OperatorLogger.class);
    }

    public static void finest(String msg, AcsLogger logger) {
    	AudienceLogger.log(Level.FINEST, msg, null, logger, staticAudience, OperatorLogger.class);
    }
}
