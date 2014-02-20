/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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
package alma.acs.logging.adapters;

import org.jacorb.config.Configuration;
import org.jacorb.config.ConfigurationException;
import org.jacorb.config.LoggingInitializer;

import alma.acs.logging.ClientLogManager;
import alma.acs.logging.level.AcsLogLevelDefinition;

/**
 * See JacORR programming guide: "For more sophisticated needs, it is also possible 
 * to provide a LoggingInitializer class, which is a hook provided by JacORB to allow configuration 
 * of a logging system based on the JacORB configuration. 
 * The class needs to extend the class org.jacorb.config.LoggingInitializer 
 * and the name of the class needs to be specified in the property jacorb.log.initializer"
 * <p>
 * This class has evolved from the "JacORBLoggerFactory" class that was used with jacorb 2.2.4 
 * and the apache avalon log framework. Now JacORB uses slf4j logging so that the "getNamedLogger" methods 
 * are no longer needed here because slf4j has its own ACS-supplied logger factory, 
 * see {@link org.slf4j.impl.JacorbLoggerHelper}.
 * 
 * @author hsommer
 */
public class AcsJacorbLoggingInitializer extends LoggingInitializer {
	
	private static int jacOrbVerbosity = -1;
	
	@Override
	public void init(Configuration config) {
		try {
			jacOrbVerbosity = config.getAttributeAsInteger("jacorb.log.default.verbosity");
		} catch (ConfigurationException e) {
			// just leave it -1
			// e.printStackTrace();
		}
		// System.out.println("******** JacORB LoggingInitializer.init called. Verbosity=" + jacOrbVerbosity);
	}

	
	/**
	 * This method supports {@link ClientLogManager#getLoggerForCorba(String, boolean)}
	 * with finding the right custom log level for the jacorb logger.
	 * @return The log level that corresponds to <code>jacorb.log.default.verbosity</code>, 
	 *         or <code>null</code> if no such log level could be found. 
	 */
	public static AcsLogLevelDefinition getLogLevelFromJacorbVerbosity() {
		// We can't use the same log levels that the container logger uses, because JacORB is much too verbose compared with ALMA code.
		// TODO: Verify if this is still the case with JacORB 3.4
		// Thus we restrict this Logger's level, assuming that the log handler will be generous enough always.
		// TODO: use custom Logger configuration in the CDB for Corba logger, instead of JacORB property
		// TODO: use a custom logger adapter, which translates the given avalon log levels to lower jdk log levels than is done now.
		
		// Log level description in orb.properties:
		// 0 = no logging (OFF)
		// 1 = errors (SEVERE)
		// 2 = warnings (WARNING)
		// 3 = informational messages (INFO)
		// 4 = debug-level output (FINE)
		AcsLogLevelDefinition[] levelMap = new AcsLogLevelDefinition[] {
				AcsLogLevelDefinition.EMERGENCY, AcsLogLevelDefinition.WARNING, AcsLogLevelDefinition.INFO, AcsLogLevelDefinition.DEBUG, AcsLogLevelDefinition.TRACE };
		
		if (jacOrbVerbosity >= 0 && jacOrbVerbosity <=4) {
			return levelMap[jacOrbVerbosity];
		}
		else {
			return null;
		}
	}

}
