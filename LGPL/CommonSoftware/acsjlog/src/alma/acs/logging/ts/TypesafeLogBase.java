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
package alma.acs.logging.ts;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogRecord;
import alma.acs.logging.AcsLogger;

/**
 * Base class for all generated type-safe log classes.
 * Logically this class belongs to module "loggingts",
 * but there it would be the only java class, and would require us to create a jar file for it.
 * Thus for pragmatic reasons we keep it here in "acsjlog".
 * <p> 
 * See http://jira.alma.cl/browse/COMP-3717 about the request to extract a base class.
 * 
 * @author hsommer
 */
public abstract class TypesafeLogBase
{
	protected final Logger logger;
	protected final Map<String, Object> nameValue;

	protected final AcsLogRecord lr;

	
	protected TypesafeLogBase(Logger logger, String logName, AcsLogLevel level, String audience, String msg, String array, String antenna) {
		
		this.logger=logger;
		if (logger instanceof AcsLogger) {
			((AcsLogger)logger).addLoggerClass(TypesafeLogBase.class);
			((AcsLogger)logger).addLoggerClass(this.getClass());
		}
		nameValue = new LinkedHashMap<String, Object>();
		nameValue.put("logName", logName);
		
		lr = new AcsLogRecord(level, msg, nameValue, logger.getName());
		lr.setAudience(audience);

		lr.setArray(array);
		lr.setAntenna(antenna);
	}
	
	
	public TypesafeLogBase setArray(String array) {
		lr.setArray(array);
		return this;
	}
	
	public TypesafeLogBase setAntenna(String antenna) {
		lr.setAntenna(antenna);
		return this;
	}
	
	public String getArray(){
		return lr.getArray();
	}
	
	public String getAntenna(){
		return lr.getAntenna();
	}
	
	/**
	 * Logs the message through the Logger supplied in the constructor, with the configured log level.
	 */	
	public void log() {
		logger.log(lr);
	}
	
}
