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
package alma.acs.monitoring.blobber;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;


/**
 * Creates a CLOB string intended for Oracle TMCDB storage from MonitorPointTimeSeries. 
 * TODO: Move this to the Oracle DAO and keep the ACS blobber layer free of CLOBs.
 * 
 * @author hsommer
 */
class Clobber
{
	private final Logger logger;

	/**
	 * @param logger
	 */
	Clobber(Logger logger) {
		this.logger = logger;
	}
	
	/**
	 * @param mpTs
	 * @return The CLOB, or null if the monitor point is of a floating number type and contains NaN
	 */
	String generateClob(MonitorPointTimeSeries mpTs) {
		
		List<MonitorPointValue> timeSeriesData = mpTs.getDataList();
		StringBuilder clobBld = new StringBuilder();
		
		for (int i = 0; i < timeSeriesData.size(); i++) {
			MonitorPointValue mpVal = timeSeriesData.get(i);
			
			List<Object> mpValData = mpVal.getData();
			if (!mpValData.isEmpty()) {
				
				// print the time
				if (i > 0) {
					clobBld.append('|');
				}
				clobBld.append(mpVal.getTime()).append('|');
				
				// We check only the first object;
				// other objects in mpValData are expected to be of the same type and same NaN status
				Object sampleObj = mpValData.get(0); 
				
				// This if-else structure will allow us to use type-specific DecimalFormat etc masks
				if (sampleObj instanceof Integer) {
					appendValueString(mpValData, clobBld);
				}
				else if (sampleObj instanceof Long) {
					appendValueString(mpValData, clobBld);
				}
				else if (sampleObj instanceof Float) {
					// Skip NaN values (COMP-5564)
					if (((Float)sampleObj).isNaN()) {
						return null;
					}
					appendValueString(mpValData, clobBld);
				}
				else if (sampleObj instanceof Double) {
					// Skip NaN values (COMP-5564)
					if (((Double)sampleObj).isNaN() ) {
						return null;
					}
					appendValueString(mpValData, clobBld);
				}
				else if (sampleObj instanceof Boolean) {
					// according to http://jira.alma.cl/browse/COMP-8496, we want to store
					// "1" for true and "0" for false. Thus cannot use Boolean.toString().
					List<Object> boolStrings = new ArrayList<Object>(mpValData.size());
					for (int j = 0; j < mpValData.size(); j++) {
						boolStrings.add((((Boolean)mpValData.get(j)).booleanValue() ? "1" : "0"));
					}
					appendValueString(boolStrings, clobBld);
				}
				else if (sampleObj instanceof String) {
					appendValueString(mpValData, clobBld);
				}
				else {
					logger.info("Unexpected data type " + sampleObj.getClass().getName() + " found, coming from Corba type " + mpTs.getCorbaTypeId());
					clobBld.append("?");
				}
			}
		}
		
		// TODO: Must we keep this trailing newline char from the legacy implementation?
		clobBld.append('\n');
		
		return clobBld.toString();
	}
	
	/**
	 * Appends the single value or space-separated list of values given in mpValData.
	 * 
	 * TODO: Pass formatting info if needed.
	 * @param mpValData
	 * @param clobBld
	 */
	private void appendValueString(List<Object> mpValData, StringBuilder clobBld) {
		for (int j = 0; j < mpValData.size(); j++) {
			Object valueObj = mpValData.get(j);
			clobBld.append(valueObj.toString());
			if (j < mpValData.size() - 1) {
				clobBld.append(' ');
			}
		}
	}
	
}
