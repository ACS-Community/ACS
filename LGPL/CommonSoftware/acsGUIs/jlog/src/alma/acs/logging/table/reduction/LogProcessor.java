/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2009, All rights reserved
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
package alma.acs.logging.table.reduction;

import java.util.Collections;
import java.util.List;
import java.util.Vector;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * An object to reduce the number of logs in an array by applying
 * some kind of reduction rules.
 * 
 * @author acaproni
 *
 */
public class LogProcessor {
	
	// The reduction rules
	private final List<ReductionRule> rules = Collections.synchronizedList(new Vector<ReductionRule>()); 

	/**
	 * Reduce the logs by applying the reduction rules.
	 * 
	 * @param logs The logs to reduce
	 */
	public void reduce(Vector<ILogEntry> logs) {
		for (int t=0; t<logs.size(); t++) {
			// This is the log that "could" reduce other logs
			ILogEntry log = logs.get(t);
			if (log==null) {
				// Should never happen
				continue;
			}
			ReductionRule rule;
			try {
				rule = new SourceAntennaRule(log);
				if (!rule.isReducible()) {
					rule = new AntennaRule(log);
					if (!rule.isReducible()) {
						continue;
					}	
				}
			} catch (Throwable th) {
				System.out.println("LogProcessor: error instantiating a Reduction rule: "+th.getMessage());
				if (log==null) {
					System.out.println("The problematic log is NULL");
				} else {
					System.out.println("Problematic log: "+log.toString());
				}
				th.printStackTrace();
				continue;
			}
			// Now scan the remaining logs in the vector, apply the rule
			// and if it is the case remove the reduced log.
			int j=t+1;
			while (j<logs.size()) {
				ILogEntry reducibleLog = logs.get(j);
				if (rule.applyRule(reducibleLog)) {
					// The log has to be deleted from the vector
					logs.remove(j);
					continue;
				}
				j++;
			}
			// All the logs have been checked against the rule
			
			if (rule.isReducingLogs()) {
				// Create the new log with the message
				ILogEntry newLog = rule.getReducedLog();
				logs.set(t, newLog);
			}
		}
	}
}
