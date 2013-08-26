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

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;

/**
 * The reduction rule for the logs having the same log message but different sources.
 * For example two logs having the very same message "Antenna in position" but sources
 * DA41 and DV01 will be reduced to a single log having the sources (DA41 and DV01 in 
 * this example) as additional data and the source replaced by the 
 * string ({@link SourceAntennaRule#reducedSource}). The message remains the same.
 *  
 * @author acaproni
 *
 */
public class SourceAntennaRule extends ReductionRule {
	
	/**
	 * The string placed in the source of the logs reducing the other entries.
	 */
	private static final String reducedSource="Several sources (see log details)";
	
	/**
	 * The name of the additional data of each source
	 */
	private static final String additionalDataName="Source";
	
	/**
	 * It is <code>true</code> if the rule is suitable for reducing logs.
	 * In this case it means that the source of the initial log contains 
	 * the name of an antenna.
	 */
	private final boolean isReducible;
	
	/**
	 * The set of the sources reduced by this rule.
	 * <P>
	 * It always include at list the source of the initial log (if it contains
	 * an antenna).
	 */
	private final Set<String> sourceObjects;
	
	/**
	 * The base source message to compare with the other log entries.
	 * <P>
	 * It is the source of the initial log with the name of the antenna
	 * replace by the place holder.
	 */
	private final String baseSourceMessage;
	
	/**
	 * The log message of the initial log.
	 */
	private final String initialLogMessage;
	
	/**
	 * The name of the antenna in the source object of the initial log
	 */
	private final String initialAntenna;
	
	/**
	 * Constructor
	 * 
	 * @param initialLog The initial log used to reduce all the other entries
	 */
	public SourceAntennaRule(ILogEntry initialLog) {
		super(initialLog);
		// Build an intermediate String as we do not want to pass a null String to the StringBuilder constructor.
		String src=initialLog.getField(LogField.SOURCEOBJECT)!=null?(String)initialLog.getField(LogField.SOURCEOBJECT):"";
		StringBuilder source = new StringBuilder(src);
		initialAntenna = Antennae.matchAndReplaceAntenna(source,placeHolder);
		isReducible=initialAntenna!=null;
		if (initialAntenna!=null) {
			baseSourceMessage=source.toString();
			sourceObjects=new HashSet<String>();
			sourceObjects.add(src);
			initialLogMessage=(String)initialLog.getField(LogField.LOGMESSAGE);
		} else {
			baseSourceMessage=null;
			sourceObjects=null;
			initialLogMessage=null;
		}
	}

	@Override
	public boolean isReducible() {
		return isReducible;
	}

	@Override
	public boolean applyRule(ILogEntry logToReduce) {
		if (!isReducible ||logToReduce==null) {
			return false;
		}
		// First check if the log messages are the same
		if (!initialLogMessage.equalsIgnoreCase((String)logToReduce.getField(LogField.LOGMESSAGE))) {
			return false;
		}
		// Now check if the sources are the same apart of the antenna name
		String src=logToReduce.getField(LogField.SOURCEOBJECT)!=null?(String)logToReduce.getField(LogField.SOURCEOBJECT):"";
		StringBuilder source = new StringBuilder(src);
		if (source.length()==0) {
			return false;
		}
		String antenna = Antennae.matchAndReplaceAntenna(source, placeHolder);
		if (antenna==null || antenna.isEmpty()) {
			return false;
		}
		if (baseSourceMessage.equalsIgnoreCase(source.toString())) {
			sourceObjects.add(src);
			return true;	
		}
		return false;
	}

	@Override
	public ILogEntry getReducedLog() {
		if (sourceObjects==null || sourceObjects.size()<=1) {
			return initialLog;
		}
		Long milliseconds=(Long)initialLog.getField(LogField.TIMESTAMP);
		Integer entrytype=((LogTypeHelper)initialLog.getField(LogField.ENTRYTYPE)).ordinal();
		String file=(String)initialLog.getField(LogField.FILE);
		Integer line=(Integer)initialLog.getField(LogField.LINE);
		String routine=(String)initialLog.getField(LogField.ROUTINE);
		String host=(String)initialLog.getField(LogField.HOST);
		String process=(String)initialLog.getField(LogField.PROCESS);
		String context=(String)initialLog.getField(LogField.CONTEXT);
		String thread=(String)initialLog.getField(LogField.THREAD);
		String logid=(String)initialLog.getField(LogField.LOGID);
		Integer priority=(Integer)initialLog.getField(LogField.PRIORITY);
		String uri=(String)initialLog.getField(LogField.URI);
		String stackid=(String)initialLog.getField(LogField.STACKID);
		Integer stacklevel=(Integer)initialLog.getField(LogField.STACKLEVEL);
		String logmessage=initialLogMessage;
        String srcObject=reducedSource;
        String audience=(String)initialLog.getField(LogField.AUDIENCE);
        String array=(String)initialLog.getField(LogField.ARRAY);
        String antenna=(String)initialLog.getField(LogField.ANTENNA);
        Vector<AdditionalData> addDatas=initialLog.getAdditionalData();
        if (addDatas==null) {
        	addDatas=new Vector<ILogEntry.AdditionalData>();
        }
        for (String ant: sourceObjects) {
        	AdditionalData ad = new AdditionalData(additionalDataName, ant);
        	addDatas.add(ad);
        }
		
		LogEntry ret = new LogEntry(
				milliseconds, 
				entrytype, 
				file, 
				line, 
				routine, 
				host, 
				process, 
				context, 
				thread, 
				logid, 
				priority, 
				uri, 
				stackid, 
				stacklevel, 
				logmessage, 
				srcObject, 
				audience, 
				array, 
				antenna, 
				addDatas);
		
		return ret;
	}

	@Override
	public boolean isReducingLogs() {
		return sourceObjects.size()>1;
	}

}
