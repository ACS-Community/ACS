/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
package com.cosylab.logging.engine.ACS;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Vector;

import com.cosylab.logging.engine.LogEngineException;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;
import com.cosylab.logging.engine.log.LogField;

import alma.ACSLoggingLog.LogBinaryRecord;
import alma.ACSLoggingLog.NameValue;
import alma.acs.util.IsoDateFormat;

/**
 * A collection of utils methods 
 * 
 * @author acaproni
 *
 */
public class CacheUtils {
	
	// The simple date format used to write and read dates from a string
	public static final SimpleDateFormat dateFormat = new IsoDateFormat();
	
	// The separator for the field of the logs in the file
	public static final char SEPARATOR_CHAR = (char)0;
	public static final String SEPARATOR = ""+CacheUtils.SEPARATOR_CHAR;
	
	private static StringBuilder sb=new StringBuilder();
	
	/**
	 * Build a log out of its string representation
	 * 
	 * @param str The string representing a log
	 * @return The log
	 */
	public synchronized static ILogEntry fromCacheString(String str) throws LogEngineException {
		String[] strs = str.split(SEPARATOR);
		long millis = 0;
		try {
			synchronized (dateFormat) {
				millis=dateFormat.parse(strs[0]).getTime();
			}
		} catch (ParseException e) {
			System.err.println("Error parsing the date: "+strs[0]);
			throw new LogEngineException(e);
		}
		Integer entrytype = new Integer(strs[1]);
		
		String srcObject = null;
		if (strs.length>2) {
			srcObject=strs[2];
		}
		String fileNM = null;
		if (strs.length>3) {
			fileNM=strs[3];
		}
		Integer line = null;
		if (strs.length>4 && strs[4].length()!=0) {
			line =new Integer(strs[4]);
		}
		String routine = null;
		if (strs.length>5) {
			routine=strs[5];
		}
		String host = null;
		if (strs.length>6) {
			host=strs[6];
		}
		String process = null;
		if (strs.length>7) {
			process=strs[7];
		}
		String context = null;
		if (strs.length>8) {
			context=strs[8];
		}
		String thread = null;
		if (strs.length>9) {
			thread=strs[9];
		}
		String logid = null;
		if (strs.length>10) {
			logid=strs[10];
		}
		Integer priority = null;
		if (strs.length>11 && strs[11].length()>0) {
			priority=new Integer(strs[11]);
		}
		String uri = null;
		if (strs.length>12) {
			uri=strs[12];
		}
		String stackid = null;
		if (strs.length>13) {
			stackid=strs[13]; 
		}
		Integer stacklevel = null;
		if (strs.length>14 && strs[14].length()>0) {
			Integer.parseInt(strs[14]);
		}
		String logmessage = null;
		if (strs.length>15) {
			logmessage=strs[15];
		}
		String audience = null;
		if (strs.length>16) {
			audience=strs[16];
		}
		String array = null;
		if (strs.length>17) {
			array=strs[17];
		}
		String antenna = null;
		if (strs.length>18) {
			antenna=strs[18];
		}
        
        Vector<ILogEntry.AdditionalData> addDatas = null;
        if (strs.length>LogField.values().length) {
        	addDatas = new Vector<ILogEntry.AdditionalData>();
        	for (int t=LogField.values().length; t<strs.length; t+=2) {
        		addDatas.add(new AdditionalData(strs[t],strs[t+1]));
        	}
        }
        return new LogEntry(
        		millis,
        		entrytype,
        		fileNM,
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
	}
	
	/**
	 * Transform the log in a string of zero separated values
	 * It is very similato what is used in the FileCache
	 * 
	 * @param log The log to get the string representation
	 * @return A string representation of the log
	 */
	public static String toCacheString(LogBinaryRecord log) {
		synchronized (sb) {
			sb.delete(0, sb.length());
			sb.append(log.TimeStamp);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.type);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.SourceObject);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.File);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Line);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Routine);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Host);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Process);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.LogContext);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Thread);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.LogId);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Priority);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Uri);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.StackId);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.StackLevel);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.MsgData);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Audience);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Array);
			sb.append(CacheUtils.SEPARATOR_CHAR);
			sb.append(log.Antenna);
			sb.append(CacheUtils.SEPARATOR_CHAR);

			NameValue[] attrs = log.attributes;
			NameValue[] vals = log.log_data;
			
			if (vals==null||attrs==null) {
				return sb.toString();
			}

			//int max=Math.max(attrs.length, vals.length);
			int min = Math.min(attrs.length, vals.length);
			for (int t = 0; t < min; t++) {
				if (attrs[t] != null) {
					sb.append(attrs[t].name);
					sb.append(CacheUtils.SEPARATOR_CHAR);
					sb.append(attrs[t].value);
					sb.append(CacheUtils.SEPARATOR_CHAR);
				}
				if (vals[t] != null) {
					sb.append(vals[t].name);
					sb.append(CacheUtils.SEPARATOR_CHAR);
					sb.append(vals[t].value);
					sb.append(CacheUtils.SEPARATOR_CHAR);
				}
			}
			if (attrs.length > vals.length) {
				for (int t = min; t < attrs.length; t++) {
					if (attrs[t] != null) {
						sb.append(attrs[t].name);
						sb.append(CacheUtils.SEPARATOR_CHAR);
						sb.append(attrs[t].value);
						sb.append(CacheUtils.SEPARATOR_CHAR);
					}
				}
			} else if (attrs.length < vals.length) {
				for (int t = min; t < vals.length; t++) {
					if (vals[t] != null) {
						sb.append(vals[t].name);
						sb.append(CacheUtils.SEPARATOR_CHAR);
						sb.append(vals[t].value);
						sb.append(CacheUtils.SEPARATOR_CHAR);
					}
				}
			}
			return sb.toString();
		}
	}
}
