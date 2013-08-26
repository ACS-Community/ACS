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
package com.cosylab.logging.engine.log;

import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import alma.acs.util.IsoDateFormat;

/**
 * Objects of this class holds implements the ILogEntry interface
 * representing a generic log record. This class does not contain
 * any reference to XML parsers
 * 
 * @author acaproni
 */
public class LogEntry implements ILogEntry {
	
	private Long date;
	private LogTypeHelper type;
	private String file;
	private Integer line;
	private String routine;
	private String host;
	private String process;
	private String context;
	private String thread;
	private String logId;
	private Integer priority;
	private String uri, stackId;
	private Integer stackLevel;
	private String logMessage;
	private String sourceObject;
	private String audience;
	private String array;
	private String antenna;
	
	// The additional data
	private  Vector<LogEntry.AdditionalData> additionalData = null;

	/**
	 * Builds a LogEntry object from the value of its fields
	 * All the fields are defined in the ILogEntry interface.
	 * 
	 * 
	 * @param milliseconds The date
	 * @param entrytype The type
	 * @param file ...
	 * @param line ...
	 * @param routine ...
	 * @param host ...
	 * @param process ...
	 * @param context ...
	 * @param thread ...
	 * @param logid ...
	 * @param priority ...
	 * @param uri ...
	 * @param stackid ...
	 * @param stacklevel ...
	 * @param logmessage ...
	 * @param srcObject ...
	 * @param audience ...
	 * @param array ...
	 * @param antenna ...
	 * @param addDatas The additional data as a Vector<String>
	 *                 The Vector contains in the even position the name 
	 *                 and in the odd the value. It can be null.
	 *                 @see LogEntryXML.getAdditionalData
	 * 
	 * @see ILogEntry
	 */
	public LogEntry(
			Long milliseconds,
			Integer entrytype,
			String file,
			Integer line,
			String routine,
			String host,
			String process,
			String context,
			String thread,
			String logid,
			Integer priority,
			String uri,
			String stackid,
			Integer stacklevel,
			String logmessage,
	        String srcObject,
	        String audience,
	        String array,
	        String antenna,
	        Vector<AdditionalData> addDatas) {
		if(null != milliseconds) {
			this.date=milliseconds;
		}
		else {
			this.date = null;
		}
		this.type=LogTypeHelper.values()[entrytype];
		this.file=file;
		this.line=line;
		this.routine=routine;
		this.host=host;
		this.process=process;
		this.context=context;
		this.thread=thread;
		this.logId=logid;
		this.priority=priority;
		this.uri=uri;
		this.stackId=stackid;
		this.stackLevel=stacklevel;
		this.logMessage=logmessage;
		this.sourceObject=srcObject;
		this.audience=audience;
		this.array=array;
		this.antenna=antenna;
		// Add the additional datas, if any
		if (addDatas!=null) {
			additionalData = new Vector<LogEntry.AdditionalData>();
			additionalData.addAll(addDatas);
		}
	}
	
	/**
	 * Build a LogEntry from a LogEntryXML
	 * 
	 * @param logXML The log entry
	 * @see LogEntryXML
	 */
	public LogEntry(LogEntryXML logXML) {
		for (LogField f: LogField.values()) {
			setField(f,logXML.getField(f));
		}
		// Add the additional datas, if any
		Vector<AdditionalData> addDatas=logXML.getAdditionalData();
		if (addDatas!=null) {
			additionalData = new Vector<LogEntry.AdditionalData>();
			additionalData.addAll(addDatas);
		}
	}

	/**
	 * @return an XML string representing this log
	 */
	public String toXMLString() {
		StringBuilder sb = new StringBuilder();

		String logType =type.logEntryType;
		sb.append("<"+logType);
		
		for (LogField t: LogField.values()) {
			if (t==LogField.LOGMESSAGE || t==LogField.ENTRYTYPE) {
				continue;
			}
			Object attrValue = getField(t);
			if (attrValue!=null) {
				if (t==LogField.TIMESTAMP) {
					SimpleDateFormat df = new IsoDateFormat();
					Date dt = new Date((Long)attrValue);
					StringBuffer dateSB = new StringBuffer();
					java.text.FieldPosition pos = new java.text.FieldPosition(0);
					df.format(dt,dateSB,pos);
					attrValue=dateSB.toString();
				}
				String attrValStr = attrValue.toString();
				attrValStr=attrValStr.replaceAll("<","&lt;");
				attrValStr=attrValStr.replaceAll(">","&gt;");
				sb.append(" "+t.getTagAttribute()+"=\""+attrValStr+"\"");
			}
		}
		
		if (type==LogTypeHelper.TRACE && !hasDatas() && logMessage!=null && logMessage.trim().isEmpty()) {
			sb.append("/>");
		} else {
			sb.append(">");
			if (logMessage!=null) {
				sb.append("<![CDATA["+logMessage+"]]>");
			}
			if (hasDatas()) {
				sb.append(getXMLDatas());
			}
			sb.append("</"+logType+">");
		}
		return sb.toString();
	}
	
	/**
	 * 
	 * @return The XML representation of the additional data
	 */
	private StringBuilder getXMLDatas() {
		StringBuilder tempStr = new StringBuilder();
		if (additionalData!=null) {
			int size = additionalData.size();
			for (int t=0; t<size; t++) {
				AdditionalData temp = additionalData.get(t);
				// Cleanup
				String tempName=temp.name.replaceAll("<","&lt;").replaceAll(">","&gt;").trim();
				String tempValue=temp.value.replaceAll("<","&lt;").replaceAll(">","&gt;").trim();
				tempStr.append("<Data Name=\""+tempName+"\"><![CDATA[");
				tempStr.append(tempValue);
				tempStr.append("]]></Data>");
			}
		}
		return tempStr;
	}

	/**
	 * @return True if the log has additional data
	 */
	public boolean hasDatas() {
		if (additionalData==null) {
			return false;
		} 
		return additionalData.size()>0;
	}

	/**
	 * @param field The field to get
	 * @return The object of the given index
	 */
	public Object getField(LogField field) {
		switch (field) {
			case TIMESTAMP: {
				return date;
			}
			case ENTRYTYPE: {
				return type;
			}
			case SOURCEOBJECT: {
				return sourceObject;
			}
			case AUDIENCE: {
				return audience;
			}
			case ARRAY: {
				return array;
			}
			case ANTENNA: {
				return antenna;
			}
			case FILE: {
				return file;
			}
			case LINE: {
				return line;
			}
			case ROUTINE: {
				return routine;
			}
			case HOST: {
				return host;
			}
			case PROCESS: {
				return process;
			}
			case CONTEXT: {
				return context;
			}
			case THREAD: {
				return thread;
			}
			case LOGID: {
				return logId;
			}
			case PRIORITY: {
				return priority;
			}
			case URI: {
				return uri;
			}
			case STACKID: {
				return stackId;
			}
			case STACKLEVEL: {
				return stackLevel;
			}
			case LOGMESSAGE: {
				return logMessage;
			}
			default: {
				throw new IllegalArgumentException("Unsupported field "+field);
			}
		}
	}
	
	/**
	 * Sets the specified field. This method is protected since the fields are not
	 * to be modified. The only time this is called is during initialization.
	 * Creation date: (11/21/2001 18:35:10)
	 * @param field The field to set
	 * @param value java.lang.Object value to set
	 */
	protected void setField(LogField field, Object value)
	{
		switch (field) {
			case TIMESTAMP: {
				date=(Long)value;
				return;
			}
			case ENTRYTYPE: {
				type=LogTypeHelper.values()[(Integer)value];
				return;
			}
			case SOURCEOBJECT: {
				sourceObject=(String)value;
				return;
			}
			case AUDIENCE: {
				audience=(String)value;
				return;
			}
			case ARRAY: {
				array=(String)value;
				return;
			}
			case ANTENNA: {
				antenna=(String)value;
				return;
			}
			case FILE: {
				file=(String)value;
				return;
			}
			case LINE: {
				line=(Integer)value;
				return;
			}
			case ROUTINE: {
				routine=(String)value;
				return;
			}
			case HOST: {
				host=(String)value;
				return;
			}
			case PROCESS: {
				process=(String)value;
				return;
			}
			case CONTEXT: {
				context=(String)value;
				return;
			}
			case THREAD: {
				thread=(String)value;
				return;
			}
			case LOGID: {
				logId=(String)value;
				return;
			}
			case PRIORITY: {
				priority=(Integer)value;
				return;
			}
			case URI: {
				uri=(String)value;
				return;
			}
			case STACKID: {
				stackId=(String)value;
				return;
			}
			case STACKLEVEL: {
				stackLevel=(Integer)value;
				return;
			}
			case LOGMESSAGE: {
				logMessage=(String)value;
				return;
			}
			default: {
				throw new IllegalArgumentException("Unsupported field "+field);
			}
		}

	}
	
	public void addData(String name, String value) {
		if (name==null || value==null) {
			throw new IllegalArgumentException("Parameter can't be null");
		}
		if (name.isEmpty() || value.isEmpty()) {
			throw new IllegalArgumentException("Parameters can't be empty");
		}
		
		if (additionalData ==null) {
			additionalData = new Vector<LogEntry.AdditionalData>();
		}
		synchronized (additionalData) {
			AdditionalData data =new AdditionalData(name,value);
			if (!additionalData.contains(data)) {
				additionalData.add(data);
			}
		}
		
	}
	
	/**
	 * Return a string representation of this entry
	 */
	public String toString() {
		StringBuffer sb = new StringBuffer("--- LogEntry ---\n");

		/* Attributes */
		for (LogField f: LogField.values())
		{
			if (getField(f) != null) {
				sb.append(f.getName() + ": ");
				if (f == LogField.ENTRYTYPE) {
					sb.append(type.logEntryType);
				} else if(f==LogField.TIMESTAMP) {
					SimpleDateFormat df = new IsoDateFormat();
					FieldPosition pos = new FieldPosition(0);
					df.format(date,sb,pos);
				} else {
					sb.append(getField(f));
				}
				sb.append("\n");
			}
		}
        
		/* Data(s) */
		if (additionalData != null) {
			sb.append("Datas: \n");
			for (int t=0; t<additionalData.size(); t++) {
				AdditionalData temp = additionalData.get(t);
				sb.append("\t"+temp.name+" : "+temp.value);
			}
		}

		return sb.toString();
	}
	
	/**
	 * @see ILogEntry
	 */
	public Vector<AdditionalData> getAdditionalData() {
		return additionalData;
	}
	
	/**
	 * @see ILogEntry
	 */
	public LogTypeHelper getType() {
		return this.type;
	}

}
