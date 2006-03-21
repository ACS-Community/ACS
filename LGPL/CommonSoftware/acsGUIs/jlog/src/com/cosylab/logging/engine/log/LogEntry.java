package com.cosylab.logging.engine.log;

import java.util.Date;
import java.util.Vector;

import java.text.SimpleDateFormat;
import java.text.FieldPosition;

import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * Objects of this class holds implements the ILogEntry interface
 * representing a generic log record. This class does not contain
 * any reference to XML parsers
 * 
 * @author acaproni
 */
public class LogEntry implements ILogEntry {
	
	/**
	 * Each additional data is a couple <name,value>
	 * The list of additional datas (see below) stores
	 * objects of this class
	 * 
	 * @author acaproni
	 *
	 */
	public class AdditionalData {
		private String name;
		private String value;
		
		public AdditionalData(String name, String value) {
			this.name=name;
			this.value=value;
		}
		
		/**
		 * Getter method 
		 * @return The value of this data
		 */
		public String getValue() {
			return this.value;
		}
		
		/**
		 * Getter method 
		 * @return The name of this data
		 */
		public String getName() {
			return this.name;
		}
		
	}
	
	private Date date;
	private Integer type;
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
	 * @param addDatas The additional data as a Vector<String>
	 *                 The Vector contains in the even position the name 
	 *                 and in the odd the value. It can be null.
	 *                 @see LogEntryXML.getAdditionalData
	 * 
	 * @see ILogEntry
	 */
	public LogEntry(
			long milliseconds,
			int entrytype,
			String file,
			int line,
			String routine,
			String host,
			String process,
			String context,
			String thread,
			String logid,
			int priority,
			String uri,
			String stackid,
			int stacklevel,
			String logmessage,
	        String srcObject,
	        Vector<String> addDatas) {
		this.date=new Date(milliseconds);
		this.type=new Integer(entrytype);
		this.file=file;
		this.line=new Integer(line);
		this.routine=routine;
		this.host=host;
		this.process=process;
		this.context=context;
		this.thread=thread;
		this.logId=logid;
		this.priority=new Integer(priority);
		this.uri=uri;
		this.stackId=stackid;
		this.stackLevel=new Integer(stacklevel);
		this.logMessage=logmessage;
		this.sourceObject=srcObject;
		// Add the additional datas, if any
		if (addDatas!=null) {
			int size = addDatas.size();
			if (size % 2 !=0 ) {
				throw new IllegalArgumentException("Additional data vector malformed");
			}
			for (int t=0; t<size; t+=2) {
				addData(addDatas.get(t),addDatas.get(t+1));
			}
		}
	}
	
	/**
	 * Build a LogEntry from a LogEntryXML
	 * 
	 * @param logXML The log entry
	 * @see LogEntryXML
	 */
	public LogEntry(LogEntryXML logXML) {
		for (int fieldIndex=0; fieldIndex<NUMBER_OF_FIELDS; fieldIndex++) {
			setField(fieldIndex,logXML.getField(fieldIndex));
		}
		// Add the additional datas, if any
		Vector<String> addDatas=logXML.getAdditionalData();
		if (addDatas!=null) {
			int size = addDatas.size();
			if (size % 2 !=0 ) {
				throw new IllegalArgumentException("Additional data vector malformed");
			}
			for (int t=0; t<size; t+=2) {
				addData(addDatas.get(t),addDatas.get(t+1));
			}
		}
	}

	/**
	 * @return an XML string representing this log
	 */
	public String toXMLString() {
		StringBuffer sb = new StringBuffer();

		String logType =LogTypeHelper.getLogTypeDescription(type);
		sb.append("<"+logType);
		
		for (int t=0; t<NUMBER_OF_FIELDS; t++) {
			if (t==FIELD_LOGMESSAGE || t==FIELD_ENTRYTYPE) {
				continue;
			}
			Object attrValue = getField(t);
			if (attrValue!=null) {
				if (Date.class.isInstance(attrValue)) {
					SimpleDateFormat df = new SimpleDateFormat(TIME_FORMAT);
					Date dt = (Date)attrValue;
					StringBuffer dateSB = new StringBuffer();
					java.text.FieldPosition pos = new java.text.FieldPosition(0);
					df.format(dt,dateSB,pos);
					attrValue=dateSB.toString();
				}
				String attrValStr = attrValue.toString();
				attrValStr=attrValStr.replaceAll("<","&lt;");
				attrValStr=attrValStr.replaceAll(">","&gt;");
				sb.append(" "+tagAttributes[t]+"=\""+attrValStr+"\"");
			}
		}
		
		if (type==LogTypeHelper.ENTRYTYPE_TRACE) {
			sb.append("/>");
		} else {
			sb.append("><![CDATA["+logMessage+"]]>");
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
	private StringBuffer getXMLDatas() {
		StringBuffer tempStr = new StringBuffer();
		if (additionalData!=null) {
			int size = additionalData.size();
			for (int t=0; t<size; t++) {
				AdditionalData temp = additionalData.get(t);
				String name = temp.getName();
				String value= temp.getValue();
				// Cleanup
				name=name.replaceAll("<","&lt;").replaceAll(">","&gt;").trim();
				value=value.replaceAll("<","&lt;").replaceAll(">","&gt;").trim();
				tempStr.append("<Data Name=\""+name+"\">");
				tempStr.append(value);
				tempStr.append("</Data>");
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
		} else {
			return additionalData.size()>0;
		}
	}

	/**
	 * @param fieldIndex The index of the field of the object to get
	 * @return The object of the given index
	 */
	public Object getField(int fieldIndex) {
		switch (fieldIndex) {
			case FIELD_TIMESTAMP: {
				return date;
			}
			case FIELD_ENTRYTYPE: {
				return type;
			}
			case FIELD_SOURCEOBJECT: {
				return sourceObject;
			}
			case FIELD_FILE: {
				return file;
			}
			case FIELD_LINE: {
				return line;
			}
			case FIELD_ROUTINE: {
				return routine;
			}
			case FIELD_HOST: {
				return host;
			}
			case FIELD_PROCESS: {
				return process;
			}
			case FIELD_CONTEXT: {
				return context;
			}
			case FIELD_THREAD: {
				return thread;
			}
			case FIELD_LOGID: {
				return logId;
			}
			case FIELD_PRIORITY: {
				return priority;
			}
			case FIELD_URI: {
				return uri;
			}
			case FIELD_STACKID: {
				return stackId;
			}
			case FIELD_STACKLEVEL: {
				return stackLevel;
			}
			case FIELD_LOGMESSAGE: {
				return logMessage;
			}
			default: {
				throw new IndexOutOfBoundsException("Illegal index "+fieldIndex);
			}
		}
	}
	
	/**
	 * Sets the specified field. This method is protected since the fields are not
	 * to be modified. The only time this is called is during initialization.
	 * Creation date: (11/21/2001 18:35:10)
	 * @param fieldIndex int index of the field
	 * @param value java.lang.Object value to set
	 */
	protected void setField(int fieldIndex, Object value)
	{
		switch (fieldIndex) {
			case FIELD_TIMESTAMP: {
				date=(Date)value;
				return;
			}
			case FIELD_ENTRYTYPE: {
				type=(Integer)value;
				return;
			}
			case FIELD_SOURCEOBJECT: {
				sourceObject=(String)value;
				return;
			}
			case FIELD_FILE: {
				file=(String)value;
				return;
			}
			case FIELD_LINE: {
				line=(Integer)value;
				return;
			}
			case FIELD_ROUTINE: {
				routine=(String)value;
				return;
			}
			case FIELD_HOST: {
				host=(String)value;
				return;
			}
			case FIELD_PROCESS: {
				process=(String)value;
				return;
			}
			case FIELD_CONTEXT: {
				context=(String)value;
				return;
			}
			case FIELD_THREAD: {
				thread=(String)value;
				return;
			}
			case FIELD_LOGID: {
				logId=(String)value;
				return;
			}
			case FIELD_PRIORITY: {
				priority=(Integer)value;
				return;
			}
			case FIELD_URI: {
				uri=(String)value;
				return;
			}
			case FIELD_STACKID: {
				stackId=(String)value;
				return;
			}
			case FIELD_STACKLEVEL: {
				stackLevel=(Integer)value;
				return;
			}
			case FIELD_LOGMESSAGE: {
				logMessage=(String)value;
				return;
			}
			default: {
				throw new IndexOutOfBoundsException("Illegal index "+fieldIndex);
			}
		}

	}
	
	public void addData(String name, String value) {
		if (name==null || value==null) {
			throw new IllegalArgumentException("Parameter can't be null");
		}
		if (name.length()==0 || value.length()==0) {
			throw new IllegalArgumentException("Parameters can't be empty string");
		}
		
		if (additionalData ==null) {
			additionalData = new Vector<LogEntry.AdditionalData>();
		}
		additionalData.add(new AdditionalData(name,value));
	}
	
	/**
	 * Return a string representation of this entry
	 */
	public String toString() {
		StringBuffer sb = new StringBuffer("--- LogEntry ---\n");

		/* Attributes */
		for (int i = 0; i < NUMBER_OF_FIELDS; i++)
		{
			if (getField(i) != null) {
				sb.append(fieldNames[i] + ": ");
				if (i == FIELD_ENTRYTYPE) {
					sb.append(LogTypeHelper.getLogTypeDescription(type));
				} else if(i==FIELD_TIMESTAMP) {
					SimpleDateFormat df = new SimpleDateFormat(TIME_FORMAT);
					FieldPosition pos = new FieldPosition(0);
					df.format(date,sb,pos);
				} else {
					sb.append(getField(i));
				}
				sb.append("\n");
			}
		}
        
		/* Data(s) */
		if (additionalData != null) {
			sb.append("Datas: \n");
			for (int t=0; t<additionalData.size(); t++) {
				AdditionalData temp = additionalData.get(t);
				sb.append("\t"+temp.getName()+" : "+temp.getValue());
			}
		}

		return sb.toString();
	}

}
