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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Random;
import java.util.Vector;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.cosylab.logging.engine.DataNode;
import com.cosylab.logging.engine.VectorNodeList;

/**
 * This is the container class for generic Log Entries.
 * Attributes, messages and Data(s) are obtained via public
 * variables. Properties, used for management of LogEntries
 * can be obtained via accessor and mutator methods. This 
 * class is used when parsing nodes. The class is used when 
 * a file is read or a node is received by the push consumer.
 * <p>
 * Note on reuse of equal Strings, Integers etc:
 * The XML parser currently produces separate instances of the same Strings, 
 * so that a lot of memory would be used up unnecessarily.
 * This class reuses String objects for its field values, 
 * but does not exchange the String instances that are referenced by the <code>log Node</code>.
 * That current design will only yield a memory advantage if the <code>LogEntryXML</code>
 * instance is not kept permanently in the application, but instead its fields are read
 * and the values transfered (without the DOM Node) to an object of another class. 
 * Ale: please check if there are other Srings that could be reused based on <code>stringPool</code>,
 * and if we should use also a pool for Integers, at least for log levels since they are always the same few. 
 */
public final class LogEntryXML implements ILogEntry
{
	private static final HashMap<String, String> stringPool = new HashMap<String, String>(1000);
    
	private VectorNodeList datas = null;
	public VectorNodeList complexLogEntryMessage = null;

	private final Object[] fields = new Object[NUMBER_OF_FIELDS];
	

	private Node log = null;
	private boolean isLogEntrySimple = true;

	// Should be set through FIELD_LOGMESSAGE
	// public String simpleLogEntryMessage = null;

	/**
	 * This costructor is used only for testing purposes.
	 * It is called by com.cosylab.logging.test.LogEntryTest
	 * @args 
	 */
	public LogEntryXML(
		String d,
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
        String srcObject)
	{
		// set whatever you want here (depending of the test you are performing);

		setField(FIELD_TIMESTAMP, d);
		setField(FIELD_ENTRYTYPE, new Integer(entrytype));
		setField(FIELD_FILE, file);
		setField(FIELD_LINE, new Integer(line));
		setField(FIELD_ROUTINE, routine);
		setField(FIELD_HOST, host);
		setField(FIELD_PROCESS, process);
		setField(FIELD_CONTEXT, context);
		setField(FIELD_THREAD, thread);
		setField(FIELD_LOGID, logid);
		setField(FIELD_PRIORITY, new Integer(priority));
		setField(FIELD_URI, uri);
		setField(FIELD_STACKID, stackid);
		setField(FIELD_STACKLEVEL, new Integer(stacklevel));
		setField(FIELD_LOGMESSAGE, logmessage);
        setField(FIELD_SOURCEOBJECT,srcObject);
	}

	public LogEntryXML(String stackId, int stackLevel) throws DOMException
	{
		setField(FIELD_STACKID, stackId);
		setField(FIELD_STACKLEVEL, new Integer(stackLevel));
	}
    
	/**
	 * This costructor is used only for testing purposes.
	 * It is called by com.cosylab.logging.engine.simulator.simulatorRemoteAccess
	 * It generates a random LogEntryXML
	 */
	// public LogEntryXML(Random random) {
	public LogEntryXML(Random random)
	{
		// set whatever you want here (depending of the test you are performing);

		setField(FIELD_TIMESTAMP, new Date());
		setField(FIELD_ENTRYTYPE, new Integer((short) random.nextInt(LogTypeHelper.getNumberOfTypes())));
		if (random.nextInt(10) < 3)
		{
			setField(FIELD_LINE, new Integer(random.nextInt(100)));
		}

		if (random.nextInt(2) == 0)
			setField(FIELD_STACKID, "Terminator");
		else
			setField(FIELD_STACKID, "Exterminator");
	}
    
	/**
	 * This constructor is called by the DOMParser.
	 * @param log org.w3c.Node 
	 */
	public LogEntryXML(Node log) throws DOMException
	{
		initialize(log);
	}
    
	public static LogEntryXML generateRandomLog(Random random)
	{
		return new LogEntryXML(random);
	}
	
	/**
	 * Checks if a String equal to <code>s</code> is already in the string pool.
	 * If so, it will be returned for reuse, otherwise it will be added and returned.
	 *  
	 * @param s a String
	 * @return the same or a different Sting object, which is equal to <code>s</code>.
	 */
	private static String fromStringPool(String s) {
		if (s == null) {
			return null;
		}
		
		String equalString = stringPool.get(s);
		if (equalString != null) {
			return equalString;
		}
		else {
			stringPool.put(s, s);
			return s;
		}
	}
	
	
	/**
	 * Check if the log entry has datas
	 * 
	 * @return ture datas is not null
	 */
	public boolean hasDatas() {
		return datas!=null;
	}
    
	/**
	 * Data Nodes are returned as a org.w3c.dom.NodeList.
	 * If data(s) are not present, null is returned.
	 */
	public NodeList getDatas()
	{
		return datas;
	}

	public Node getNode()
	{
		return log;
	}

	/**
	 * Returns the type of this Object as String.
	 * "Undeclared" id type is not specified.
	 * @return String
	 */
	public String getEntryTypeAsString()
	{
		return LogTypeHelper.getLogTypeDescription(this);
	}

    
	/**
	 * Returns a specific field according to field name constants.
	 * Creation date: (11/21/2001 18:16:03)
	 * @return java.lang.Object
	 * @param fieldIndex int index of the field to return
	 */
	public Object getField(int fieldIndex)
	{
		return (isValidFieldIndex(fieldIndex) ? fields[fieldIndex] : null);
	}
    
	/**
	 * Return the class of a specific field 
	 * @return java.lang.Class
	 * @param fieldIndex int
	 */
	public static Class getFieldClass(int fieldIndex)
	{
		return (isValidFieldIndex(fieldIndex) ? fieldClasses[fieldIndex] : void.class);
	}
    
	/**
	 * Returns textual description of the field.
	 * Creation date: (11/21/2001 20:09:52)
	 * @return java.lang.String
	 * @param fieldIndex int
	 */
	public static String getFieldDescription(int fieldIndex)
	{
		return (isValidFieldIndex(fieldIndex) ? fieldNames[fieldIndex] : "");
	}
    
	private void initAttributes(Node log) throws DOMException
	{
		//public void initAttributes(Node log) throws DOMException {
		NamedNodeMap nnm = log.getAttributes();
		Node attr;

		attr = nnm.getNamedItem(tagAttributes[FIELD_TIMESTAMP]);
		if (attr == null)
			throw new DOMException(
				DOMException.NOT_FOUND_ERR,
				"TimeStamp attribute is missing in " + getField(FIELD_ENTRYTYPE));
		SimpleDateFormat df = new SimpleDateFormat(TIME_FORMAT);

        // The time stamp is required!
		try
		{
			setField(FIELD_TIMESTAMP, df.parse(attr.getNodeValue()));
		}
		catch (ParseException pe)
		{
			throw new DOMException(DOMException.SYNTAX_ERR, "Error while parsing TimeStamp: " + pe);
		}

        // Search for the others fields (they are not required so nothing will
        // happen is some of them are missing)
		attr = nnm.getNamedItem(tagAttributes[FIELD_FILE]);
		if (attr != null)
			setField(FIELD_FILE, fromStringPool(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_LINE]);
		if (attr != null)
			setField(FIELD_LINE, new Integer(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_ROUTINE]);
		if (attr != null)
			setField(FIELD_ROUTINE, fromStringPool(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_HOST]);
		if (attr != null)
			setField(FIELD_HOST, fromStringPool(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_PROCESS]);
		if (attr != null)
			setField(FIELD_PROCESS, fromStringPool(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_CONTEXT]);
		if (attr != null)
			setField(FIELD_CONTEXT, fromStringPool(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_THREAD]);
		if (attr != null)
			setField(FIELD_THREAD, fromStringPool(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_STACKID]);
		if (attr != null)
			setField(FIELD_STACKID, fromStringPool(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_STACKLEVEL]);
		if (attr != null)
			setField(FIELD_STACKLEVEL, new Integer(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_LOGID]);
		if (attr != null)
			setField(FIELD_LOGID, fromStringPool(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_PRIORITY]);
		if (attr != null)
			setField(FIELD_PRIORITY, new Integer(attr.getNodeValue()));

		attr = nnm.getNamedItem(tagAttributes[FIELD_URI]);
		if (attr != null)
			setField(FIELD_URI, fromStringPool(attr.getNodeValue()));
        
        attr = nnm.getNamedItem(tagAttributes[FIELD_SOURCEOBJECT]);
        if (attr != null) {
            setField(FIELD_SOURCEOBJECT, fromStringPool(attr.getNodeValue()));
        }
	}
    
	private void initBody(Node log) throws DOMException
	{
		NodeList list = log.getChildNodes();
		for (int i = 0; i < list.getLength(); i++)
		{
			Node node = list.item(i);
			boolean isData = node.getNodeName().equals("Data");
			short type = node.getNodeType();
			if (isData)
			{
				DataNode dataNode = new DataNode(node);
				if (datas == null) {
					datas = new VectorNodeList(1, 2);
				}
				datas.add(dataNode);
			}
			else
			{
				if (isLogEntrySimple) {
					isLogEntrySimple = !node.hasChildNodes();
				}
				if (isLogEntrySimple)
				{
					// simpleLogEntryMessage = node.getNodeValue();
					setField(FIELD_LOGMESSAGE, fromStringPool(node.getNodeValue()));
				}
				else
				{
					if (complexLogEntryMessage == null)
						complexLogEntryMessage = new VectorNodeList(1, 2);
					complexLogEntryMessage.add(node);
				}
			}
		}
	}
    
	private void initialize(Node log) throws DOMException
	{
		initLogEntryType(log);
		initAttributes(log);
		initBody(log);
		this.log = log;
	}
    
	private void initLogEntryType(Node log) throws DOMException
	{
		String logEntryType = log.getNodeName();
		if (logEntryType == null)
			throw new DOMException(DOMException.SYNTAX_ERR, "logEntryType is null.");
		
		// todo: reuse Integer objects
        setField(FIELD_ENTRYTYPE,LogTypeHelper.parseLogTypeDescription(logEntryType));

        if (getField(FIELD_ENTRYTYPE) == null)
			throw new DOMException(DOMException.NOT_FOUND_ERR, "Unknown logEntryType: " + logEntryType);
	}
    
	/**
	 * This boolean tag specifies whether LogEntryMessage is simple or not:
	 * <UL><LI><code>logEntryMessageSimple = true</code>: Log message is a single <code>String</code>. It is obtained via
	 * <code> String logEntry.simpleLogEntryMessage</code>. Value is null if message is not present.</LI>
	 * <LI><code>logEntryMessageSimple = false</code>: Log message is more complex, it is an XML <code>
	 * org.w3c.dom.NodeList</code>. It is obtained via <code>org.w3c.dom.NodeList logEntry.complexLogEntryMessage</code>.
	 * Value is null if message is simple.</LI>
	 * </UL>
	 */
	public boolean isLogEntryMessageSimple()
	{
		return false;
	}
    
	/**
	 * Internal method to check if the specified field index is defined.
	 * Creation date: (11/21/2001 20:10:50)
	 * @return boolean
	 * @param fieldIndex int
	 */
	public static boolean isValidFieldIndex(int fieldIndex)
	{
		return ((fieldIndex >= 0) && (fieldIndex < NUMBER_OF_FIELDS));
	}
    
	/**
	 * Insert the method's description here.
	 * Creation date: (12/4/2001 12:21:35)
	 * @return boolean
	 * @param index int
	 */
	public final static boolean isValidLogEntryType(int index)
	{
		return ((index >= 0) && (index < 9));
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
		//public void setField(int fieldIndex, Object value) {
		if (isValidFieldIndex(fieldIndex))
		{
			fields[fieldIndex] = value;
		}
	}
	
	/**
	 * Returns a String representation of the log.
	 */
	public String toString()
	{
		StringBuffer sb = new StringBuffer("--- LogEntryXML ---\n");

		/* Attributes */
		for (int i = 0; i < NUMBER_OF_FIELDS; i++)
		{
			if (getField(i) != null)
				if (i == FIELD_ENTRYTYPE)
					sb.append(getFieldDescription(i) + ": " + getEntryTypeAsString() + "\n");
				else
					sb.append(getFieldDescription(i) + ": " + getField(i) + "\n");
		}
        
		/* Data(s) */
		if (datas != null)
			sb.append("Datas: " + datas + "\n");

		return sb.toString();
	}
	
	/**
	 * Recursively prints the struct of the node
	 * 
	 * @param logNode The node to print 
	 */
	public static void printNode(Node logNode,int depth) {
		String indent="";
		for (int i=0; i<depth; i++) indent=indent+"\t";
		System.out.print(indent+"Node name "+logNode.getNodeName());
		System.out.print(", type "+logNode.getNodeType());
		System.out.println(", val  "+logNode.getNodeValue());
		NamedNodeMap map = logNode.getAttributes();
		if (map!=null) {
			for (int i=0; i<map.getLength(); i++) {
				Node nd =map.item(i);
				System.out.print(indent+"\tAttr name "+nd.getNodeName());
				System.out.print(", type "+nd.getNodeType());
				System.out.println(", val  "+nd.getNodeValue());
			}
		}
		NodeList childs = logNode.getChildNodes();
		if (childs!=null)
		{
			for (int i=0; i<childs.getLength(); i++) {
				printNode(childs.item(i),depth+1);
			}
		}
	}
	
	/**
	 * Return the object as XML string
	 */
	public String toXMLString() {
		if (log==null) {
			throw new IllegalStateException("Node is null");
		}
		StringBuffer sb = new StringBuffer();

		String logType =getEntryTypeAsString();
		sb.append("<"+logType);
		
		for (int t=0; t<LogEntryXML.NUMBER_OF_FIELDS; t++) {
			if (t==LogEntryXML.FIELD_LOGMESSAGE || t==LogEntryXML.FIELD_ENTRYTYPE) {
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
				sb.append(" "+LogEntryXML.tagAttributes[t]+"=\""+attrValStr+"\"");
			}
		}
		
		Integer type = (Integer)getField(LogEntryXML.FIELD_ENTRYTYPE);
		if (type==LogTypeHelper.ENTRYTYPE_TRACE) {
			sb.append("/>");
		} else {
			sb.append("><![CDATA["+getField(LogEntryXML.FIELD_LOGMESSAGE).toString()+"]]>");
			sb.append(getXMLDatas());
			sb.append("</"+logType+">");
		}
		return sb.toString();
	}
	
	private StringBuffer getXMLDatas() {
		StringBuffer tempStr = new StringBuffer();
		if (datas!=null) {
			Vector<AdditionalData> temp=getAdditionalData();
			if (temp==null) {
				// No additional data
				return tempStr;
			}
			for (int t=0; t<temp.size(); t++) {
				tempStr.append("<Data Name=\""+temp.get(t).getName()+"\">");
				tempStr.append(temp.get(t).getValue()+"</Data>");
			}
		}
		return tempStr;
	}
	
	/**
	 * The vector return contains only strings and it is formed
	 * in this way:
	 *  name value name value name value...
	 * i.e. it is a plain representation of couples of values
	 *  
	 * @return a Vector of String with the key and value of each 
	 *         additional data
	 *         If the log does not contain any additional data,
	 *         returns null
	 */
	public Vector<AdditionalData> getAdditionalData() {
		Vector<AdditionalData> tempVector = null; 
		if (datas!=null) {
			if (datas.size()==0) {
				return null;
			} 
			tempVector = new Vector<AdditionalData>();
			int size = datas.size();
			for (int t=0; t<size; t++) {
				String dataName=null;
				Node dataItem = datas.item(t);
				if (dataItem.hasAttributes()) {
					org.w3c.dom.NamedNodeMap attr=dataItem.getAttributes();
					for (int at=0; at<attr.getLength(); at++) {
						org.w3c.dom.Node attrNode = attr.item(t);
						if (attrNode!=null) {
							dataName = attrNode.getTextContent().trim();
							// Cleanup
							dataName=dataName.replaceAll("<","&lt;");
							dataName=dataName.replaceAll(">","&gt;");
						} else {
							org.w3c.dom.Node temp = attr.getNamedItem("Name");
							if (temp!=null) {
								dataName=temp.getNodeValue().replaceAll("<","&lt;").replaceAll(">","&gt;").trim();
							} else {
								// The name should always be defined!
								dataName=("N/A");
							}
						}
					}
				}
				
				String dataContent = "";
				if (dataItem.hasChildNodes()) {
					Node child = dataItem.getFirstChild();
					dataContent= child.getNodeValue().trim().replaceAll("<","&lt;").replaceAll(">","&gt;");
				}
				tempVector.add(new AdditionalData(dataName,dataContent));
			}
			
		}
		return tempVector;
	}
	
	/**
	 *	Add a data node to this log:
	 *  <Data Name=name>value</Data>
	 * 
	 * @param name The name, i.e. the key of the pair
	 * @param value The value of the field
	 */
	public void addData(String name, String value) {
		Document doc = log.getOwnerDocument();
		Element ele = doc.createElement("Data");
		ele.setAttribute("Name",name);
		Text txt = doc.createTextNode(value);
		ele.appendChild(txt);
		
		if (datas==null) {
			datas = new VectorNodeList(1,1);
		}
		datas.add(ele);
	}

}
