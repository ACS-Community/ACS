/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */

/** 
 * @author  acaproni   
 * @version $Id: CategorySubscriber.java,v 1.6 2011/03/25 13:57:08 acaproni Exp $
 * @since    
 */

package alma.acs.lasercore.test.stress.category;

import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.lasercore.test.stress.CategoryClient;

import java.io.StringReader;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Topic;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.cosylab.acs.jms.ACSJMSSession;
import com.cosylab.acs.jms.ACSJMSTextMessage;
import com.cosylab.acs.jms.ACSJMSTopicSubscriber;

public class CategorySubscriber   implements MessageListener {
	
	public static final String NOT_AVAILABLE = "N/A";
	// Container services
	private ContainerServices acsCS;
	
	// Root and path names
	private String rootName;
	private String pathName;
	
	// The variables for JMS 
	private ACSJMSTopicSubscriber consumer;
	private ACSJMSSession session;
	private Topic topic;
	
	// The DOM parser
	private DocumentBuilder builder = null;
	
	// The listener of alarms
	private CategoryClient categoryClient;
	
	// Remember if the Subscriber has been closed
	private volatile boolean closed=false;
	
	/**
	 * Constructor 
	 * 
	 * @param contSvc ACS container services
	 * @param rootName The root name
	 * @param pathName The path name
	 */
	public CategorySubscriber(ContainerServices contSvc, String rootName, String pathName, CategoryClient categoryClient) throws Exception {
		if (contSvc==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices in constructor");
		}
		if (rootName==null || rootName.length()==0) {
			throw new IllegalArgumentException("Invalid root name in constructor");
		}
		if (pathName==null || pathName.length()==0) {
			throw new IllegalArgumentException("Invalid path name in constructor");
		}
		if (categoryClient==null) {
			throw new IllegalArgumentException("Invalid null CategoryClient in constructor");
		}
		this.categoryClient=categoryClient;
		acsCS=contSvc;
		this.rootName=rootName;
		this.pathName=pathName;
		initParser();
		connectSubscriber();
	}
	
	/**
	 * Close the subscriber
	 * 
	 * Should be called when done. 
	 */
	public void close() {
		if (closed) {
			return;
		}
		closed=true;
		try {
			consumer.close();
		} catch (Exception e) {
			System.err.println("Error closing the consumer "+e.getMessage());
			e.printStackTrace();
		}
	}
	
	/**
	 * @see Object.<code>finalize</code>
	 */
	public void finalize() {
		close();
		try {
			super.finalize();
		} catch (Throwable t) {
			System.err.println("Error finalizing "+t.getMessage());
			t.printStackTrace();
		}
	}
	
	/**
	 * Initialize the parser
	 *
	 */
	private void initParser() throws Exception {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

		builder = factory.newDocumentBuilder();
	}
	
	/**
	 * Connect the JMS consumer settting this class as listener
	 * for incoming messages 
	 * 
	 * @throws JMSException 
	 */
	private void connectSubscriber() throws JMSException {
		acsCS.getLogger().log(AcsLogLevel.INFO,"Connecting subscriber to "+rootName+"."+pathName);
		session = new ACSJMSSession(acsCS);
		topic = session.createTopic(rootName+"."+pathName);
		consumer = (ACSJMSTopicSubscriber)session.createConsumer(topic);
		if (consumer!=null) {
			acsCS.getLogger().log(AcsLogLevel.INFO,"Consumer connected to "+rootName+"."+pathName);
			consumer.setMessageListener(this);
		}
	}
	
	/**
	 * Executed when a message has been received
	 * 
	 * @param msg
	 */
	public void onMessage(Message msg) {
		if (closed) {
			return;
		}
		acsCS.getLogger().log(AcsLogLevel.DEBUG,"Message received from "+rootName+"."+pathName);
		if (msg instanceof ACSJMSTextMessage) {
			ACSJMSTextMessage textMsg = (ACSJMSTextMessage)msg;
			String content=null;
			try {
				content=textMsg.getText();
			} catch (JMSException e) {
				acsCS.getLogger().log(AcsLogLevel.WARNING,"Exception caught while getting a message "+e.getMessage());
				return;
			}
			if (content==null || content.length()==0) {
				return;
			}
			adddAlarmView(content);
		} else {
			System.out.println("Not a text msg");
		}
	}
	
	/**
	 * Get the fields of the alarm to show in the table and
	 * add the alarm to the model
	 * 
	 * @param s
	 */
	private void adddAlarmView(String xmlString) {
		String alarmID=""; // Triplet
		String priority = ""; // Priority
		String sourceTimestamp=null; // Source timestamp
		String description=NOT_AVAILABLE; // Problem description
		String cause=NOT_AVAILABLE; // The cause of the alarm
		String active=NOT_AVAILABLE; // Active
		String hostName=NOT_AVAILABLE;
		String nodeParent=NOT_AVAILABLE;
		String nodeChild=NOT_AVAILABLE;
		String multiplicityParent=NOT_AVAILABLE;
		String multiplicityChild=NOT_AVAILABLE;
		String reduced=NOT_AVAILABLE;
		String masked=NOT_AVAILABLE;
		
		Document document = null;
		try {
			document = builder.parse(new InputSource(new StringReader(xmlString)));
		} catch (Exception e) {
			acsCS.getLogger().log(AcsLogLevel.ERROR, "Error parsing an alarm");
			e.printStackTrace(System.err);
			return;
		}
		Node node = document.getFirstChild();
		NodeList list=node.getChildNodes();
		for (int t=0; t<list.getLength(); t++) {
			Node childNode = list.item(t);
			if (childNode.getNodeName().equals(("alarmId"))) {
				Node idNode = childNode.getLastChild();
				alarmID=idNode.getNodeValue();
			}
			if (childNode.getNodeName().equals(("priority"))) {
				Node priNode = childNode.getLastChild();
				priority=priNode.getNodeValue();
			}
			if (childNode.getNodeName().equals(("cause"))) {
				Node priNode = childNode.getLastChild();
				cause=priNode.getNodeValue();
			}
			if (childNode.getNodeName().equals(("nodeParent"))) {
				Node priNode = childNode.getLastChild();
				nodeParent=priNode.getNodeValue();
			}
			if (childNode.getNodeName().equals(("nodeChild"))) {
				Node priNode = childNode.getLastChild();
				nodeChild=priNode.getNodeValue();
			}
			if (childNode.getNodeName().equals(("multiplicityParent"))) {
				Node priNode = childNode.getLastChild();
				multiplicityParent=priNode.getNodeValue();
			}
			if (childNode.getNodeName().equals(("multiplicityChild"))) {
				Node priNode = childNode.getLastChild();
				multiplicityChild=priNode.getNodeValue();
			}
			// This tag sometimes appear inside visualFields (see below)
			if (childNode.getNodeName().equals("problemDescription")) {
				Node dateNode=childNode.getLastChild();
				if (dateNode!=null) {
					description=dateNode.getNodeValue();
				} else {
					description=NOT_AVAILABLE;
				}
			}
			if (childNode.getNodeName().equals(("status"))) {
				NodeList statusNodeList=childNode.getChildNodes();
				for (int j=0; j<statusNodeList.getLength(); j++) {
					Node statusNode=statusNodeList.item(j);
					if (statusNode.getNodeName().equals("active")) {
						Node activeNode=statusNode.getLastChild();
						active=activeNode.getNodeValue();
					}
					if (statusNode.getNodeName().equals("reduced")) {
						Node activeNode=statusNode.getLastChild();
						reduced=activeNode.getNodeValue();
					}
					if (statusNode.getNodeName().equals("masked")) {
						Node activeNode=statusNode.getLastChild();
						masked=activeNode.getNodeValue();
					}
					if (statusNode.getNodeName().equals("sourceTimestamp")) {
						NodeList timestampNodeList=statusNode.getChildNodes();
						for (int m=0; m<timestampNodeList.getLength(); m++) {
							Node timeNode=timestampNodeList.item(m);
							if (timeNode.getNodeName().equals("date")) {
								Node dateNode=timeNode.getLastChild();
								sourceTimestamp=dateNode.getNodeValue();
							}
						}
					}
				}	
			}
			// Get the host name
			if (childNode.getNodeName().equals("source")) {
				NodeList sourceNodeList=childNode.getChildNodes();
				for (int m=0; m<sourceNodeList.getLength(); m++) {
					Node hostNameNode=sourceNodeList.item(m);
					if (hostNameNode.getNodeName().equals("hostName")) {
						Node hNameNode=hostNameNode.getLastChild();
						if (hNameNode!=null) {
							hostName=hNameNode.getNodeValue();
						} else {
							hostName=NOT_AVAILABLE;
						}
						break;
					}
				}
			}
			
			// Sometimes problemDescription appeared into visualFields
			if (childNode.getNodeName().equals("visualFields")) {
				NodeList visualFieldsNodeList=childNode.getChildNodes();
				for (int m=0; m<visualFieldsNodeList.getLength(); m++) {
					Node descNode=visualFieldsNodeList.item(m);
					if (descNode.getNodeName().equals("problemDescription")) {
						Node dateNode=descNode.getLastChild();
						if (dateNode!=null) {
							description=dateNode.getNodeValue();
						} else {
							description=NOT_AVAILABLE;
						}
						break;
					}
				}
			}
		}
		try {
			categoryClient.dispatchAlarm(
					new AlarmView(
							alarmID,
							priority,
							sourceTimestamp,
							description,
							cause,
							active,
							hostName,
							nodeParent,
							nodeChild,
							multiplicityParent,
							multiplicityChild,
							reduced,
							masked));
		} catch (Throwable t) {
			System.out.println("Error building alarm "+t.getMessage());
		}
	}
}
