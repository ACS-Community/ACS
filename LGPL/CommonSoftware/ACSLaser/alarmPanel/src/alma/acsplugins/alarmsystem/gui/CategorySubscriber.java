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
 * @version $Id: CategorySubscriber.java,v 1.1.1.1 2007/09/21 09:10:11 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;

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
	
	// The modell to ad alarms into
	private AlarmTableModel model;
	
	/**
	 * Constructor 
	 * 
	 * @param contSvc ACS container services
	 * @param rootName The root name
	 * @param pathName The path name
	 */
	public CategorySubscriber(ContainerServices contSvc, String rootName, String pathName, AlarmTableModel model) throws Exception {
		if (contSvc==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices in constructor");
		}
		if (rootName==null || rootName.length()==0) {
			throw new IllegalArgumentException("Invalid root name in constructor");
		}
		if (pathName==null || pathName.length()==0) {
			throw new IllegalArgumentException("Invalid path name in constructor");
		}
		if (model==null) {
			throw new IllegalArgumentException("Invalid null AlarmTableModel in constructor");
		}
		this.model=model;
		acsCS=contSvc;
		this.rootName=rootName;
		this.pathName=pathName;
		initParser();
		connectSubscriber();
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
		System.out.println("Connecting subscriber to "+rootName+"."+pathName);
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
		String description=""; // Problem description
		String cause=""; // The cause of the alarm
		String active=""; // Active
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
			if (childNode.getNodeName().equals(("status"))) {
				NodeList statusNodeList=childNode.getChildNodes();
				for (int j=0; j<statusNodeList.getLength(); j++) {
					Node statusNode=statusNodeList.item(j);
					if (statusNode.getNodeName().equals("active")) {
						Node activeNode=statusNode.getLastChild();
						active=activeNode.getNodeValue();
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
			if (childNode.getNodeName().equals("visualFields")) {
				NodeList visualFieldsNodeList=childNode.getChildNodes();
				for (int m=0; m<visualFieldsNodeList.getLength(); m++) {
					Node descNode=visualFieldsNodeList.item(m);
					if (descNode.getNodeName().equals("problemDescription")) {
						Node dateNode=descNode.getLastChild();
						description=dateNode.getNodeValue();
					}
				}
			}
		}
		model.addAlarm(alarmID,priority,sourceTimestamp,description,cause,active);
	}
}
