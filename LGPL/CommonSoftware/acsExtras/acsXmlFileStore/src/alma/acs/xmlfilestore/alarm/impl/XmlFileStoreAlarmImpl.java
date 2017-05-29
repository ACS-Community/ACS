/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2016 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.xmlfilestore.alarm.impl;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.util.stringqueue.TimestampedStringQueue;
import alma.acs.xmlfilestore.common.QueueFileHandler;
import alma.alarmsystem.clients.SourceClient;
import alma.alarmsystem.clients.source.SourceListener;
import alma.xmlFileStore.AlarmsXmlStoreOperations;
import cern.laser.source.alarmsysteminterface.FaultState;

/** 
 * @author  acaproni
 * @since   ACS 2016.6
 */

public class XmlFileStoreAlarmImpl extends ComponentImplBase implements AlarmsXmlStoreOperations, SourceListener {
	
	/**
	 * The name of the java property to set the max size of each file of logs
	 */
	static public final String MAXFILESIZE_PROPNAME = "alma.acs.extras.alarmLogger.maxFileSize";
	
	/**
	 * The default max size of each file of logs
	 */
	static public final int DEFAULTMAXFILESIZE = 134217728;
	
	/**
	 * The name of the java property to set the max number of file of logs
	 */
	static public final String MAXNUMBEROFFILES_PROPNAME = "alma.acs.extras.alarmlogger.maxNumberFiles";
	
	/**
	 * The default max number of file of logs
	 */
	static public final int DEFAULTMAXNUMBEROFFILES = 750;
	
	/**
	 * The name of the java property to set the path to save the file of logs
	 */
	static public final String LOGDIR_PROPNAME = "alma.acs.extras.alarmlogger.logDir";
	
	/**
	 * The default max number of file of logs
	 */
	static public final String DEFAULLOGDIR = "/mnt/gas02/data1/AcsAlarmLogs-8.1";

	/**
	 * Container services
	 */
	private ContainerServices cs;
	
	/**
	 * The logger
	 */
	private Logger m_logger;
	
	/**
	 * The queue of XMLs
	 */
	private TimestampedStringQueue queue;
	
	
	
	/**
	 * The alarm source client to get the XML to store on files
	 */
	private SourceClient alarmSourceClient;
	
	/**
	 *  Constructor
	 */
	public XmlFileStoreAlarmImpl() {
		super();
	}
	
	/**
	 * Life cycle
	 * @see alma.acs.component.ComponentLifecycle#initialize()
	 */
	@Override
	public void initialize(ContainerServices containerServices)
			throws ComponentLifecycleException {
		super.initialize(containerServices);
		cs = containerServices;
		m_logger = cs.getLogger();
		
		// Prepare the queue
		String folderPath = System.getProperty(XmlFileStoreAlarmImpl.LOGDIR_PROPNAME, XmlFileStoreAlarmImpl.DEFAULLOGDIR);
		File f = new File(folderPath);
		if (!f.exists()) {
			f.mkdirs();
		}
		
		int fileMax = Integer.getInteger(XmlFileStoreAlarmImpl.MAXNUMBEROFFILES_PROPNAME, XmlFileStoreAlarmImpl.DEFAULTMAXNUMBEROFFILES);
		int fileSizeLimit = Integer.getInteger(XmlFileStoreAlarmImpl.MAXFILESIZE_PROPNAME, XmlFileStoreAlarmImpl.DEFAULTMAXFILESIZE);
		if (fileMax < 1 || fileSizeLimit < 100000 ) {
			StringBuilder str = new StringBuilder(XmlFileStoreAlarmImpl.MAXNUMBEROFFILES_PROPNAME);
			str.append(" must be greater then 1 and ");
			str.append(XmlFileStoreAlarmImpl.MAXFILESIZE_PROPNAME);
			str.append(" must be greater then 100000");
			throw new ComponentLifecycleException(str.toString());
		}
		
		StringBuilder str = new StringBuilder("Will save alarms files in : ");
		str.append(folderPath);
		str.append(" (max log file size: ");
		str.append(fileSizeLimit);
		str.append(", max # log files: " );
		str.append(fileMax);
		str.append(')');
		m_logger.info(str.toString());
		QueueFileHandler qFileHandler;
		try {
			qFileHandler = new QueueFileHandler(cs, folderPath,fileMax, fileSizeLimit, "alarmSources", "AlarmsLogger");
		} catch (Throwable t) {
			throw new ComponentLifecycleException("Could not create the queue file handler",t);
		}
		queue = new TimestampedStringQueue(qFileHandler, "<source-timestamp>");
		queue.start();
		
		// Connects to the alarm source NC
		try {
			alarmSourceClient = new SourceClient(cs);
			alarmSourceClient.connect();
		} catch (Throwable t) {
			throw new ComponentLifecycleException("Could not connect to alarm source NC",t);
		}
		alarmSourceClient.addAlarmListener(this);
	}
	
	/**
	 * Life cycle
	 * @see alma.acs.component.ComponentLifecycle#cleanUp()
	 */
	@Override
	public void cleanUp() throws alma.maciErrType.wrappers.AcsJComponentCleanUpEx {
		alarmSourceClient.close();
		queue.close(true);
		if (m_logger.isLoggable(Level.FINE)) m_logger.fine("cleaning up");
		super.cleanUp();
	}
	
	/**
	 * @see SourceListener#sourceXMLMsgReceived(String)
	 */
	@Override
	public void sourceXMLMsgReceived(String xml) {
		// ASI message begins with XML start tag that we will remove
		xml=xml.trim();
		if (xml.startsWith("<?xml")) {
			int retChar=xml.indexOf('>');
			xml=xml.substring(retChar+1).trim();
		}
		try {
			queue.push(xml);
		} catch (Throwable t) {
			m_logger.log(AcsLogLevel.ERROR,"Error pushing an alarm in the queue: alarm lost",t);
		}
	}
	
	/**
	 * This method does nothing as we are interested in storing the XMLs
	 * @see SourceListener#faultStateReceived(cern.laser.source.alarmsysteminterface.FaultState)
	 */
	@Override
	public void faultStateReceived(FaultState fs) {}
}
