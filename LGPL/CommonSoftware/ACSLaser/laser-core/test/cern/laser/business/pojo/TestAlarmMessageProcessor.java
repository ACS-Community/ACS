/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2011
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
 */

package cern.laser.business.pojo;

import java.net.InetAddress;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.jms.JMSException;
import javax.jms.Message;

import junit.framework.TestCase;

import org.apache.log4j.Appender;
import org.apache.log4j.Layout;
import org.apache.log4j.Level;
import org.apache.log4j.spi.ErrorHandler;
import org.apache.log4j.spi.Filter;
import org.apache.log4j.spi.LoggingEvent;
import org.omg.CORBA.ORB;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.testsupport.DummyContainerServices;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.ACSPorts;
import cern.laser.business.cache.AlarmCacheListenerImpl;
import cern.laser.business.data.AlarmChange;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;
import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;
import com.cosylab.acs.jms.ACSJMSTextMessage;
import com.cosylab.acs.laser.dao.ACSAlarmCacheImpl;
import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
import com.cosylab.acs.laser.dao.ACSCategoryDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;

public class TestAlarmMessageProcessor extends TestCase {

	private static Logger m_logger;
	private static final int N_STATE_CHANGES = 2;
	private static String hostname;
	private static ORB m_orb;
	private static DAL theDAL;
	private static AlarmMessageProcessorImpl processor;
	private static Calendar loadingTime;

	static {
		org.apache.log4j.Logger root = org.apache.log4j.Logger.getRootLogger();
		root.removeAllAppenders();
		root.addAppender(new Appender() {

			@Override
			public void setName(String paramString) {
			}

			@Override
			public void setLayout(Layout paramLayout) {
			}

			@Override
			public void setErrorHandler(ErrorHandler paramErrorHandler) {
			}

			@Override
			public boolean requiresLayout() {
				return false;
			}

			@Override
			public String getName() {
				return null;
			}

			@Override
			public Layout getLayout() {
				return null;
			}

			@Override
			public Filter getFilter() {
				return null;
			}

			@Override
			public ErrorHandler getErrorHandler() {
				return null;
			}

			@Override
			public void close() {
			}

			@Override
			public void clearFilters() {
			}

			@Override
			public void addFilter(Filter paramFilter) {
			}

			private Map<Level, java.util.logging.Level> levels = new HashMap<Level, java.util.logging.Level>();
			{
				levels.put(Level.INFO, java.util.logging.Level.INFO);
				levels.put(Level.DEBUG, java.util.logging.Level.FINE);
				levels.put(Level.TRACE, java.util.logging.Level.FINER);
				levels.put(Level.WARN, java.util.logging.Level.WARNING);
				levels.put(Level.ERROR, java.util.logging.Level.SEVERE);
			}

			@Override
			public void doAppend(LoggingEvent event) {
				LogRecord record = new LogRecord(levels.get(event.getLevel()), event.getRenderedMessage());
				record.setLoggerName(event.getLoggerName());
				m_logger.log(record);
			}

		});

		System.setProperty("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
		System.setProperty("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");
		hostname = "localhost";
		try {
			m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("TestAlarmMessageProcessor", false);
			hostname = InetAddress.getLocalHost().getHostName();
			m_orb = ORB.init((String[])null, null);
			theDAL = DALHelper.narrow(m_orb.string_to_object("corbaloc::" + hostname + ":" + ACSPorts.getCDBPort() + "/CDB"));

			ConfigurationAccessor configAccessor = ConfigurationAccessorFactory.getInstance(new DummyContainerServices(null, m_logger) {
				@Override
				public DAL getCDB() throws AcsJContainerServicesEx {
					return theDAL;
				}
			});

			ACSAlarmDAOImpl alarmDAO = new ACSAlarmDAOImpl(m_logger);
			ACSCategoryDAOImpl categoryDAO = new ACSCategoryDAOImpl(m_logger, alarmDAO);
			alarmDAO.setConfAccessor(configAccessor);
			categoryDAO.setConfAccessor(configAccessor);
			long startTime = System.currentTimeMillis();
			alarmDAO.loadAlarms();
			long endTime = System.currentTimeMillis();
			loadingTime = Calendar.getInstance();
			loadingTime.setTimeInMillis(endTime-startTime);
			System.out.println("Loading alarm execution time: "+loadingTime.get(Calendar.MINUTE)+" mins and "+loadingTime.get(Calendar.SECOND)+" secs");
			categoryDAO.loadCategories();

			AlarmCacheServerImpl server = new AlarmCacheServerImpl() {
				@Override
				public void publish(Collection alarmChanges) {
					for (Object o: alarmChanges) {
						AlarmChange change = (AlarmChange)o;
						m_logger.info(change.getAlarmId() + " changed to " + change.getCurrent().getStatus().getActive());
					}
				}
				@Override
				public void notify(Collection alarmChanges) {
					// no-op, don't try to send e-mails or SMSs
				}
			};
			server.setAlarmDAO(alarmDAO);
			AlarmCacheListenerImpl listener = new AlarmCacheListenerImpl(server);
			ACSAlarmCacheImpl alarmCache = new ACSAlarmCacheImpl(alarmDAO, categoryDAO, listener, m_logger);

			processor = new AlarmMessageProcessorImpl();
			processor.setAlarmCache(alarmCache);
		} catch (Exception e) { 
			e.printStackTrace();
		}
	}

	public void testStressKnownAlarmsUmatchingRR() throws Exception {
		Message[] messages =  new Message[6];
		messages[0] = createJMSMessage("PSA", "CONTROL/DV01/PSA", 1, true);
		messages[1] = createJMSMessage("PSA", "CONTROL/DV02/PSA", 1, true);
		messages[2] = createJMSMessage("PSA", "CONTROL/LA02/PSA", 1, true);
		messages[3] = createJMSMessage("PSA", "CONTROL/DV01/PSA", 2, true);
		messages[4] = createJMSMessage("PSA", "CONTROL/DV02/PSA", 2, true);
		messages[5] = createJMSMessage("PSA", "CONTROL/LA02/PSA", 2, true);
		internalTestStressWithMessages(messages);
	}

	public void testStressUnknownAlarmsMatchingRR() throws Exception {
		Message[] messages =  new Message[6];
		messages[0] = createJMSMessage("PSD", "CONTROL/CA01/PSD", 2, true);
		messages[1] = createJMSMessage("PSD", "CONTROL/CA02/PSD", 2, true);
		messages[2] = createJMSMessage("PSD", "CONTROL/CA03/PSA", 2, true);
		messages[3] = createJMSMessage("DTX", "CONTROL/CA01/DTXBBpr4", 10, true);
		messages[4] = createJMSMessage("DTX", "CONTROL/CA02/DTXBBpr4", 10, true);
		messages[5] = createJMSMessage("DTX", "CONTROL/CA03/DTXBBpr4", 10, true);
		internalTestStressWithMessages(messages);
	}

	public void testStressUnknownAlarmsUmatchingRR() throws Exception {
		Message[] messages =  new Message[6];
		messages[0] = createJMSMessage("PSD", "CONTROL/CA01/PSD", 2, true);
		messages[1] = createJMSMessage("PSD", "CONTROL/CA02/PSD", 2, true);
		messages[2] = createJMSMessage("PSD", "CONTROL/CA03/PSA", 2, true);
		messages[3] = createJMSMessage("DTX", "CONTROL/CA01/DTXBBpr0", 10, true);
		messages[4] = createJMSMessage("DTX", "CONTROL/CA02/DTXBBpr0", 10, true);
		messages[5] = createJMSMessage("DTX", "CONTROL/CA03/DTXBBpr0", 10, true);
		internalTestStressWithMessages(messages);
	}

	public void internalTestStressWithMessages(Message[] messages) throws Exception {
		for (int i = 0; i < messages.length; i++)
			processor.process(messages[i]);
	}

	private ACSJMSTextMessage createJMSMessage(String ff, String fm, int code, boolean active)
			throws Exception, JMSException {

		// Create the fault states
		List<FaultState> states = new ArrayList<FaultState>();
		for(int j=0; j!= N_STATE_CHANGES; j++) {
			FaultState faultState = new FaultStateImpl(ff, fm, code);
			faultState.setDescriptor( active ? FaultState.ACTIVE : FaultState.TERMINATE );
			faultState.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
			states.add(faultState);
		}

		// Create the ASIMessage with the fault states
		ASIMessage asiMessage = ASIMessageHelper.marshal(states);
		cern.laser.source.alarmsysteminterface.impl.message.Timestamp timestamp = new cern.laser.source.alarmsysteminterface.impl.message.Timestamp();
		long currentTimeMillis = System.currentTimeMillis();
		timestamp.setMicroseconds(currentTimeMillis%1000);
		timestamp.setSeconds(currentTimeMillis/1000);
		asiMessage.setSourceTimestamp(timestamp);
		asiMessage.setSourceHostname(hostname);
		asiMessage.setBackup(false);
		asiMessage.setSourceName("ALARM_SYSTEM_SOURCES");

		// And wrap it inside a JMS text message
		ACSJMSTextMessage message = new ACSJMSTextMessage(new DummyContainerServices("test", m_logger));
		String text = XMLMessageHelper.marshal(asiMessage);
		message.setText(text);
		return message;
	}

	@Override
	protected void tearDown() throws Exception {
		// TODO Auto-generated method stub
		super.tearDown();
		System.out.println("Loading alarm execution time: "+loadingTime.get(Calendar.MINUTE)+" mins and "+loadingTime.get(Calendar.SECOND)+" secs");
	}

}