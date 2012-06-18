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
 * 
 */
package alma.acs.alarm.gui.senderpanel;

import java.util.List;
import java.util.concurrent.CountDownLatch;

import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;

import alma.acs.alarmsystem.generated.FaultCode;
import alma.acs.alarmsystem.generated.FaultFamily;
import alma.acs.alarmsystem.generated.FaultMember;
import alma.acs.container.ContainerServices;

/**
 * Send alarms read from the TM/CDB.
 * <P>
 * CDBSender reads alarms reusing the classes in laser-core.
 * As the reading can be quite slow when there are a lot of alarms defined in the 
 * TM/CDB, the reading is done by a dedicated thread and no other operation is 
 * allowed before it terminates.
 * {@link ACSAlarmDAOImpl} is quite slow because it reads all the alarms definitions
 * but also the reduction rules then matches them with the alarms and so on. 
 * This class does not need to know reduction rules and so on but has to wait until
 * the DAO finishes building its internal data structures. 
 * <P>
 * If a FaultFamily supports default fault member, then the triplet is built
 * with a '*' as FaultMember name.
 * 
 * @author acaproni
 *
 */
public class CDBSender extends BaseAlarmsSender {
	
	/**
	 * The latch to know that all the alarms have been read from the TM/CDB.
	 * <BR>It is not used at the present but ready for future needs.
	 * 
	 */
	private final CountDownLatch waitCDBLoading = new CountDownLatch(1);
	
	/**
	 * Constructor.
	 * <P>
	 * The constructor starts the thread that reads alarms from TM/CDB:
	 * as this task can be surprisingly slow, the thread is started as soon
	 * as possible in an attempt to hide this slowness to the user. 
	 * 
	 * @param parent the parent component of the dialog
	 * @param contSvcs The ContainerServices
	 * @param sender The object to send alarms
	 */
	public CDBSender(SenderPanel parent,ContainerServices contSvcs, AlarmSender sender) {
		super(parent, contSvcs,  sender,CDBSender.class.getName()+"_");
		
		alarmsSenderThread = threadFactory.newThread(new Runnable() {
			public void run() {
				try {
					readAlarmsFromCDB();
				} catch (Throwable t) {
					System.err.println("Error reading alarms from CDB: "+t.getMessage());
					t.printStackTrace();
				} finally {
					waitCDBLoading.countDown();
					notifyAlarmsRead();
					dumpAlarms();	
				}
			}
		});
		alarmsSenderThread.start();
	}
	
	/**
	 * Read alarms from the TM/CDB with the help of {@link ACSAlarmDAOImpl}.
	 * <P>
	 * This method can be quite slow so it would be better to run 
	 * in a dedicated thread.
	 * 
	 * @throws Exception
	 */
	private void readAlarmsFromCDB() throws Exception {
		ConfigurationAccessor conf;
		conf = ConfigurationAccessorFactory.getInstance(contSvcs);
		ACSAlarmDAOImpl alarmDAO = new ACSAlarmDAOImpl(contSvcs.getLogger());
		alarmDAO.setConfAccessor(conf);
		List<FaultFamily> FFs=alarmDAO.loadAlarms();
		synchronized (alarms) {
			for (FaultFamily ff : FFs) {
				String faultFamily = ff.getName();
				for (FaultMember fm : ff.getFaultMember()) {
					for (FaultCode fc : ff.getFaultCode()) {
						alarms.add(new AlarmRead(faultFamily + ","
								+ fm.getName() + "," + fc.getValue(), null));
					}
				}
				// Has this FF a default fault member?
				if (ff.getFaultMemberDefault()!=null) {
					System.out.println("Found a default fault member for "+ff.getName());
					for (FaultCode fc : ff.getFaultCode()) {
						alarms.add(new AlarmRead(faultFamily + ",*,"+ fc.getValue(), null));
					}
				}
			}
		}
	}

}
