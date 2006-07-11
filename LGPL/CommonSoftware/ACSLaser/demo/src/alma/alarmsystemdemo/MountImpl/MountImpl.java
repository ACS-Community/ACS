package alma.alarmsystemdemo.MountImpl;

import alma.acs.component.ComponentImplBase;
import alma.alarmsystemdemo.MountOperations;
import alma.alarmsystemdemo.Antenna;
import alma.alarmsystemdemo.AntennaOperations;
import alma.alarmsystemdemo.AntennaHelper;

import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterface;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.FaultState;
import java.sql.Timestamp;
import java.util.Properties;
import cern.laser.source.alarmsysteminterface.ASIException;

class MountImpl extends ComponentImplBase implements MountOperations {
	Antenna antenna;
	public void faultMount() {
		System.out.println("faultMount");
		send_alarm("AlarmSource","ALARM_SOURCE_MOUNT",1,FaultState.ACTIVE);
		try { 
			Thread.sleep(5000);
		} catch (Exception e) {}
		Antenna ant = getAntenna();
		if (ant!=null) {
			ant.faultAntenna();
		}
	}
	public void terminate_faultMount() {
		System.out.println("terminate_faultMount");
		send_alarm("AlarmSource","ALARM_SOURCE_MOUNT",1,FaultState.TERMINATE);
		try { 
			Thread.sleep(5000);
		} catch (Exception e) {}
		Antenna ant = getAntenna();
		if (ant!=null) {
			ant.terminate_faultAntenna();
		}
	}
	
	public void send_alarm(String faultFamily, String faultMember, int faultCode, String faultState) {
		AlarmSystemInterface alarmSource;
		try {
			alarmSource = ACSAlarmSystemInterfaceFactory.createSource(this.name());
			FaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(
					faultFamily, faultMember, faultCode);
			fs.setDescriptor(faultState);
			fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

			Properties props = new Properties();
			props.setProperty(FaultState.ASI_PREFIX_PROPERTY, "prefix");
			props.setProperty(FaultState.ASI_SUFFIX_PROPERTY, "suffix");
			props.setProperty("TEST_PROPERTY", "TEST_VALUE");
			fs.setUserProperties(props);

			alarmSource.push(fs);
		} catch (ASIException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private Antenna getAntenna() {
		if (this.antenna==null) {
			org.omg.CORBA.Object cmp = null;
			try
			{
				cmp = m_containerServices.getComponent("ALARM_SOURCE_ANTENNA");
				antenna = AntennaHelper.narrow(cmp);
			}
			catch (Exception ex)
			{}
		}
		return antenna;
	}
}