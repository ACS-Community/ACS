package alma.alarmsystemdemo.MountImpl;

import alma.acs.component.ComponentImplBase;
import alma.alarmsystemdemo.MountOperations;
import alma.alarmsystemdemo.Antenna;
import alma.alarmsystemdemo.AntennaOperations;
import alma.alarmsystemdemo.AntennaHelper;

import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import java.sql.Timestamp;
import java.util.Properties;

class MountImpl extends ComponentImplBase implements MountOperations {
	Antenna antenna;
	public void faultMount() {
		send_alarm("AlarmSource","ALARM_SOURCE_MOUNT",1,ACSFaultState.ACTIVE);
		try { 
			Thread.sleep(5000);
		} catch (Exception e) {}
		Antenna ant = getAntenna();
		if (ant!=null) {
			ant.faultAntenna();
		}
	}
	public void terminate_faultMount() {
		send_alarm("AlarmSource","ALARM_SOURCE_MOUNT",1,ACSFaultState.TERMINATE);
		try { 
			Thread.sleep(5000);
		} catch (Exception e) {}
		Antenna ant = getAntenna();
		if (ant!=null) {
			ant.terminate_faultAntenna();
		}
	}
	
	public void send_alarm(String faultFamily, String faultMember, int faultCode, String faultState) {
		ACSAlarmSystemInterface alarmSource;
		try {
			alarmSource = ACSAlarmSystemInterfaceFactory.createSource(this.name());
			ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(
					faultFamily, faultMember, faultCode);
			fs.setDescriptor(faultState);
			fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

			Properties props = new Properties();
			props.setProperty(ACSFaultState.ASI_PREFIX_PROPERTY, "prefix");
			props.setProperty(ACSFaultState.ASI_SUFFIX_PROPERTY, "suffix");
			props.setProperty("TEST_PROPERTY", "TEST_VALUE");
			fs.setUserProperties(props);

			alarmSource.push(fs);
		} catch (Exception e) {
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
