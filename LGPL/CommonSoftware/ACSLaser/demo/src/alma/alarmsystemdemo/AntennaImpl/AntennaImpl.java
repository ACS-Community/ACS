package alma.alarmsystemdemo.AntennaImpl;

import alma.alarmsystemdemo.AntennaOperations;
import alma.acs.component.ComponentImplBase;

import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

import java.sql.Timestamp;
import java.util.Properties;

class AntennaImpl extends ComponentImplBase implements AntennaOperations
{
	public void faultAntenna() {
		send_alarm("Antenna","ALARM_SOURCE_ANTENNA",1,ACSFaultState.ACTIVE);
	}
	public void terminate_faultAntenna() {
		send_alarm("Antenna","ALARM_SOURCE_ANTENNA",1,ACSFaultState.TERMINATE);
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
}
