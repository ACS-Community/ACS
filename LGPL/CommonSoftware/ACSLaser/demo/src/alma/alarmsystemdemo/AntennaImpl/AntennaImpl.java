package alma.alarmsystemdemo.AntennaImpl;

import alma.alarmsystemdemo.AntennaOperations;
import alma.acs.component.ComponentImplBase;

import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterface;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.FaultState;
import java.sql.Timestamp;
import java.util.Properties;
import cern.laser.source.alarmsysteminterface.ASIException;

class AntennaImpl extends ComponentImplBase implements AntennaOperations
{
	public void faultAntenna() {
		System.out.println("faultAntenna");
		send_alarm("AlarmSource","ALARM_SOURCE_ANTENNA",1,FaultState.ACTIVE);
	}
	public void terminate_faultAntenna() {
		System.out.println("terminate_faultAntenna");
		send_alarm("AlarmSource","ALARM_SOURCE_ANTENNA",1,FaultState.TERMINATE);
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
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}