package alma.alarmsystemdemo.PSImpl;

import alma.acs.component.ComponentImplBase;
import alma.alarmsystemdemo.PSOperations;
import alma.alarmsystemdemo.Mount;
import alma.alarmsystemdemo.MountOperations;
import alma.alarmsystemdemo.MountHelper;

import cern.laser.source.alarmsysteminterface.AlarmSystemInterface;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.FaultState;
import java.sql.Timestamp;
import java.util.Properties;
import cern.laser.source.alarmsysteminterface.ASIException;



class PSImpl extends ComponentImplBase implements PSOperations {
	Mount mount = null;
	
	public void faultPS() {
		System.out.println("faultPS");
		send_alarm("AlarmSource","ALARM_SOURCE_PS",1,FaultState.ACTIVE);
		try { 
			Thread.sleep(5000);
		} catch (Exception e) {}
		Mount mnt = getMount();
		if (mnt!=null) {
			mnt.faultMount();
		}
	}
	public void terminate_faultPS() {
		System.out.println("faultPS");
		send_alarm("AlarmSource","ALARM_SOURCE_PS",1,FaultState.TERMINATE);
		try { 
			Thread.sleep(5000);
		} catch (Exception e) {}
		Mount mnt = getMount();
		if (mnt!=null) {
			mnt.terminate_faultMount();
		}
	}
	
	public void send_alarm(String faultFamily, String faultMember, int faultCode, String faultState) {
		AlarmSystemInterface alarmSource;
		try {
			alarmSource = AlarmSystemInterfaceFactory.createSource(this.name());
			FaultState fs = AlarmSystemInterfaceFactory.createFaultState(
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
	
	private Mount getMount() {
		if (this.mount==null) {
			org.omg.CORBA.Object cmp = null;
			try
			{
				cmp = m_containerServices.getComponent("ALARM_SOURCE_MOUNT");
				mount = MountHelper.narrow(cmp);
			}
			catch (Exception ex)
			{}
		}
		return mount;
	}
}
