package alma.acs.monitoring.blobber;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.MonitorArchiver.Controller;
import alma.MonitorArchiver.ControllerHelper;
import alma.MonitorErr.CollectorRegistrationFailedEx;
import alma.MonitorErr.DeviceAlreadyRegisteredEx;
import alma.MonitorErr.DeviceNotRegisteredEx;
import alma.MonitorErr.RegisteringDeviceProblemEx;
import alma.MonitorErr.StartMonitoringProblemEx;
import alma.MonitorErr.StopMonitoringProblemEx;
import alma.TMCDB.MonitorBlob;
import alma.TMCDB.MonitorCollectorOperations;
import alma.TMCDB.MonitorDataBlock;
import alma.TMCDB.doubleBlobData;
import alma.TMCDB.doubleBlobDataSeqHelper;
import alma.TMCDB.doubleSeqBlobDataHelper;
import alma.TMCDB.doubleValueType;
import alma.TMCDB.propertySerailNumber;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

public class FileReaderCollectorImpl extends ComponentImplBase implements MonitorCollectorOperations {

	private static final String PREFIX = System.getProperty("alma.acs.monitoring.filesDir") + File.separatorChar;

	public static final String[] monitoredComponents = {
		"CONTROL/CentralLO/ML",
		"CONTROL/CentralLO/MLD",
		"CONTROL/CentralLO/PSLLC1",
		"CONTROL/CentralLO/PSLLC2",
		"CONTROL/CentralLO/PSSAS1",
		"CONTROL/CentralLO/PSSAS2",
		"CONTROL/PM03/IFProc0",
		"CONTROL/PM03/IFProc1",
		"CONTROL/PM03/LORR",
		"CONTROL/PM03/PSA",
		"CONTROL/PM03/PSD",
		"CONTROL/PM04/IFProc0",
		"CONTROL/PM04/IFProc1",
		"CONTROL/PM04/LORR",
		"CONTROL/PM04/PSA",
		"CONTROL/PM04/PSD",
	};
	private static final SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");

	private Map<String, List<FileBasedMonitorPoint>>   m_files = new HashMap<String, List<FileBasedMonitorPoint>>();

	private static class FileBasedMonitorPoint {
		public BufferedReader reader;
		public long lastTimestamp;
		public String propertyname;
		public long readBytes;
	}

	@Override
	public void initialize(ContainerServices inContainerServices) throws ComponentLifecycleException {
		super.initialize(inContainerServices);
		Controller controller;
		try {
			controller = ControllerHelper.narrow(m_containerServices.getComponent("ARCHIVE/TMCDB/MONITOR_CONTROL"));
			controller.registerCollector(name());
		} catch (AcsJContainerServicesEx ex) {
			throw new ComponentLifecycleException(ex);
		} catch (CollectorRegistrationFailedEx ex) {
			throw new ComponentLifecycleException(ex);
		}
	}

	@Override
	public void registerMonitoredDevice(String componentName,
			String serialNumber) throws RegisteringDeviceProblemEx,
			DeviceAlreadyRegisteredEx {
	}

	@Override
	public void registerMonitoredDeviceWithMultipleSerial(String componentName,
			propertySerailNumber[] serialNumbers)
			throws RegisteringDeviceProblemEx, DeviceAlreadyRegisteredEx {
	}

	@Override
	public void deregisterMonitoredDevice(String componentName)
			throws DeviceNotRegisteredEx {
	}

	@Override
	public void startMonitoring(String componentName)
			throws StartMonitoringProblemEx {

		String componentDirPath = PREFIX + componentName.replaceAll("/", "_");
		File componentDir = new File(componentDirPath);

		List<FileBasedMonitorPoint> monitorPointData = new ArrayList<FileBasedMonitorPoint>();
		for(String fileName: componentDir.list()) {
			FileBasedMonitorPoint mp = new FileBasedMonitorPoint();
			mp.propertyname = fileName.substring(0, fileName.lastIndexOf('.'));
			try {
				mp.reader = new BufferedReader(new FileReader(new File(componentDirPath + File.separator + fileName)));
				mp.reader.mark(80);
				mp.lastTimestamp = parseLine(mp.reader.readLine()).time;
				mp.reader.close();
				mp.readBytes = 0;
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			} catch (ParseException e) {
				e.printStackTrace();
			}
			monitorPointData.add(mp);
		}

		m_files.put(componentName, monitorPointData);

	}

	@Override
	public void stopMonitoring(String componentName)
			throws StopMonitoringProblemEx {
		
		for(String component: m_files.keySet())
			if( component.equals(componentName) ) {
				try {
					for(FileBasedMonitorPoint mp: m_files.get(component) )
						mp.reader.close();
				} catch (IOException e) {}
			}

	}

    @Override
    public void set_archiving_interval(String compName, String propName, long time) {
    }

    @Override
    public void enable_archiving(String compName, String propName) {
    }

    @Override
    public void suppress_archiving(String compName, String propName) {
    }

	@Override
	public MonitorDataBlock[] getMonitorData() {

		// For each component, we read all its related files
		List<MonitorDataBlock> blocks = new ArrayList<MonitorDataBlock>();
		for(String componentName: m_files.keySet()) {
 
			MonitorDataBlock block = new MonitorDataBlock();
			List<MonitorBlob> blobList = new ArrayList<MonitorBlob>();

			// For each file (property), we read some values and wrap them
			for(FileBasedMonitorPoint mp: m_files.get(componentName)) {

				try {
					String componentDirPath = PREFIX + componentName.replaceAll("/", "_");
					mp.reader = new BufferedReader(new FileReader(new File(componentDirPath + File.separator + mp.propertyname + ".txt")));
					if( !mp.reader.ready() )
						break;
					mp.reader.skip(mp.readBytes);
				} catch (IOException e1) {
					break;
				}

				String line = null;
				MonitorBlob blob = new MonitorBlob();
				blob.propertyName = mp.propertyname;
				blob.blobDataSeq = m_containerServices.getAdvancedContainerServices().getORB().create_any();
				blob.blobDataSeq.type(doubleSeqBlobDataHelper.type());
				blob.propertySerialNumber = new String[0];
				blob.typeOfValue = doubleValueType.value;
				List<doubleBlobData> dataList = new ArrayList<doubleBlobData>();

				do {

					try {

						mp.reader.mark(80);
						line = mp.reader.readLine();
						if( line == null )
							break;

						mp.readBytes += line.length() + 1; // +1 = '\n'
						doubleBlobData data = parseLine(line);

						// We suppose we're being called every 1 minute here
						m_logger.fine("Read: " + line + " for " + componentName + "/" + mp.propertyname);
						if( (data.time - mp.lastTimestamp) >= 60*1000 ) {
							m_logger.fine("Changing last timestamp from: " + mp.lastTimestamp + " for " + componentName + "/" + mp.propertyname);
							mp.lastTimestamp = mp.lastTimestamp + 60*1000;
							mp.reader.reset();
							mp.readBytes -= (line.length() + 1);
							break;
						}

						dataList.add(data);

					} catch (IOException e) {
						e.printStackTrace();
					} catch (ParseException e) {
						e.printStackTrace();
					}

				} while(line!=null);

				try {
					mp.reader.close();
				} catch (IOException e) {
					e.printStackTrace();
				}

				doubleBlobDataSeqHelper.insert(blob.blobDataSeq, dataList.toArray(new doubleBlobData[0]));
				blobList.add(blob);

			}
			block.componentName = componentName;
			block.deviceSerialNumber = "123456";
			block.startTime = new Date().getTime();
			block.stopTime = new Date().getTime();
			block.monitorBlobs = blobList.toArray(new MonitorBlob[blobList.size()]);

			blocks.add(block);
		}

		m_logger.fine("Returning " + blocks.size() + " MonitorDataBlocks");
		return blocks.toArray(new MonitorDataBlock[blocks.size()]);
	}

	private doubleBlobData parseLine(String line) throws IOException, ParseException {
		String[] parts = line.split(" ");
		Date timestamp = formatter.parse(parts[0]);
		Double value   = Double.valueOf(parts[1]);
		return new doubleBlobData(timestamp.getTime(), value);
	}
}
