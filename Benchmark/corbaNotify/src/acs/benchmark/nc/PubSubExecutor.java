package acs.benchmark.nc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;

import alma.acs.logging.ClientLogManager;
import alma.acs.pubsubtest.config.ContainerSpecT;
import alma.acs.pubsubtest.config.PubSubTestSpec;

/**
 * Creates publisher and subscriber components as specified 
 * @author hsommer
 */
public class PubSubExecutor
{
	private final Logger logger;


	public PubSubExecutor(Logger logger) {
		this.logger = logger;
	}
	
	/**
	 * Loads the test spec (castor classes) from an xml file.
	 * <p>
	 * This method does not change any fields (could be static).
	 */
	public PubSubTestSpec loadXmlSpec(File xmlFile, boolean validate) 
					throws FileNotFoundException, IOException, MarshalException, ValidationException {
		if (xmlFile.exists() && !xmlFile.isDirectory() && xmlFile.canRead()) {
			Unmarshaller unm = new Unmarshaller(PubSubTestSpec.class);
			unm.setValidation(validate);
			return (PubSubTestSpec) unm.unmarshal(new FileReader(xmlFile));
		}
		else {
			throw new FileNotFoundException(xmlFile.getAbsolutePath());
		}
	}
	
	
	/**
	 * Sets container host names in the given <code>PubSubTestSpec</code>.
	 * <p>
	 * This method does not change any fields (could be static).
	 * @param spec  Pub/sub test spec. Host names for containers will be overwritten by matching data in <code>containerHostMapping</code>.
	 * @param containerHostMapping  Container-host mapping in the form <code>container1=host1:container2=host2</code>
	 */
	public void setHostNames(PubSubTestSpec spec, String containerHostMapping) {
		
		logger.finer("Will process container host mapping '" + containerHostMapping + "'.");
		Map<String, String> containerHostMappingMap = new HashMap<String, String>();
		
		for (String mapping : containerHostMapping.split(":+")) {
			String[] tmp = mapping.split("=");
			if (tmp.length == 2) {
				containerHostMappingMap.put(tmp[0], tmp[1]);
			}
			else {
				logger.info("Failed to process container-host mapping '" + mapping + "'.");
			}
		}
		
		setHostNames(spec, containerHostMappingMap);
	}
	
	
	/**
	 * Sets container host names in the given <code>PubSubTestSpec</code>.
	 * <p>
	 * This method does not change any fields (could be static).
	 * @param spec  Pub/sub test spec. Host names for containers will be overwritten by matching data in <code>containerHostMapping</code>.
	 * @param containerHostMapping  key = container name, value = host name.
	 */
	public void setHostNames(PubSubTestSpec spec, Map<String, String> containerHostMapping) {
		ContainerSpecT[] containerSpecs = spec.getContainer();
		
		for (ContainerSpecT containerSpec : containerSpecs) {
			String hostName = containerHostMapping.get(containerSpec.getContainerName());
			if (hostName != null && !hostName.trim().isEmpty()) {
				if (containerSpec.getHostName() == null || containerSpec.getHostName().isEmpty()) {
					logger.fine("Mapping container '" + containerSpec.getContainerName() + "' to host '" + hostName + "'.");
				}
				else {
					logger.info("Mapping container '" + containerSpec.getContainerName() + "' to host '" + hostName 
							+ "', overwriting old host name '" + containerSpec.getHostName() + "'.");
				}
				containerSpec.setHostName(hostName);
			}
			else {
				logger.fine("No host mapping found for container '" + containerSpec.getContainerName() + "'. Will use localhost.");
				containerSpec.setHostName(null);
			}
		}
	}
	

	public void execute(PubSubTestSpec spec) {
		logger.info("Do not execute!");
	}

	/**
	 * Parameters:
	 * <ol>
	 *   <li>XML config file name.
	 *   <li>Optional container-host mapping in the form <code>container1=host1:container2=host2</code>.
	 * </ol>
	 * @param args 
	 */
	public static void main(String[] args) {
		if (args.length < 1 || args.length > 2) {
			throw new IllegalArgumentException("Expecting 1 or 2 arguments: <XML config file> [<container1=host1:>]");
		}
		
		try {
			Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(PubSubExecutor.class.getSimpleName(), false);
			PubSubExecutor exec = new PubSubExecutor(logger);
			File xmlFile = new File(args[0]);
			PubSubTestSpec spec = exec.loadXmlSpec(xmlFile, true);
			if (args.length == 2) {
				exec.setHostNames(spec, args[1]);
			}
			else {
				logger.fine("No container-host mapping specified, will use localhost for all containers.");
			}
			exec.execute(spec);
		} catch (Throwable thr) {
			thr.printStackTrace();
		}
	}

}
