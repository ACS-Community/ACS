package acs.benchmark.nc.client;

import si.ijs.maci.ComponentSpec;

import acs.benchmark.util.ComponentAccessUtil;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServices;
import alma.benchmark.CorbaNotifyConsumerHelper;
import alma.benchmark.CorbaNotifyConsumerOperations;
import alma.benchmark.CorbaNotifySupplierHelper;
import alma.benchmark.CorbaNotifySupplierOperations;

/**
 * ComponentAccessUtil with special support for test publisher and subscriber components.
 */
public class PubSubComponentAccessUtil extends ComponentAccessUtil
{
	public PubSubComponentAccessUtil(ContainerServices contSrv) {
		super(contSrv);
	}

	public CorbaNotifySupplierOperations getDynamicJavaSupplierComponent(String componentName, String containerName) throws AcsJContainerServicesEx {
		ComponentSpec compSpec = new ComponentSpec(componentName,
				CorbaNotifySupplierHelper.id(), // "IDL:alma/benchmark/CorbaNotifySupplier:1.0" 
				"acs.benchmark.nc.supplier.CorbaNotifySupplierComponentHelper", // component helper not visible to normal CL 
				containerName);
		return getDynamicComponent(compSpec, CorbaNotifySupplierOperations.class);
	}
	
	public CorbaNotifyConsumerOperations getDynamicJavaSubscriberComponent(String componentName, String containerName) throws AcsJContainerServicesEx {
		ComponentSpec compSpec = new ComponentSpec(componentName,
				CorbaNotifyConsumerHelper.id(), // "IDL:alma/benchmark/CorbaNotifyConsumer:1.0" 
				"acs.benchmark.nc.consumer.CorbaNotifyConsumerComponentHelper", // component helper not visible to normal CL 
				containerName);
		return getDynamicComponent(compSpec, CorbaNotifyConsumerOperations.class);
	}
	
}
