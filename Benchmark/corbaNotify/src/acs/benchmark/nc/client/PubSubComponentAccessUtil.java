package acs.benchmark.nc.client;

import si.ijs.maci.ComponentSpec;

import acs.benchmark.util.ComponentAccessUtil;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServices;
import alma.acs.pubsubtest.config.types.ImplLangT;
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

//	/**
//	 * @deprecated Use {@link #getDynamicSupplierComponent(String, String, ImplLangT)}
//	 */
//	public CorbaNotifySupplierOperations getDynamicJavaSupplierComponent(String componentName, String containerName) throws AcsJContainerServicesEx {
//		return getDynamicSupplierComponent(componentName, containerName, ImplLangT.JAVA);
//	}
	
	public CorbaNotifySupplierOperations getDynamicSupplierComponent(String componentName, String containerName, ImplLangT implLang) throws AcsJContainerServicesEx {
		String code = null;
		switch (implLang.getType()) {
		case ImplLangT.JAVA_TYPE:
			// Cannot use class#getName because the component helper is not visible to the normal client classloader
			code = "acs.benchmark.nc.comp.publisher.CorbaNotifySupplierComponentHelper"; 
			break;
		default:
			throw new IllegalArgumentException("Component impl '" + implLang.toString() + "' not yet supported!");
		}
		ComponentSpec compSpec = new ComponentSpec(componentName,
				CorbaNotifySupplierHelper.id(), // "IDL:alma/benchmark/CorbaNotifySupplier:1.0" 
				code, 
				containerName);
		return getDynamicComponent(compSpec, CorbaNotifySupplierOperations.class);
	}

	
	/**
	 * @deprecated Use {@link #getDynamicSubscriberComponent(String, String, ImplLangT)}
	 */
	public CorbaNotifyConsumerOperations getDynamicJavaSubscriberComponent(String componentName, String containerName) throws AcsJContainerServicesEx {
		return getDynamicSubscriberComponent(componentName, containerName, ImplLangT.JAVA);
	}
	
	public CorbaNotifyConsumerOperations getDynamicSubscriberComponent(String componentName, String containerName, ImplLangT implLang) throws AcsJContainerServicesEx {
		String code = null;
		switch (implLang.getType()) {
		case ImplLangT.JAVA_TYPE:
			// Cannot use class#getName because the component helper is not visible to the normal client classloader
			code = "acs.benchmark.nc.comp.subscriber.CorbaNotifyConsumerComponentHelper"; 
			break;
			
		default:
			throw new IllegalArgumentException("Component impl '" + implLang.toString() + "' not yet supported!");
		}
		ComponentSpec compSpec = new ComponentSpec(componentName,
				CorbaNotifyConsumerHelper.id(), // "IDL:alma/benchmark/CorbaNotifyConsumer:1.0" 
				code,
				containerName);
		return getDynamicComponent(compSpec, CorbaNotifyConsumerOperations.class);
	}
	
}
