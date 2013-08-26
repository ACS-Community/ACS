/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.plug;

import org.omg.CORBA.ORB;

import com.cosylab.acs.maci.Daemon;
import com.cosylab.acs.maci.ServiceDaemon;
import com.cosylab.acs.maci.Transport;

import alma.acs.util.ACSPorts;
import alma.acsdaemon.ContainerDaemonHelper;
import alma.acsdaemon.ServicesDaemonHelper;
import alma.acsdaemon.containerDaemonServiceName;
import alma.acsdaemon.servicesDaemonServiceName;

/**
 * CORBA implementation of transport.
 * @author msekoranja
 */
public class CORBATransport implements Transport {

	ORB orb;
	String managerIOR;
	
	/**
	 * Constructor.
	 * @param orb	CORBA ORB.
	 * @param managerIOR	manager IOR.
	 */
	public CORBATransport(ORB orb, String managerIOR)
	{
		this.orb = orb;
		this.managerIOR = managerIOR;
	}
	
	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.Transport#getDaemon(java.lang.String)
	 */
	public Daemon getDaemon(String host) {
		
		String daemonCORBALOC = "corbaloc::" + host + ":" + ACSPorts.getContainerDaemonPort() + "/" + containerDaemonServiceName.value;
		alma.acsdaemon.ContainerDaemon daemon = null;
		try {
			org.omg.CORBA.Object obj = orb.string_to_object(daemonCORBALOC);
			daemon = ContainerDaemonHelper.narrow(obj);
			if (daemon == null) {
				throw new NullPointerException("Daemon object was null");
			}
		} catch (Throwable thr) {
			throw new RuntimeException("Failed to resolve daemon reference for " + daemonCORBALOC, thr);
		}
		
		// return proxy
		return new DaemonProxy(daemon);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.Transport#getServiceDaemon(java.lang.String)
	 */
	public ServiceDaemon getServiceDaemon(String host) {
		
		String daemonCORBALOC = "corbaloc::" + host + ":" + ACSPorts.getServicesDaemonPort() + "/" + servicesDaemonServiceName.value;
		alma.acsdaemon.ServicesDaemon daemon = null;
		try {
			org.omg.CORBA.Object obj = orb.string_to_object(daemonCORBALOC);
			daemon = ServicesDaemonHelper.narrow(obj);
			if (daemon == null) {
				throw new NullPointerException("service daemon object was null");
			}
		} catch (Throwable thr) {
			throw new RuntimeException("Failed to resolve service daemon reference for " + daemonCORBALOC, thr);
		}
		
		// return proxy
		return new ServiceDaemonProxy(daemon);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.Transport#getManagerReference()
	 */
	public String getManagerReference() {
		return managerIOR;
	}
}
