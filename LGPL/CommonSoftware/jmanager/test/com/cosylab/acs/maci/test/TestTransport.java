/*
 * Created on 2.10.2006
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.acs.maci.test;

import java.util.HashMap;
import java.util.Map;

import com.cosylab.acs.maci.Daemon;
import com.cosylab.acs.maci.ServiceDaemon;
import com.cosylab.acs.maci.Transport;

/**
 * @author msekoranja
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TestTransport implements Transport {

	private Map daemons = new HashMap();
	
	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.Transport#getManagerReference()
	 */
	public String getManagerReference() {
		return "(local)";
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.Transport#getDaemon(java.lang.String)
	 */
	public Daemon getDaemon(String host) {
		synchronized (daemons)
		{
			Daemon daemon = (Daemon)daemons.get(host);
			if (daemon == null)
				throw new RuntimeException("no deamon on " + host);
			else
				return daemon;
		}
	}

	public void registerDeamon(String host, Daemon daemon)
	{
		synchronized (daemons)
		{
			daemons.put(host, daemon);
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.Transport#getServiceDaemon(java.lang.String)
	 */
	public ServiceDaemon getServiceDaemon(String host) {
		// TODO Auto-generated method stub
		return null;
	}
	
}
