/*
 * Created on Mar 6, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.perftest.client;

import java.util.logging.Logger;

import alma.acs.component.client.AdvancedComponentClient;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ComponentClientSingleton extends AdvancedComponentClient {
	private static AdvancedComponentClient instance = null;

	private ComponentClientSingleton(Logger logger, String managerLoc, String clientName) throws Exception {
		super(logger, managerLoc, clientName);
	}

	public static AdvancedComponentClient getInstance() {
		return instance;
	}

	public synchronized static void prepareInstance(Logger logger,
			String managerLoc, String clientName) throws Exception {
		if (instance == null)
			instance = new ComponentClientSingleton(logger, managerLoc, clientName);
	}

	public synchronized static void destroyInstance() throws Exception {
		if (instance != null) {
			instance.tearDown();
		}
		instance = null;
	}
}
