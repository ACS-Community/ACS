package com.cosylab.cdb.jdal;

import java.util.logging.Logger;

import org.omg.PortableServer.POA;

public class NoDestroyDAOImpl extends DAOImpl {

	public NoDestroyDAOImpl(String name, XMLTreeNode rootNode, POA poa,
			Logger logger) {
		super(name, rootNode, poa, logger);
	}

	public NoDestroyDAOImpl(String name, XMLTreeNode rootNode, POA poa,
			Logger logger, boolean silent) {
		super(name, rootNode, poa, logger, silent);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.DAOImpl#destroy()
	 */
	@Override
	public void destroy() {
		// TODO now destroy is disabled, why:
		// destroy should also remove this instance from cache (daoMap, wdaoMap, etc.)
		// reference counting is needed, since we have multiple clients
	}

	
}
