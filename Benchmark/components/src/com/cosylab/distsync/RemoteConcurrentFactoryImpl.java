/*
 * Copyright (c) 2003 by Cosylab d.o.o.
 *
 * The full license specifying the redistribution, modification, usage and other
 * rights and obligations is included with the distribution of this project in
 * the file license.html. If the license is not included you may find a copy at
 * http://www.cosylab.com/legal/abeans_license.htm or may write to Cosylab, d.o.o.
 *
 * THIS SOFTWARE IS PROVIDED AS-IS WITHOUT WARRANTY OF ANY KIND, NOT EVEN THE
 * IMPLIED WARRANTY OF MERCHANTABILITY. THE AUTHOR OF THIS SOFTWARE, ASSUMES
 * _NO_ RESPONSIBILITY FOR ANY CONSEQUENCE RESULTING FROM THE USE, MODIFICATION,
 * OR REDISTRIBUTION OF THIS SOFTWARE.
 */

package com.cosylab.distsync;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import java.util.Hashtable;
import java.util.Map;


/**
 * DOCUMENT ME!
 *
 * @author $Author: dfugate $
 * @version $Revision: 1.1 $
 */
public class RemoteConcurrentFactoryImpl extends UnicastRemoteObject
	implements RemoteConcurrentFactory
{
	private Map cyclicBarriers = new Hashtable();

	/**
	 * DOCUMENT ME!
	 *
	 * @throws RemoteException
	 */
	protected RemoteConcurrentFactoryImpl() throws RemoteException
	{
		super();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.distsync.RemoteBarrierFactory#createBarrier(int)
	 */
	synchronized public RemoteCyclicBarrier getCyclicBarrier(String name,
	    int parties) throws RemoteException
	{
		RemoteCyclicBarrier result;

		if (this.cyclicBarriers.containsKey(name)) {
			result = (RemoteCyclicBarrier)this.cyclicBarriers.get(name);

			if (result.parties() != parties) {
				throw new RemoteException("Cyclic barrier " + name
				    + " configured for " + result.parties() + " parties, but "
				    + parties + " were requested.");
			}
		} else {
			result = new RemoteCyclicBarrierImpl(parties);
			this.cyclicBarriers.put(name, result);
		}

		return result;
	}
}

/* __oOo__ */
