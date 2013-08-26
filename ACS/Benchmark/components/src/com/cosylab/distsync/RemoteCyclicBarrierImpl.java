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

import java.util.concurrent.CyclicBarrier;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;


/**
 * DOCUMENT ME!
 *
 * @author $Author: hsommer $
 * @version $Revision: 1.2 $
 */
public class RemoteCyclicBarrierImpl extends UnicastRemoteObject
	implements RemoteCyclicBarrier
{
	private CyclicBarrier cyclicBarrier;

	/**
	 * DOCUMENT ME!
	 *
	 * @param parties
	 *
	 * @throws RemoteException DOCUMENT ME!
	 */
	public RemoteCyclicBarrierImpl(int parties) throws RemoteException
	{
		super();
		this.cyclicBarrier = new CyclicBarrier(parties);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.distsync.RemoteCyclicBarrier#barrier()
	 */
	public int barrier() throws RemoteException
	{
		try {
			return this.cyclicBarrier.await();
		} catch (Exception e) {
			throw new RemoteException("Exception in RemoteCyclicBarrier.barrier()",
			    e);
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.distsync.RemoteCyclicBarrier#getParties()
	 */
	public int parties() throws RemoteException
	{
		return this.cyclicBarrier.getParties();
	}
}

/* __oOo__ */
