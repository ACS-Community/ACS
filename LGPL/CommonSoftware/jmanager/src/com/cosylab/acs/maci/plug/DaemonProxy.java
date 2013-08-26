/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import com.cosylab.acs.maci.Daemon;
import com.cosylab.acs.maci.RemoteException;

import alma.acsdaemon.ContainerDaemonHelper;


/**
 * CORBA Deamon Proxy.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class DaemonProxy extends CORBAReferenceSerializator implements Daemon, Serializable
{

	/**
	 * Serial version UID.
	 */
	private static final long serialVersionUID = -5090533056497509226L;

	/**
	 * CORBA reference.
	 */
	protected alma.acsdaemon.ContainerDaemon daemon;

	/**
	 * Constructor for DaemonProxy.
	 * @param	daemon	CORBA reference, non-<code>null</code>.
	 */
	public DaemonProxy(alma.acsdaemon.ContainerDaemon daemon)
	{
		this.daemon = daemon;
	}

	/**
	 * @see com.cosylab.acs.maci.Daemon#startContainer(java.lang.String, java.lang.String, short, java.lang.String)
	 */
	public void startContainer(String containerType, String containerName,
			short instanceNumber, String flags) throws RemoteException {
		try
		{
			// @TODO: get typeModifiers from the CDB, 
			// see http://www.eso.org/projects/alma/develop/acs/OnlineDocs/ACS_docs/schemas/urn_schemas-cosylab-com_Container_1.0/complexType/DeployInfo.html
			String[] typeModifiers = new String[0];
			daemon.start_container(containerType, containerName, instanceNumber, typeModifiers, flags);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'start_container()' method.", ex);
			throw re;
		}
	}

	/**
	 * Returns the daemon.
	 * @return alma.acsdaemon.Daemon
	 */
	public alma.acsdaemon.ContainerDaemon getDaemon()
	{
		return daemon;
	}

    /**
     * Save the state of the <tt>ContainerProxy</tt> instance to a stream (that
     * is, serialize it).
     */
    private void writeObject(ObjectOutputStream stream)
        throws IOException
    {
        stream.writeObject(serialize(daemon));
    }

    /**
     * Reconstitute the <tt>ContainerProxy</tt> instance from a stream (that is,
     * deserialize it).
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException
    {
		try {
			daemon = ContainerDaemonHelper.narrow(deserialize((String)stream.readObject()));
		}
		catch (Exception e) {
			// silent here and set reference to null.
			// An method after deserialization should clean such invalid reference
			daemon = null;
		}
    }

	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("DaemonProxy = { ");
		sbuff.append("daemon = '");
		sbuff.append(daemon);
		sbuff.append("' }");
		return new String(sbuff);
	}

	/**
	 * @see java.lang.Object#equals(Object)
	 */
	public boolean equals(Object obj) {
		if (daemon == null)
			return (obj == null);
		else if (obj instanceof alma.acsdaemon.ContainerDaemon) {
			try {
				//return daemon.toString().equals(((alma.acsdaemon.ContainerDaemon) obj).toString());
				return daemon._is_equivalent((alma.acsdaemon.ContainerDaemon) obj);
			} catch (Exception ex) {
				return false;
			}
		} else if (obj instanceof DaemonProxy) {
			try {
				//return daemon.toString().equals(((DaemonProxy) obj).getDaemon().toString());
				return daemon._is_equivalent(((DaemonProxy) obj).getDaemon());
			} catch (Exception ex) {
				return false;
			}
		} else
			return false;
	}

}
