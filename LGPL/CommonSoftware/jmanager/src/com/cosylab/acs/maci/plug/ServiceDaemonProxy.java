/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import alma.acsdaemon.ServicesDaemonHelper;

import com.cosylab.acs.maci.RemoteException;
import com.cosylab.acs.maci.ServiceDaemon;


/**
 * CORBA Deamon Proxy.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
/**
 * @author msekoranja
 *
 */
public class ServiceDaemonProxy extends CORBAReferenceSerializator implements ServiceDaemon, Serializable
{

	/**
	 * Serial version UID.
	 */
	private static final long serialVersionUID = -5090533056497509227L;

	/**
	 * CORBA reference.
	 */
	protected alma.acsdaemon.ServicesDaemon daemon;

	/**
	 * Constructor for ServiceDaemonProxy.
	 * @param	daemon	CORBA reference, non-<code>null</code>.
	 */
	public ServiceDaemonProxy(alma.acsdaemon.ServicesDaemon daemon)
	{
		this.daemon = daemon;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.ServiceDaemon#setManagerReference(java.lang.String)
	 */
	public void setManagerReference(String reference) throws RemoteException {
		try
		{
			daemon.set_manager_reference((short)alma.acs.util.ACSPorts.getBasePort(), reference);
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
	public alma.acsdaemon.ServicesDaemon getServiceDaemon()
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
			daemon = ServicesDaemonHelper.narrow(deserialize((String)stream.readObject()));
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
		sbuff.append("ServiceDaemonProxy = { ");
		sbuff.append("daemon = '");
		sbuff.append(daemon);
		sbuff.append("' }");
		return new String(sbuff);
	}

	/**
	 * @see java.lang.Object#equals(Object)
	 */
	public boolean equals(Object obj)
	{
		if (daemon == null)
			return (obj == null);
		else if (obj instanceof alma.acsdaemon.ServicesDaemon)
		{
			try
			{
				return daemon._is_equivalent((alma.acsdaemon.ServicesDaemon)obj);
			}
			catch (Exception ex)
			{
				return false;
			}
		}
                else if (obj instanceof ServiceDaemonProxy)
                {
                        try
                        {
                                return daemon._is_equivalent(((ServiceDaemonProxy)obj).getServiceDaemon());
                        }
                        catch (Exception ex)
                        {
                                return false;
                        }
                }
		else
			return false;
	}

}
