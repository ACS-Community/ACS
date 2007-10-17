/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.io.IOException;
import java.io.ObjectInputStream;

import org.omg.CORBA.Object;

import com.cosylab.acs.maci.SynchronousAdministrator;

/**
 * CORBA SynchronousAdministrator Proxy.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class SynchronousAdministratorProxy extends AdministratorProxy implements SynchronousAdministrator
{

	/**
	 * Serial version ID.
	 */
	private static final long serialVersionUID = -5434657815391774479L;

	/**
	 * Constructor for SynchronousAdministratorProxy.
	 * @param	administrator	CORBA reference, non-<code>null</code>.
	 */
	public SynchronousAdministratorProxy(si.ijs.maci.SynchronousAdministrator administrator)
	{
		super(administrator);
	}

	/**
     * Reconstitute the <tt>ContainerProxy</tt> instance from a stream (that is,
     * deserialize it).
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException
    {
		try {
			administrator = si.ijs.maci.SynchronousAdministratorHelper.narrow(deserialize((String)stream.readObject()));
		}
		catch (Exception e) {
			// silent here and set reference to null.
			// An method after deserialization should clean such invalid reference
			administrator = null;
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
		sbuff.append("SynchronousAdministratorProxy = { ");
		sbuff.append("administrator = '");
		sbuff.append(administrator);
		sbuff.append("' }");
		return new String(sbuff);
	}

	/**
	 * @see java.lang.Object#equals(Object)
	 */
	public boolean equals(Object obj)
	{
		if (administrator == null)
			return (obj == null);
		else if (obj instanceof si.ijs.maci.SynchronousAdministrator)
		{
			try
			{
				return administrator._is_equivalent((si.ijs.maci.SynchronousAdministrator)obj);
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
