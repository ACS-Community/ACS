/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import org.omg.CORBA.InterfaceDef;
import org.omg.CORBA.InterfaceDefHelper;
import org.omg.CORBA.Object;
import org.omg.CORBA.ObjectHelper;
import org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.pluggable.RemoteException;

import com.cosylab.acs.maci.Component;

/**
 * CORBA Component Proxy.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ComponentProxy extends CORBAReferenceSerializator implements Component, Identifiable, Serializable
{

	/**
	 * Default Component name.
	 */
	private static final String defaultName = "<unnamed>";

	/**
	 * Identifier.
	 */
	private Identifier id = null;

	/**
	 * Component name, can be <code>null</code>.
	 */
	private String name = defaultName;

	/**
	 * non-<code>null</code> if Component implements <code>maci::Constructable</code>.
	 */
	//private Constructable constructable;
	
	/**
	 * List of implemented interfaces.
	 */
	private String[] interfaces;

	/**
	 * CORBA reference.
	 */
	private Object reference;

	/**
	 * Construct an implementaiton of Component.
	 * 
	 * @param	reference	CORBA reference of Component, non-<code>null</code>
	 */
	public ComponentProxy(Object reference)
	{
		this (null, reference);
	}

	/**
	 * Construct an implementaiton of Component.
	 * 
	 * @param	name		name of the Component.
	 * @param	reference	CORBA reference of Component, non-<code>null</code>
	 */
	public ComponentProxy(String name, Object reference)
	{
		assert (reference != null);
		
		this.name = name;
		this.reference = reference;

		checkConstructable();
	}


	/**
	 * Check if Component implements <code>maci::Constructable</code> interface.
	 */
	private void checkConstructable()
	{
		/*
		if (reference != null)
		{
			try
			{
				if (reference._is_a(ConstructableHelper.id()))
					constructable = ConstructableHelper.narrow(reference);
			}
			catch (Exception ex) {};
		}
		*/
	}

	/**
	 * @see com.cosylab.acs.maci.Component#doesImplement(String)
	 */
	public boolean doesImplement(String type)
	{
		if (reference != null)
		{
			try
			{
				return reference._is_a(type);
			}
			catch (Exception ex) {};
		}

		return false;
	}

	/**
	 * Returns list of implemented interfaces.
	 * @param	list of implemented interfaces.
	 */
	private String[] resolveImplementedInterfaces()
	{
		String[] interfaces = null;
		
		try
		{
			InterfaceDef interfaceDef = InterfaceDefHelper.narrow(reference._get_interface_def());
			if (interfaceDef!=null)
			{
				FullInterfaceDescription fid = interfaceDef.describe_interface();
				if (fid != null)
					interfaces = fid.base_interfaces;
			}
		}
		catch (Exception ex) {};
		
		if (interfaces == null)
			return new String[] { ObjectHelper.id() };
		else
			return interfaces;
	}

	/**
	 * @see com.cosylab.acs.maci.Component#construct()
	 */
	public void construct() throws RemoteException
	{
		/*
		try
		{
			if (constructable != null)
				constructable.construct();
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to invoke 'construct()' method.", ex);
			re.caughtIn(this, "construct");
			throw re;
		}
		*/
	}

	/**
	 * @see com.cosylab.acs.maci.Component#destruct()
	 */
	public void destruct() throws RemoteException
	{
		/*
		try
		{
			if (constructable != null)
				constructable.destruct();
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to invoke 'destruct()' method.", ex);
			re.caughtIn(this, "destruct");
			throw re;
		}
		*/
	}

	/**
	 * @see com.cosylab.acs.maci.Component#implementedInterfaces()
	 */
	public String[] implementedInterfaces()
	{
		if (interfaces == null)
			resolveImplementedInterfaces();
			
		return interfaces;
	}

	/**
	 * Returns the object.
	 * @return Object
	 */
	public java.lang.Object getObject()
	{
		return reference;
	}

    /**
     * Save the state of the <tt>ComponentProxy</tt> instance to a stream (that
     * is, serialize it).
     */
    private void writeObject(ObjectOutputStream stream)
        throws IOException
    {
        stream.writeObject(name);
        stream.writeObject(interfaces);
        stream.writeObject(serialize(reference));
    }



    /**
     * Reconstitute the <tt>ComponentProxy</tt> instance from a stream (that is,
     * deserialize it).
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException
    {
        name = (String)stream.readObject();
        interfaces = (String[])stream.readObject();
        reference = deserialize((String)stream.readObject());

		checkConstructable();
    }

 	/*****************************************************************************/
	/*************************** [ Abeans methods ] ******************************/
	/*****************************************************************************/

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null)
			id = new IdentifierSupport(name, name, Identifier.PLUG);
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return false;
	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ComponenyProxy = { ");
		sbuff.append("name = '");
		sbuff.append(name);
		sbuff.append("', reference = '");
		sbuff.append(reference);
		//sbuff.append("', constructable = '");
		//sbuff.append(constructable);
		sbuff.append("' }");
		return new String(sbuff);
	}



}
