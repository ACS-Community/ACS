/*
 * @@COPYRIGHT@@
 */

package com.cosylab.cdb.client;

import java.util.Iterator;
import java.util.LinkedList;

import com.cosylab.CDB.DAOOperations;
import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.WrongCDBDataTypeEx;

/**
 * CDB DAO proxy.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class DAOProxy implements DAOOperations
{
	/**
	 * Path part of the URI reference directly mapping to CDB CURL
	 */
	protected String curl = null;

	/**
	 * DAO CORBA reference.
	 */
	protected DAOOperations dao = null;

	/**
	 * Connection listeners.
	 */
	protected LinkedList listeners = new LinkedList();

	/**
	 * Constructor for DAOProxy.
	 * 
	 * @param curl		CDB CURL of the DAO
	 * @param dao		CORBA reference of the DAO
	 */
	public DAOProxy(String curl, DAOOperations dao)
	{
		assert (curl != null);
		
		this.curl = curl;
		this.dao = dao;
	}

	/**
	 * Constructor for DAOProxy.
	 * 
	 * @param curl		CDB CURL of the DAO
	 */
	public DAOProxy(String curl)
	{
		assert (curl != null);
		this.curl = curl;
	}

	/**
	 * Initializer of DAOProxy.
	 * 
	 * @param dao		CORBA reference of the DAO
	 */
	public void initialize(DAOOperations dao)
	{
		if (dao == null)
			destroy();
		else
		{
			this.dao = dao;
			
			// notify
			synchronized (listeners)
			{
				Iterator iter = listeners.iterator();
				while (iter.hasNext())
				{
					try
					{
						((DAOProxyConnectionListener)iter.next()).connected(this);
					}
					catch (Throwable th) 
					{
						th.printStackTrace();
					}
				}
			}
		}
	}
	
	/**
	 * Returns the CDB CURL of the DAO.
	 * @return 	CDB CURL of the DAO
	 */
	public String getCURL()
	{
		return curl;
	}

	/**
	 * Returns the CORBA reference of the DAO.
	 * @return		CORBA reference of the DAO
	 */
	public DAOOperations getDAO()
	{
		return dao;
	}

	/**
	 * @see com.cosylab.CDB.DAOOperations#get_double_seq(String)
	 */
	public double[] get_double_seq(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx
	{
		return dao.get_double_seq(propertyName);
	}

	/**
	 * @see com.cosylab.CDB.DAOOperations#get_double(String)
	 */
	public double get_double(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx
	{
		return dao.get_double(propertyName);
	}

	/**
	 * @see com.cosylab.CDB.DAOOperations#get_field_data(String)
	 */
	public String get_field_data(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx
	{
		return dao.get_field_data(propertyName);
	}

	/**
	 * @see com.cosylab.CDB.DAOOperations#get_long_seq(String)
	 */
	public int[] get_long_seq(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx
	{
		return dao.get_long_seq(propertyName);
	}

	/**
	 * @see com.cosylab.CDB.DAOOperations#get_long(String)
	 */
	public int get_long(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx
	{
		return dao.get_long(propertyName);
	}

	/**
	 * @see com.cosylab.CDB.DAOOperations#get_string_seq(String)
	 */
	public String[] get_string_seq(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx
	{
		return dao.get_string_seq(propertyName);
	}

	/**
	 * @see com.cosylab.CDB.DAOOperations#get_string(String)
	 */
	public String get_string(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx
	{
		return dao.get_string(propertyName);
	}

	/**
	 * @see com.cosylab.CDB.DAOOperations#destroy()
	 */
	public void destroy()
	{
		dao.destroy();
		dao = null;
		
		// notify
		synchronized (listeners)
		{
			Iterator iter = listeners.iterator();
			while (iter.hasNext())
			{
				try
				{
					((DAOProxyConnectionListener)iter.next()).disconnected(this);
				}
				catch (Throwable th) 
				{
					th.printStackTrace();
				}
			}
		}
	}

	/**
	 * Register new connection listener.
	 * @param listener	listener to register.
	 */
	public void addConnectionListener(DAOProxyConnectionListener listener)
	{
		synchronized (listeners) {
			if (!listeners.contains(listener))
				listeners.add(listener);
		}
	}

	/**
	 * Unregister new connection listener.
	 * @param listener	listener to unregister.
	 */
	public void removeConnectionListener(DAOProxyConnectionListener listener)
	{
		synchronized (listeners) {
				listeners.remove(listener);
		}
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "Proxy of " + curl;
	}

	// this is not supported by pure DAO (via remote DAO)
	private String elementName = null;

	/**
	 * @return the elementName
	 */
	public String getElementName() {
		return elementName;
	}

	/**
	 * @param elementName the elementName to set
	 */
	public void setElementName(String elementName) {
		this.elementName = elementName;
	}
	
}
