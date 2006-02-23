/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.cdb.dal;

import com.cosylab.CDB.DALOperations;
import com.cosylab.CDB.DAOOperations;

import abeans.pluggable.Proxy;

/**
 * CDB DAO proxy
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class DAOProxy implements Proxy
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
	 * DAL CORBA reference.
	 */
	protected DALOperations dal = null;

	/**
	 * Constructor for DAOProxy.
	 * 
	 * @param curl		CDB CURL of the DAO
	 * @param dao		CORBA reference of the DAO
	 * @param dal		CORBA reference of the DAL
	 */
	public DAOProxy(DALOperations dal, String curl, DAOOperations dao)
	{
		assert (curl != null);
		
		this.curl = curl;
		this.dao = dao;
		this.dal = dal;
	}

	/**
	 * Returns the CDB CURL of the DAO.
	 * @return 	CDB CURL of the DAO
	 */
	public String getCurl()
	{
		return curl;
	}

	/**
	 * Returns the CORBA reference of the DAO.
	 * @return		CORBA reference of the DAO
	 */
	public DAOOperations getDao()
	{
		return dao;
	}

	/**
	 * Returns the CORBA reference of the DAL.
	 * @return		CORBA reference of the DAL
	 */
	public DALOperations getDal()
	{
		return dal;
	}

}
