/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 */
/*
 * Created on Feb 6, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.cdb.jdal;

import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;

import com.cosylab.CDB.DALChangeListener;
import com.cosylab.CDB.DAO;
import com.cosylab.CDB.WJDALOperations;

import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBXMLErrorEx;

/**
 * @author dragan
 *
 *This is base class for WDAL where we delegate all base methods to JDAL
 *
 */
abstract public class WDALBaseImpl implements WJDALOperations, Recoverer {
	protected final DALImpl dalImpl;
	protected final Logger logger;
	
	/**
	 * @param args
	 * @param orb_val
	 * @param poa_val
	 */
	public WDALBaseImpl(String[] args, ORB orb_val, POA poa_val, Logger logger) {
		dalImpl = new DALImpl(args, orb_val, poa_val, logger);
		this.logger = logger;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.JDALOperations#clear_cache_all()
	 */
	public void clear_cache_all() {
		dalImpl.clear_cache_all();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.JDALOperations#clear_cache(java.lang.String)
	 */
	public void clear_cache(String curl) {
		dalImpl.clear_cache(curl);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#add_change_listener(com.cosylab.CDB.DALChangeListener)
	 */
	public int add_change_listener(DALChangeListener listener) {
		return dalImpl.add_change_listener(listener);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#get_DAO_servant(java.lang.String)
	 */
	public DAO get_DAO_Servant(String curl) throws CDBXMLErrorEx, CDBRecordDoesNotExistEx {
		return dalImpl.get_DAO_Servant(curl);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#get_DAO(java.lang.String)
	 */
	public String get_DAO(String curl) throws CDBXMLErrorEx, CDBRecordDoesNotExistEx {
		return dalImpl.get_DAO(curl);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#list_nodes(java.lang.String)
	 */
	public String list_nodes(String name) {
		return dalImpl.list_nodes(name);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#listen_for_changes(java.lang.String, int)
	 */
	public void listen_for_changes(String curl, int listenerID) {
		dalImpl.listen_for_changes(curl, listenerID);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#remove_change_listener(int)
	 */
	public void remove_change_listener(int listenerID) {
		dalImpl.remove_change_listener(listenerID);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#shutdown()
	 */
	public void shutdown() {
		dalImpl.shutdown();
	}
	
	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.Recoverer#recoverClients()
	 */
	public void recoverClients() {
		dalImpl.recoverClients();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#configuration_name()
	 */
	public String configuration_name() {
		return dalImpl.configuration_name();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#list_daos(java.lang.String)
	 */
	public String list_daos(String name) {
		return dalImpl.list_daos(name);
	}
	
	
	/**
	 * Shuts down this instance, without destroying global resources.
	 * Use this when you run a WDAL embedded in other code, and only want to shut down the wdal
	 * without side effects on your ORB, logging etc.
	 */
	public void shutdownEmbeddedWDALImpl() {
		dalImpl.shutdownEmbeddedDALImpl();
	}

}
