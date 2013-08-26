package com.cosylab.cdb.jdal;

import java.util.Hashtable;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.NamingException;
import javax.naming.spi.InitialContextFactory;

import org.omg.CORBA.ORB;

import alma.acs.logging.ClientLogManager;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;

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
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class JNDIContextFactory implements InitialContextFactory {

	/**
	 * @see InitialContextFactory#getInitialContext(Hashtable)
	 */
	public Context getInitialContext(Hashtable environment) throws NamingException {

		String strIOR = (String) environment.get(Context.PROVIDER_URL);
		if (strIOR == null)
			throw new NamingException("There is no " + Context.PROVIDER_URL + " property set!");
		// try to get the server
		// create and initialize the ORB
		String[] argv = {};
		ORB orb = ORB.init(argv, null);

		DAL dal = DALHelper.narrow(orb.string_to_object(strIOR));

		JNDIContext.setOrb(orb);
		JNDIContext.setDal(dal);

		//System.out.println( "Returning CDBContext" );
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(Server.CDB_LOGGER_NAME, true);
		return new JNDIContext("", dal.list_nodes(""), logger);

	}

}
