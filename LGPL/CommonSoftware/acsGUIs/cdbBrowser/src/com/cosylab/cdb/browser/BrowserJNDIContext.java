/***************************************************************************
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
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
*    MA 02111-1307  USA
*/
package com.cosylab.cdb.browser;

import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.naming.Name;
import javax.naming.NamingException;

import org.omg.CORBA.ORB;

import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;

import com.cosylab.CDB.DAL;


public class BrowserJNDIContext extends com.cosylab.cdb.jdal.JNDIContext {

    /**
     * Sets the dal.
     * @param dal The dal to set
     */
    public static void setDal(DAL dal) {
	BrowserJNDIContext.dal = dal;
	CDBLogic.setDAL(dal);
    }
    
    /**
     * Sets the orb.
     * @param orb The orb to set
     */
    public static void setOrb(ORB orb) {
	BrowserJNDIContext.orb = orb;
    }
    
    /**
     * Constructor for CDBContext.
     */
    public BrowserJNDIContext(Logger logger) {
	super(logger);
    }
    
    /**
     *  Takes a String representation of a CDBTree level as parameter.
     *  When clicking on one of the Nodes of this level the method 'lookup(Name name)' is called.
     *  The method will create another level by returning the appropriate constructor
     */
    public BrowserJNDIContext(String name, String elements, Logger logger) {
    	super(name, elements, logger);
    	//System.out.println(elements);
    }
    
    /**
     * This methos returns either a new JNDI_Context or a new JNDI_XMLContxt obj.
     */
    public Object lookup(Name name) throws NamingException {
		final String lookupName = name.toString();
    	final String fullLookupName = this.name + "/" + lookupName;

    	String daoElements = dal.list_daos(fullLookupName);
    	if (daoElements.length() == 0)
    		daoElements = null;
   
    	// @todo TODO this creates DAO and wastes some of resources... DAL method to get resources would be nice
    	boolean hasAttributes = false;
    	try {
    		// NOTE check only if needed
    		if (daoElements == null)
    			hasAttributes = dal.get_DAO_Servant(fullLookupName).get_string("_attributes").trim().length() > 0;
		} catch (Throwable th) {
			// noop
		}
		CDBLogic.setKey(fullLookupName);

    	// is subnode
		StringTokenizer token = new StringTokenizer(elements);
		while (token.hasMoreTokens())
			if (token.nextElement().equals(lookupName))
			{
				// is DAO?
				if (daoElements != null || hasAttributes)
				{
					try {
						return new BrowserJNDIXMLContext(fullLookupName, dal.list_nodes(fullLookupName), dal.get_DAO(fullLookupName), logger);
					} catch (CDBXMLErrorEx th) {
						AcsJCDBXMLErrorEx jex = AcsJCDBXMLErrorEx.fromCDBXMLErrorEx(th);
						NamingException ex2 = new NamingException(jex.getFilename() + ": " + jex.getErrorString());
						ex2.setRootCause(jex);
						throw ex2;
					} catch (Throwable th) {
						throw new NamingException("Failed to retrieve DAO: " + fullLookupName);
					}
				}
				else
					return new BrowserJNDIContext(fullLookupName, dal.list_nodes(fullLookupName), logger);
			}
		
		if (daoElements != null)
		{
			// lookup in DAO
			token = new StringTokenizer(daoElements);
			while (token.hasMoreTokens())
				if (token.nextElement().equals(lookupName))
				{
					try {
						return new BrowserJNDIXMLContext(fullLookupName, dal.list_nodes(fullLookupName), dal.get_DAO(fullLookupName), logger);
					} catch (CDBXMLErrorEx th) {
						AcsJCDBXMLErrorEx jex = AcsJCDBXMLErrorEx.fromCDBXMLErrorEx(th);
						NamingException ex2 = new NamingException(jex.getFilename() + ": " + jex.getErrorString());
						ex2.setRootCause(jex);
						throw ex2;
					} catch (Throwable th) {
						throw new NamingException("Failed to retrieve DAO: " + fullLookupName);
					}
				}
		}
		
		// not found
		throw new NamingException("No name " + fullLookupName);
    }
}
