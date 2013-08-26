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

import java.util.Hashtable;
import java.util.logging.Logger;
import java.util.regex.PatternSyntaxException;

import javax.naming.Context;
import javax.naming.NamingException;
import javax.naming.spi.InitialContextFactory;

import org.omg.CORBA.ORB;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;

import alma.acs.logging.ClientLogManager;

public class BrowserJNDIContextFactory implements InitialContextFactory {
    
    /**
     * An initial context factory (JNDI_ContextFactory)  must implement the InitialContextFactory interface, which
     * provides a method for creating instances of initial context that implement the Context interface
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
	
	BrowserJNDIContext.setOrb(orb);
	BrowserJNDIContext.setDal(dal);
	
	String rootElements = dal.list_nodes("");
	//Returns the first level of the CDBTree (possible: "alma MACI schema CVS")
	try{
	    //remove 'CVS' and 'schema' from Root
	    rootElements = rootElements.replaceAll("CVS","");
	    rootElements = rootElements.replaceAll("schemas","");
	}
	catch(PatternSyntaxException e){}
	catch(NullPointerException e){}
	
	Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("cdbBrowser", false);
	return new BrowserJNDIContext("", rootElements, logger);     //JNDI_Context implements Context	    
    }    
}
