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

import javax.naming.Name;
import javax.naming.NamingException;
import org.omg.CORBA.ORB;
// The package containing the CORBA stubs.
import com.cosylab.CDB.*;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBXMLErrorEx;
import java.util.regex.PatternSyntaxException;


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
    public BrowserJNDIContext() {
	super();
    }
    
    /**
     *  Takes a String representation of a CDBTree level as parameter.
     *  When clicking on one of the Nodes of this level the method 'lookup(Name name)' is called.
     *  The method will create another level by returning the appropriate constructor
     */
    public BrowserJNDIContext(String name,String elements) {
	super(name, elements);
	//System.out.println(elements);
    }
    
    /**
     * This methos returns either a new JNDI_Context or a new JNDI_XMLContxt obj.
     */
    public Object lookup(Name name) throws NamingException {	

	//full path of the last Node selected           (/alma/LAMP1)
	String nameToLookup = this.name + "/" + name;
	//name of the last Node selected.. without path (LAMP1)
	String recordName = nameToLookup.substring(nameToLookup.lastIndexOf('/')+1);

	CDBLogic.setKey(nameToLookup);
	
	//get elements (SUBNODES) of the selected node
	String elements = dal.list_nodes(nameToLookup);
	//Remove CVS from list f elements
	try{
	    elements = elements.replaceAll("CVS","");
	}catch(PatternSyntaxException e){}
	catch(NullPointerException e){}
	// put elements in alphabetical order
	//elements = CDBLogic.abcOrder(elements);*********************************

	//System.out.println(elements);

	//PASS THE ELEMENTS OF THE NEXT LEVEL AS A PARAMETER TO THE  APPROPRIATE CONSTRUCTOR
	try {
	    //CASE 1 *********************************************************
	    //check if elements contains an xml file: if yes -> return JNDI_XMLContext(nameToLookup, elements, xml);
	    if (elements.indexOf(recordName + ".xml") != -1) {
		String xml = dal.get_DAO(nameToLookup);
		Browser.getInstance().display("==> Returning XML record for: " + nameToLookup, true);
		return new BrowserJNDIXMLContext(nameToLookup, elements, xml);
	    } else {


		//CASE 2 *********************************************************
		//if current 'Node' contains no elements -> check the parent 'node'
		if (elements.length() == 0 ) { // inside a XML?

		    int slashIndex = nameToLookup.lastIndexOf('/');
		    String newName;
		    while( slashIndex != -1 ) {
			newName = nameToLookup.substring(0,slashIndex);             //full path of the parent 'node'
			recordName = newName.substring(newName.lastIndexOf('/')+1); //name of the parent
			elements = dal.list_nodes(newName);                         //get elements of the parent
			if (elements.indexOf(recordName + ".xml") != -1) {          //element is xml file??
			    String xml = dal.get_DAO(newName);
			    Browser.getInstance().display("==> Returning XML record for: " + newName, true); 
			    recordName = nameToLookup.substring(slashIndex+1);      //get the selected node
			    //System.out.println("2) return a BrowserJNDIXMLContext instance");
			    return new BrowserJNDIXMLContext(newName, elements, xml).lookup(recordName);
			}
			slashIndex = newName.lastIndexOf('/');
		    }
		    throw new NamingException("No name " + nameToLookup );
		}


		//CASE 3 *********************************************************
		//Elements of current node do not contain xml file  -> return JNDI_Context(nameToLookup, elements)
		return new BrowserJNDIContext(nameToLookup, elements);
	    }
	} catch (CDBRecordDoesNotExistEx e) {


	    //CASE 4 *********************************************************
	    //repeat case 3 (statement never reached???)
	    return new BrowserJNDIContext(nameToLookup, elements);
	} catch (CDBXMLErrorEx e) {
		throw new NamingException();
		//  throw new NamingException(e);
	}
    }
}
