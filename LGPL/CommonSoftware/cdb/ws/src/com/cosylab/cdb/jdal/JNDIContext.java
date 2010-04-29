package com.cosylab.cdb.jdal;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.naming.CompositeName;
import javax.naming.Context;
import javax.naming.Name;
import javax.naming.NameClassPair;
import javax.naming.NameParser;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.NotContextException;

import org.omg.CORBA.ORB;

import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;

import com.cosylab.CDB.DAL;

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
public class JNDIContext implements Context {

	// statics
	protected static ORB orb;
	protected static DAL dal;

	protected final Logger logger;

	// members
	protected String name; // this name like MACI/Managers
	// all elements in this level i.e. all directories in MACI/Managers
	protected String elements;
	/**
	 * Sets the dal.
	 * @param dal The dal to set
	 */
	public static void setDal(DAL dal) {
		JNDIContext.dal = dal;
	}

	/**
	 * Sets the orb.
	 * @param orb The orb to set
	 */
	public static void setOrb(ORB orb) {
		JNDIContext.orb = orb;
	}

	/**
	 * Constructor for CDBContext.
	 */
	public JNDIContext(Logger logger) {
		this(null, null, logger);
	}

	/**
	 * Constructor for CDBContext.
	 */
	public JNDIContext(String name, String elements, Logger logger) {
		this.logger = logger;
		this.name = name;
		this.elements = elements;
	}

	/**
	 * @see Context#lookup(Name)
	 * THIS IS OLD CDB implementation
	public Object lookup(Name name) throws NamingException {
		//System.out.println("CDBContext lookup on " + this.name + " for " + name.toString());
		String nameToLookup = this.name + "/" + name;
		String recordName = nameToLookup.substring(nameToLookup.lastIndexOf('/')+1);
		// get list from the server 
		String elements = dal.list_nodes(nameToLookup);
		try {
			if (elements.indexOf(recordName + ".xml") != -1) {
				String xml = dal.get_DAO(nameToLookup);
				return new JNDIXMLContext(nameToLookup, elements, xml);
			} else {
				if (elements.length() == 0 ) { // inside a XML?
					int slashIndex = nameToLookup.lastIndexOf('/');
					String newName;
					while( slashIndex != -1 ) {
						newName = nameToLookup.substring(0,slashIndex);
						recordName = newName.substring(newName.lastIndexOf('/')+1);
						elements = dal.list_nodes(newName);
						if (elements.indexOf(recordName + ".xml") != -1) {
							String xml = dal.get_DAO(newName);
							recordName = nameToLookup.substring(slashIndex+1);
							return new JNDIXMLContext(newName, elements, xml).lookup(recordName);
						}
						slashIndex = newName.lastIndexOf('/');
					}
					throw new NamingException("No name " + nameToLookup );
				}
				return new JNDIContext(nameToLookup, elements);
			}
		} catch (CDBRecordDoesNotExistEx e) {
			// if it does not exists then it is just a context
			return new JNDIContext(nameToLookup, elements);
		} catch (CDBXMLErrorEx e) {
			AcsJCDBXMLErrorEx acse = new AcsJCDBXMLErrorEx(e);
			throw new NamingException(acse.getFilename());
		}
	}*/
    /**
     * This methos returns either a new JNDI_Context or a new JNDI_XMLContxt obj.
     */
    public Object lookup(Name name) throws NamingException {
		final String lookupName = name.toString();
    	final String fullLookupName = this.name + "/" + lookupName;

    	String daoElements = dal.list_daos(fullLookupName);
    	if (daoElements.length() == 0)
    		daoElements = null;
    	
    	// is subnode
		StringTokenizer token = new StringTokenizer(elements);
		while (token.hasMoreTokens())
			if (token.nextElement().equals(lookupName))
			{
				// is DAO?
				if (daoElements != null)
				{
					try {
						return new JNDIXMLContext(fullLookupName, dal.list_nodes(fullLookupName), dal.get_DAO(fullLookupName), logger);
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
					return new JNDIContext(fullLookupName, dal.list_nodes(fullLookupName), logger);
			}
		
		if (daoElements != null)
		{
			// lookup in DAO
			token = new StringTokenizer(daoElements);
			while (token.hasMoreTokens())
				if (token.nextElement().equals(lookupName))
				{
					try {
						return new JNDIXMLContext(fullLookupName, dal.list_nodes(fullLookupName), dal.get_DAO(fullLookupName), logger);
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

	/**
	 * @see Context#lookup(String)
	 */
	public Object lookup(String name) throws NamingException {
		return lookup(new CompositeName(name));
	}

	/**
	 * @see Context#list(Name)
	 */
	public NamingEnumeration list(Name name) throws NamingException {
		//System.out.println("CDBContext list on " + this.name + " for name " + name.toString() + "->" + elements);
		if (name.isEmpty()) {
			return new JNDIElementEnumeration(new StringTokenizer(elements));
		}

		Object target = lookup(name);
		if (target instanceof Context) {
			return ((Context) target).list("");
		}
		throw new NotContextException(name + " unable to list");
	}

	/**
	 * @see Context#list(String)
	 */
	public NamingEnumeration list(String name) throws NamingException {
		return list(new CompositeName(name));
	}

	/**
	 * @see Context#bind(Name, Object)
	 */
	public void bind(Name name, Object obj) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#bind(String, Object)
	 */
	public void bind(String name, Object obj) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#rebind(Name, Object)
	 */
	public void rebind(Name name, Object obj) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#rebind(String, Object)
	 */
	public void rebind(String name, Object obj) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#unbind(Name)
	 */
	public void unbind(Name name) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#unbind(String)
	 */
	public void unbind(String name) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#rename(Name, Name)
	 */
	public void rename(Name oldName, Name newName) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#rename(String, String)
	 */
	public void rename(String oldName, String newName) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#listBindings(Name)
	 */
	public NamingEnumeration listBindings(Name name) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#listBindings(String)
	 */
	public NamingEnumeration listBindings(String name) throws NamingException {
		throw new NamingException("Not implemented");
	}

	/**
	 * @see Context#destroySubcontext(Name)
	 */
	public void destroySubcontext(Name name) throws NamingException {
	}

	/**
	 * @see Context#destroySubcontext(String)
	 */
	public void destroySubcontext(String name) throws NamingException {
	}

	/**
	 * @see Context#createSubcontext(Name)
	 */
	public Context createSubcontext(Name name) throws NamingException {
		return null;
	}

	/**
	 * @see Context#createSubcontext(String)
	 */
	public Context createSubcontext(String name) throws NamingException {
		return null;
	}

	/**
	 * @see Context#lookupLink(Name)
	 */
	public Object lookupLink(Name name) throws NamingException {
		return null;
	}

	/**
	 * @see Context#lookupLink(String)
	 */
	public Object lookupLink(String name) throws NamingException {
		return null;
	}

	/**
	 * @see Context#getNameParser(Name)
	 */
	public NameParser getNameParser(Name name) throws NamingException {
		return null;
	}

	/**
	 * @see Context#getNameParser(String)
	 */
	public NameParser getNameParser(String name) throws NamingException {
		return null;
	}

	/**
	 * @see Context#composeName(Name, Name)
	 */
	public Name composeName(Name name, Name prefix) throws NamingException {
		return null;
	}

	/**
	 * @see Context#composeName(String, String)
	 */
	public String composeName(String name, String prefix) throws NamingException {
		return null;
	}

	/**
	 * @see Context#addToEnvironment(String, Object)
	 */
	public Object addToEnvironment(String propName, Object propVal) throws NamingException {
		return null;
	}

	/**
	 * @see Context#removeFromEnvironment(String)
	 */
	public Object removeFromEnvironment(String propName) throws NamingException {
		return null;
	}

	/**
	 * @see Context#getEnvironment()
	 */
	public Hashtable getEnvironment() throws NamingException {
		return null;
	}

	/**
	 * @see Context#close()
	 */
	public void close() throws NamingException {
	}

	/**
	 * @see Context#getNameInNamespace()
	 */
	public String getNameInNamespace() throws NamingException {
		return null;
	}

	/**
	 * @author dragan
	 *
	 * To change this generated comment go to 
	 * Window>Preferences>Java>Code Generation>Code Template
	 */

	private class JNDIElementEnumeration implements NamingEnumeration {

		// members
		protected Enumeration names;

		/**
		 * Constructor for CDBNamesList.
		 */
		public JNDIElementEnumeration(Enumeration names) {
			super();
			this.names = names;
		}

		/**
		 * @see NamingEnumeration#next()
		 */
		public Object next() throws NamingException {
			String name = (String) names.nextElement();
			// here are elements are a context
			return new NameClassPair(name, Context.class.getName());
		}

		/**
		 * @see NamingEnumeration#hasMore()
		 */
		public boolean hasMore() throws NamingException {
			return names.hasMoreElements();
		}

		/**
		 * @see NamingEnumeration#close()
		 */
		public void close() throws NamingException {
		}

		/**
		 * @see Enumeration#hasMoreElements()
		 */
		public boolean hasMoreElements() {
			try {
				return hasMore();
			} catch (NamingException e) {
				return false;
			}
		}

		/**
		 * @see Enumeration#nextElement()
		 */
		public Object nextElement() {
			try {
				return next();
			} catch (NamingException e) {
				throw new NoSuchElementException(e.toString());
			}
		}

	}
}
