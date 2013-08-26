/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.container.corba;

import java.util.ArrayList;
import java.util.Properties;

import alma.acs.util.CmdLineArgs;
import alma.acs.util.CmdLineRegisteredOption;
import alma.acs.util.ACSPorts;


/**
 * Configures the CORBA ORB.
 * Subclasses will encapsulate settings that are particular to certain ORBs. 
 * <p>
 * Created on 17-Oct-2002 10:50:46
 * @author hsommer
 */
public abstract class OrbConfigurator
{
	protected static boolean debug = false;
	
	private CmdLineArgs m_cmdArgs;
	
	private CmdLineRegisteredOption[] m_opts;

	public static final String ORB_CLASS_KEY = "org.omg.CORBA.ORBClass";
	public static final String ORB_SINGLETON_CLASS_KEY = "org.omg.CORBA.ORBSingletonClass";
	public static final int ORB_DEFAULT_PORT = ACSPorts.getBasePort()*100 + 3000 + 51;
	private int m_port;


	protected OrbConfigurator()
	{
		m_opts = _declareOptions();
		if (m_opts == null)
		{
			m_opts = new CmdLineRegisteredOption[0];
		}
		
		// init parser
		m_cmdArgs = new CmdLineArgs();
		for (int i = 0; i < m_opts.length; i++)
		{
			m_cmdArgs.registerOption(m_opts[i]);
		}
	}

	public static OrbConfigurator getOrbConfigurator()
	{
		// todo: make the choice of ORB-specific subclass configurable
		
		return new JacOrbConfigurator();
//		return new OrbacusConfigurator();
//		return new JavaIDLConfigurator();
	}
	
	public static void setDebug(boolean debug) {
		OrbConfigurator.debug = debug;
	}
	
	/**
	 * Declares command line options that will be recognized later.
	 *  
	 * @return CmdLineRegisteredOption[]
	 */
	protected abstract CmdLineRegisteredOption[] _declareOptions();
	

	/**
	 * Sets options for the ORB as they would occur in a command line call. 
	 * If any of the options in <code>args</code> have been set already, the value will be replaced.
	 */
	public void setOptions(String[] args)
	{
		m_cmdArgs.parseArgs(args);
	}
	
	/**
	 * Sets the standard option "-ORBInitRef", see Corba spec 4.5.3.2.
	 */
	public void setORBInitRef(String objectID, String objectURL) {
		if (objectID != null && objectURL != null) {
			m_cmdArgs.parseArgs(new String[] {"-ORBInitRef", objectID + "=" + objectURL});
		}
	}
	
	public void setPort(int port)
	{
		m_port = port;
	}

	public String[] getOptions()
	{
		return m_cmdArgs.getAllArgs();
	}
	
	/**
	 * Gets the Properties like org.omg.CORBA.ORBClass
	 */
	public Properties getProperties() {
		return getProperties(false);
	}
	
	public Properties getProperties(boolean suppressPortProperty) {
		Properties props = _getProperties();
		if (props == null)
		{
			props = new Properties();
		}
		if (getORBClass() != null)
		{
			props.setProperty(ORB_CLASS_KEY, getORBClass());
		}
		if (getORBSingleton() != null)
		{
			props.setProperty(ORB_SINGLETON_CLASS_KEY, getORBSingleton());
		}
		if (getPortPropertyName() != null && !suppressPortProperty)
		{
			props.setProperty(getPortPropertyName(), Integer.toString(m_port));
		}
		return props;
	}
	

	/**
	 * The values for ORB_CLASS_KEY, ORB_SINGLETON_CLASS_KEY don't need to be set here;
	 * if they are, they will be overwritten by those from <code>getORBClass()</code> and <code>getORBSingleton()</code>.
	 */
	protected abstract Properties _getProperties();
	
	/**
	 * Gets the value for the key "org.omg.CORBA.ORBClass".
	 */
	public abstract String getORBClass();
	
	/**
	 * Gets the value for the key "org.omg.CORBA.ORBSingletonClass".
	 */
	public abstract String getORBSingleton();
	
	
	public abstract String getPortPropertyName();
	
	/**
	 * Sets values for default options that can be overwritten by calling <code>setOptions</code>.
	 * TODO: call this method from ctor
	 */
	protected void setDefaultOptions()
	{
		ArrayList<CmdLineRegisteredOption> options = new ArrayList<CmdLineRegisteredOption>();
		
		// adapted from maciContainer.cpp
		CmdLineRegisteredOption orbDottedDecAdr = new CmdLineRegisteredOption("-ORBDottedDecimalAddresses", 1);
		options.add(orbDottedDecAdr);
		options.add(new CmdLineRegisteredOption("-ORBEndpoint", 1));
		options.add(new CmdLineRegisteredOption("-ORBInitRef", 1));
		
//from C++ Container 
	//      // enable ORBDottedDecimalAddresses option
	//      if (strCmdLn.find("-ORBDottedDecimalAddresses")==ACE_CString::npos)
	//	strCmdLn += " -ORBDottedDecimalAddresses 1";
	//
	//      // add defaut Container's endpoint if no other specified
	//      if (strCmdLn.find("-ORBEndpoint")==ACE_CString::npos)
	//	{
	//	  ACE_TCHAR hostname[200];
	//	  ACE_OS::hostname (hostname, 200);
	//
	//	  strCmdLn += " -ORBEndpoint iiop://";
	//	  strCmdLn += hostname;
	//	  strCmdLn += ":3 0 5 0";                                // to be done with #define
	//	}
	//
	}

//	/**
//	 * Checks if a given exception comes from a failed CORBA narrow operation.
//	 * To be overridden by ORB-specific subclasses. Default return value is <code>false</code>.
//	 * <p>  
//	 * This method does not quite belong in this class, since it has nothing to do with ORB configuration.
//	 * Yet as long as there is no other class to encapsulate ORB-specific behavior, we put it here.
//	 */
//	public boolean isNarrowException(Throwable thr) {		
//		return false; 
//	}
}

