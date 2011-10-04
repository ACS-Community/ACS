/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.container;

import si.ijs.maci.ContainerOperations;

import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

/**
 * Runner for an embedded container, i.e. a container that gets started in the JVM of an ACS-aware application
 * such as the Exec GUI.
 * <p>
 * See comments for {@link #run(AcsCorba, String, String)}, which is the only method that must be called,
 * with the required external initializations.
 *  
 * @author hsommer
 * created Apr 25, 2005 5:55:27 PM
 */
public class AcsEmbeddedContainerRunner {

    protected AcsLogger m_logger;
    protected AcsCorba m_acsCorba;

    protected AcsContainer m_container;
    protected boolean isEmbedded;
    protected boolean useRecoveryMode;

    protected String m_containerName;
    protected String m_managerLoc;
    protected AcsManagerProxy m_managerProxy;

    
    
    /**
     * Constructor to be used by a separate application which needs to create an embedded container.
     */
    public AcsEmbeddedContainerRunner() {
        this(true, false);
     }
     
    /**
     * @param isEmbedded  false if this runner is not started by a separate application, but merely by some other runner.
     */
    AcsEmbeddedContainerRunner(boolean isEmbedded, boolean useRecoveryMode) {
       this.isEmbedded = isEmbedded;
       this.useRecoveryMode = useRecoveryMode;
    }

    
    /**
     * Runs this container using the ORB provided in <code>acsCorba</code> and returns w/o blocking on the ORB.
     * It is assumed that <code>acsCorba</code> is initialized, 
     * and that the singleton <code>ClientLogManager</code> has been or will be initialized for remote logging
     * outside of this method.  
     * <p>
     * Note on the implementation: the steps involved are grouped as private methods
     * that access the instance variables. The intent was to make the sequence clearer.
     *   
     * @param acsCorba  the shared instance of AcsCorba 
     * @param containerName 
     * @param managerLoc 
     * @throws AcsJContainerServicesEx  at the slightest provocation...
     */
    public void run(AcsCorba acsCorba, String containerName, String managerLoc) throws AcsJContainerEx
    {
        setContainerName(containerName);
        setManagerLoc(managerLoc);
        run(acsCorba);
    }


	void run(AcsCorba acsCorba) throws AcsJContainerEx {
		getContainerLogger();

		if (!acsCorba.isInitialized()) {
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo("The provided AcsCorba object must be initialized!");
			throw ex;
		}

		m_acsCorba = acsCorba;

		checkReadyToRun(null);

		// not calling AcsCorba.initCorba here is the main reason we have a separate embedded-container runner

		System.out.println(ContainerOperations.ContainerStatusStartupBeginMsg);
		initManagerProxy();
		createContainer();
		m_container.initialize();
		System.out.println(ContainerOperations.ContainerStatusStartupEndMsg);
	}
    
    /**
     * Gets the logger used by the container. If necessary, creates and initializes the logger. 
     * @return container logger
     */
    public AcsLogger getContainerLogger() {
        if (m_logger == null) {
            m_logger = ClientLogManager.getAcsLogManager().getLoggerForContainer(m_containerName);
        }
        return m_logger;
    }
    
    AcsContainer getContainer() {
        if (m_container == null) {
            throw new IllegalStateException("Container not yet created.");
        }
        return m_container;
    }

    AcsManagerProxy getManagerProxy() {
        if (m_managerProxy == null) {
            throw new IllegalStateException("Manager proxy not yet created.");
        }
        return m_managerProxy;
    }
    
    void checkReadyToRun(String otherMsg) throws AcsJContainerEx {
        String msg = "";
        if (m_managerLoc == null || m_managerLoc.trim().length() == 0)
        {
            msg += "no manager-location specified; ";
        }
        
        if (m_containerName == null || m_containerName.trim().length() == 0)
        {
            msg += "no container name specified; ";
        }
    
        if (otherMsg != null) {
            msg += otherMsg;
        }
        
        if (msg.length() > 0) {
        	AcsJContainerEx ex = new AcsJContainerEx();
        	ex.setContextInfo("can't start container because of missing information: " + msg);
        	throw ex;
        }
    }

    protected void createContainer() throws AcsJContainerEx {
        m_logger.fine("creating the AcsContainer " + m_containerName);
        
        m_container = new AcsContainer(m_containerName, m_acsCorba, m_managerProxy, isEmbedded);
        m_container.setRecoveryMode(useRecoveryMode);
        
        m_logger.fine("AcsContainer '" + m_containerName + "' created.");
    }

    
    /**
     * Creates the instance for <code>m_managerProxy</code> 
     * and calls <code>getManager</code> on it so that the connection 
     * to the manager will be established, without logging in to the manager yet.
     * 
     * @throws AcsJContainerEx
     */
    protected void initManagerProxy() throws AcsJContainerEx {
        m_managerProxy = new AcsManagerProxy(m_managerLoc, m_acsCorba.getORB(), m_logger);
        m_managerProxy.getManager();
    }

    void setContainerName(String name) {
        m_containerName = name;
    }

    void setManagerLoc(String loc) {
        m_managerLoc = loc;
    }

}
