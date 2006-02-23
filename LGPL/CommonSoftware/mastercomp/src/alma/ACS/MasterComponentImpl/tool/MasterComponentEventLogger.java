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
package alma.ACS.MasterComponentImpl.tool;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACS.MasterComponentReadOnly;
import alma.ACS.ROstringSeq;
import alma.ACS.MasterComponentImpl.StateChangeListener;
import alma.acs.component.client.ComponentClient;

/**
 * Command line tool that attaches to all master components in the system and displays their state changes.
 * <p>
 * Should be useful mainly for Release-2 integration where the Exec GUI does not always seem to display 
 * all expected state changes, and it's not clear who's at fault (Exec or mastercomp).
 *  
 * @author hsommer
 * created Nov 19, 2004 6:11:59 PM
 * 
 * @deprecated This is work in progress, don't use it yet!!!
 */
public class MasterComponentEventLogger extends ComponentClient {

    /**
     * map: key = (String) curl, value = (MastercompBundle) master component and associated objects 
     */
    private Map mastercompNameToObject;
    
    
    
    /**
     * @param logger
     * @param managerLoc
     * @param clientName
     * @throws java.lang.Exception
     */
    public MasterComponentEventLogger(Logger logger, String managerLoc,
            String clientName) throws Exception {
        super(logger, managerLoc, clientName);
        mastercompNameToObject = new HashMap();
    }

    /**
     * 
     */
    private void run() {
        runMasterComponentsCheckThread();
    }

    private void runMasterComponentsCheckThread() {
    }
    
    
    private void checkCurrentMasterComponents() throws Exception {
        
        Map aliveMasterComps = new HashMap();
		
		try {	        
			String[] curls = getContainerServices().findComponents(null, "IDL:alma/ACS/MasterComponent:1.0");
			
            for (int i = 0; i < curls.length; i++) {
                String curl = curls[i];
                MastercompBundle bundle = new MastercompBundle();
                if (!mastercompNameToObject.containsKey(curl)) {
                    // new mastercomponent found
                    org.omg.CORBA.Object compObj = getContainerServices().getComponent(curl);                
                    MasterComponentReadOnly mcomp = alma.ACS.MasterComponentReadOnlyHelper.narrow(compObj);
                    StateChangeListener listener = attachListener(mcomp);

                    System.out.println("found new mastercomponent '" + curl + "'.");
                    bundle.masterComp = mcomp;
                    bundle.stateChangeListener = listener;                
                    aliveMasterComps.put(curl, bundle);
                }
                else {
                    // got one we had already
                    bundle = (MastercompBundle) mastercompNameToObject.get(curl);                                
                    aliveMasterComps.put(curl, bundle);

                    // remove from old map so that we learn about dead components
                    mastercompNameToObject.remove(curl);
                }
            }
        } 
		catch (Exception e) {
            m_logger.log(Level.WARNING, "exception occured while updating list of master components: ", e);
            throw e;
        }
		
		// deal with dead components
		for (Iterator iter = mastercompNameToObject.keySet().iterator(); iter.hasNext();) {
            String deadCurl = (String) iter.next();
            m_logger.info("master component '" + deadCurl + "' no longer there.");
            MastercompBundle deadBundle = (MastercompBundle) mastercompNameToObject.get(deadCurl);                                
            deadBundle.stateChangeListener.destroyMonitor();
        }
		
		// only discard old map when all was cleaned up well
		mastercompNameToObject = aliveMasterComps;
    }

    
    
    /**
     * @param mcomp
     * @throws Exception
     */
    private StateChangeListener attachListener(MasterComponentReadOnly mcomp) throws Exception {
		ROstringSeq statesProperty = mcomp.currentStateHierarchy();
		StateChangeListener listener = new MyStateChangeListener(m_logger);		
		listener.createMonitor(statesProperty, getContainerServices());
		return listener;
    }

    
    public static void main(String[] args) {
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out.println("use start command 'acsStartJava', or set the Java property 'ACS.manager' by hand!");
			System.exit(-1);
		}
		MasterComponentEventLogger mcel = null;
		try {
			mcel = new MasterComponentEventLogger(null, managerLoc, "MasterComponentEventLogger");
			
			mcel.run();
		}
		catch (Exception e) {
			e.printStackTrace(System.err);
		}
		finally {
			if (mcel != null) {
				try {
					mcel.tearDown();
				}
				catch (Exception e1) {
					// bad luck
				}
			}
		}
    }

    private static class MyStateChangeListener extends StateChangeListener {
        
        public MyStateChangeListener(Logger logger) {
            super(logger);
        }

        protected void stateChangedNotification(String[] newStateHierarchy) {
            super.stateChangedNotification(newStateHierarchy);
        }
    }
    
}

class MastercompBundle {
    public MasterComponentReadOnly masterComp;
    public StateChangeListener stateChangeListener;
}

