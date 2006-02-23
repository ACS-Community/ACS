/*
 * AlarmNodeManagerFactory.java
 *
 * Created on August 29, 2003, 5:59 PM
 */

package cern.laser.guiplatform.alarms;

import java.util.Collection;

import org.apache.log4j.Logger;

import cern.laser.guiplatform.util.LogFactory;

/**
 *
 * @author  Woloszyn
 */
public class AlarmNodeManagerFactory {
   
    final static Logger logger = 
       LogFactory.getLogger(AlarmNodeManagerFactory.class.getName());

    /** Creates a new instance of AlarmNodeManagerFactory 
     * do not instantiate
     */
    private AlarmNodeManagerFactory() {
    }
    /**
     * This method creates appropriate <code>AlarmNodeManager</code>.
     *
     * @param key this should be one of: Constants.ACTIVE_LISTENER_KEY,
     *        Constants.INHIBIT_LISTENER_KEY,
     *
     * @param list this list should be used to initialize node manager
     *         this list should contain <code>AlarmBean</code> objects
     *
     * @return AlarmNodeManager
     */ 
    public static AlarmNodeManager createNodeManager(String key, Collection list) {  
            return new AlarmNodeManagerImpl(key, list);
    }
}
