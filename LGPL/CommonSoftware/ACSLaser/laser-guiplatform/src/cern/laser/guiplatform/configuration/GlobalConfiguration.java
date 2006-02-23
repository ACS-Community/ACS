/*
 * GlobalConfiguration.java
 *
 * Created on March 21, 2003, 3:45 PM
 */

package cern.laser.guiplatform.configuration;

import java.util.List;


/**
 * This class stores inforamtions about configuration and is used when user
 * wants to apply configuration without saving it to database, etc
 *
 * @deprecated should not be used, is repleaced by <code>Configuration</code>
 *
 * @author  pawlowsk
 */
public class GlobalConfiguration {
    
    private java.util.List categories = null;
    
    private java.util.List filters = null;
   
    // this should be changed into some object (maybe HashMap)
    private java.lang.Object behaviour = null;
    
    /** column names, which should be displayed on alarm screen */
    private String [] displayColumns = null;
    
    /** Creates a new instance of GlobalConfiguration 
     * This is wrapper for configuration object from business layer
     *
     */
    public GlobalConfiguration(/* this should be business layer object*/) {


    }

    /** Creates a new instance of GlobalConfiguration
     *
     */ 
    public GlobalConfiguration(List categories, List filters, Object behaviour) {
        this.categories = categories;
        this.filters = filters;
        this.behaviour = behaviour;
    }
   
    public List getFilters() {
        return filters;
    }
}
