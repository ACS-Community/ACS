/*
 * ConfigurationBean.java
 *
 * Created on March 6, 2003, 11:23 AM
 */

package cern.laser.guiplatform.configuration;

import java.awt.Image;

import org.apache.log4j.Logger;

import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.guiplatform.actions.configuration.ConfigurationDeleteAction;
import cern.laser.guiplatform.actions.configuration.ConfigurationLoadAction;
import cern.laser.guiplatform.actions.configuration.ConfigurationSetAsDefaultAction;
import cern.laser.guiplatform.util.BeanUtils;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This is configuration bean (configuration wrapper)
 *
 * @author  pawlowsk
 */
public class ConfigurationBean extends cern.gp.beans.BeanSupport 
//    implements ConfigurationLoadCapability, ConfigurationDeleteCapability,
//                ConfigurationSetAsDefaultCapability
{
    
    private static final Logger logger = 
        LogFactory.getLogger(ConfigurationBean.class.getName());
    
    
    private Configuration conf = null;
    
    // --
    // -- these variables was created because of LaserException
    // -- constructor or other method should throw LaserException
    // -- becuase somehow Excpetion should be shown to the user
    // --
    
    /** configuration name */
    private String name = null;
    
    /** if this configuration is default */
    private boolean isDefault = false;
    


    /** Creates a new instance of ConfigurationBean 
     * 
     * In the future there shuold be only one constructor
     * public ConfugurationBean(Configuration conf)
     * 
     * Configuration -- object which comes from business API
     *
     *
     *
     */
    public ConfigurationBean(Configuration conf) throws LaserConsoleException {
        this.conf = conf;
        name = conf.getName();  // this throws LaserConsoleException
        
        // inside Configuration object should be isDefault() method
        // now is not implemented yet
        isDefault = conf.isDefault();

    }
    /** only for testing */
    /*
    public ConfigurationBean() {
        this("UNKNOWN");
    }
    */
    /**
     * Only for testing
     * @param name configuration name
     */
    /*
    public ConfigurationBean(String name) {
        this.name = name;
    }
    */
    
    /**
     * This constructor should be used only for testing
     * @param name conf name
     * @param isDefault shows whether this configuration is default configuration
     */
    /*
    public ConfigurationBean(String name, boolean isDefault) {
        this.name = name;
        this.isDefault = isDefault;
    }
    */
    
    public String getName() {
        return name;
    }
    
    public String getDisplayName() {
        return name;
    }
    
    public String[] getNodeActions() {
        String [] nodeActions = null;
        
        nodeActions = new String[] {          
            ConfigurationLoadAction.class.getName(),
            ConfigurationDeleteAction.class.getName(),
            ConfigurationSetAsDefaultAction.class.getName()
        };
        
        return nodeActions;
    }
    
    public String getNodeDefaultAction() {
        //return "cern.laser.guiplatform.configuration.ConfigurationLoadAction";
        return null;
    }
    
    public java.awt.Image getNodeIcon() {
        //return super.getNodeIcon();
        
        Image image = null;
        if ( isDefault ) 
            image = BeanUtils.loadImage(
                        "cern/laser/guiplatform/images/default_configuration24_24.png", 
                        ConfigurationBean.class);
        else
            image = BeanUtils.loadImage(
                        "cern/laser/guiplatform/images/configuration24_24.png", 
                        ConfigurationBean.class);

        return image;
        
    }
    
    //public Boolean getNodePropertiesCacheable() {
    //}
    
    public cern.gp.beans.PropertyInfo[] getPropertyInfo() {
        return null;
    }
    
    //
    // -- method from Configuration
    
    /** This matehod returns "isDefault" status for this configuration 
     * @return true if this configuration is default, otherwise false
     */
    public boolean getIsDefault() {
        return isDefault;
    }
    public void setIsDefault(boolean flag)  {
        isDefault = flag;
        fireNodeIconPropertyChange(getNodeIcon());
    } 
     
    public String getShortDescription() {
        return "not implemented yet"; 
    }
    
    
    /**************************************************************************
     *  methods from Capability interfaces
     * will never be implemented
     *************************************************************************/
    /*
    public int loadConfiguration() {
        logger.debug("user wants to load this configuration");
        
        
        // How can I obtain user name for this configuration ????
        
        
        return 0;   // this is probably unnecessary
    }
    
    public void deleteConfiguration() {
        logger.debug(getClass().getName() + "user wants to delete this confiruation ");
    }
    
    public void setAsDefaultConfiguration() {
        logger.debug(getClass().getName() + "user wants to set as default this confiruation ");
    }
    */ 
}
