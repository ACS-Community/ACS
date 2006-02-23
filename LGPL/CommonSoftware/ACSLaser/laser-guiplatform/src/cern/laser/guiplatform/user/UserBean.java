/*
 * UserBean.java
 *
 * Created on March 5, 2003, 4:47 PM
 */

package cern.laser.guiplatform.user;

import org.apache.log4j.Logger;

import cern.laser.client.LaserException;
import cern.laser.console.User;
import cern.laser.guiplatform.configuration.ConfigurationBean;
import cern.laser.guiplatform.util.BeanUtils;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This is Alarm User Beans, (User, which comes from bussines API, wrapper)
 * @author  pawlowsk
 */
public class UserBean extends cern.gp.beans.BeanSupport {
    
    /** logger */
    private final static Logger logger = LogFactory.getLogger(UserBean.class.getName());
    //private String loginName = null;
    
    /** user from business layer */
    private User user = null;
    
    // --
    // -- these variables was created because of LaserException
    // -- constructor or other method should trow LaserException
    // -- becuase somehow Excpetion should be shown to the user
    // --
    
    /** user first name */
    //private String firstname = null;
    /** user last name */
    //private String lastName = null;
    
    /** alarm user name (login name) */
    private String name = null;

    /** indicates wheteher this user is logged */
    private boolean isLogged = false;
    
    /** Configurations for given alarm user (ConfigurationBean objects) */
    private java.util.Collection configurations = null;
    
    /** Creates a new instance of UserBean */
    public UserBean(User user) throws LaserException {
        this.user = user;
        name = user.getName();

        configurations = user.getConfigurations();
    }
    
    /*
    public UserBean(String name) {                                            
        this.name = name;
        firstName = name;
        lastName = name;
    }
     */

    public void setIsLogged(boolean flag) {
        isLogged = flag;
    }
    
    public String getDisplayName() {
        if ( isLogged )
            return name + " (logged in)";
        else
            return name;
    }
    
    public String getName() {
        return name;
    }
    
    public String[] getNodeActions() {
        //return super.getNodeActions();
        return new String[] {          
            "cern.gp.actions.PropertiesAction"
            ,"cern.gp.actions.OpenLocalExplorerAction"
        };
    }
    
    public String getNodeDefaultAction() {
        return "cern.gp.actions.PropertiesAction";
    }
    
    public java.awt.Image getNodeIcon() {
        //return getNodeIconFromPathname("");
        //return super.getNodeIcon();
        //return Utilities.loadImage("cern/laser/guiplatform/images/user_icon24_24.png");
    
        
        logger.debug(" load image for user icon ");
        java.awt.Image image = null;
        String imagePath = null;
        
        //if ( !isLogged )
            imagePath = "cern/laser/guiplatform/images/user_icon24_24.png";
        //else
        //    imagePath = "cern/laser/guiplatform/images/logged_user_icon24_24.png";
        
        image = BeanUtils.loadImage(imagePath, UserBean.class);
        
        logger.debug("image is " + image + "path=" + imagePath); 
        return image;
         
    }
    
    public cern.gp.beans.PropertyInfo[] getPropertyInfo() {
        return null;
    }
    
    public String getShortDescription() {
        return "user short description";
    }

    
    
    //
    // -- methods from User class -------------------------------------
    //
    
    /** This method returns configurations for user */
    public java.util.Collection getConfigurations() {
        return configurations;
    }
    /** This method sets configuration list for given user 
     * 
     * @conf <code>List</code> with <code>ConfigurationBean</objects>
     */
    public void setConfigurations(java.util.List conf) {
        configurations = conf;
    }
    /** This method returns default configuration for this user 
     * @return default configuration <code>ConfigurationBean</code> object
     *          of null if user does not have default configuration
     */
    public ConfigurationBean getDefaultConfiguration() {
        return null;
    }
}
