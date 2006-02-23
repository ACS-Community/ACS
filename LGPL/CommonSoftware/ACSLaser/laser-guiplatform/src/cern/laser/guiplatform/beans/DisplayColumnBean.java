/*
 * DisplayColumnBean.java
 *
 * Created on April 14, 2003, 9:55 AM
 */

package cern.laser.guiplatform.beans;

import org.apache.log4j.Logger;

import cern.gp.beans.BeanSupport;
import cern.laser.guiplatform.capabilities.DisableCapability;
import cern.laser.guiplatform.capabilities.EnableCapability;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.BeanUtils;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;

/**
 * 
 * @author  pawlowsk
 */
public class DisplayColumnBean extends BeanSupport 
    implements DisableCapability, EnableCapability {

    static Logger logger =
        LogFactory.getLogger(DisplayColumnBean.class.getName());

    /** displayable column name (this is property name from 
     * <code>DisplayableChangeEventAdapter</code>
     */
    private String name = null;

    /** display name for property name */
    private String displayName = null;

    /** if column is enabled */
    boolean enabled = false;

    /** icon name */
    private String iconPath = null;

    /** defaultNodeAction */
    private static final String defaultNodeAction = 
        cern.laser.guiplatform.actions.EnableColumnAction.class.getName();
    
    /** nodeActions */
    private static final String [] nodeActions = 
        new String [] {
            cern.laser.guiplatform.actions.EnableColumnAction.class.getName(),
            cern.laser.guiplatform.actions.DisableColumnAction.class.getName(),
            cern.laser.guiplatform.actions.MoveUpConfChangeEventAction.class.getName(),
            cern.laser.guiplatform.actions.MoveDownConfChangeEventAction.class.getName()
        };
        
    /** Creates a new instance of DisplayColumnBean 
     * @param propName property name
     * @param displayName property display name
     * @param iconPath icon path
     * @param enabled if column is enabled
     */
    public DisplayColumnBean(String propName, String displayName, 
                            String iconPath, boolean enabled) {
        this.name = propName;
        this.displayName = displayName;
        this.iconPath = iconPath;
        this.enabled = enabled;
    }


    /** 
     * This constructor should be used only with setRootNode method
     */
    public DisplayColumnBean() {
    }

    public String getName() {
        return name;
    }
    /** This method indicates whether this column is enabled 
     * @return true if columns is enabled
     *          false if not
     */
    public boolean isEnabled() {
        return enabled;
    }
    
    public java.awt.Image getNodeIcon() {
        return this.getNodeIconFromPathname(iconPath); 
    }
    
    public cern.gp.beans.PropertyInfo[] getPropertyInfo() {
        return super.getPropertyInfo();
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public String getNodeDefaultAction() {
        return defaultNodeAction;
    }
    
    public String[] getNodeActions() {
        return nodeActions;
    }
    
    public String getShortDescription() {
       return super.getShortDescription(); 
    }
   
    protected java.awt.Image getNodeIconFromPathname(String newPathname) {
        return BeanUtils.loadImage(newPathname, this.getClass());
    } 
   
    // *************** methods from interfaces ***************************

    /** tell the object that implements this nterfaces to be disabled  */
    public void disable() {
        if(name == "date" || name=="time" ) {
            // columns date and time always active!
            return;
        }
        logger.debug("this node " + name + " should be disabled !!!!"); 
        iconPath = Constants.DISABLE_COLUMN_ICON_PATH;
        enabled = false;
        fireNodeIconPropertyChange(
                BeanUtils.loadImage(Constants.DISABLE_COLUMN_ICON_PATH,
                                    this.getClass())
        );
        AppRegister.getInstance().notifyConfigurationChange();
         
        /*
        firePropertyChange(super.NODE_ICON_PROPERTY_NAME, null, 
                        BeanUtils.loadImage(Constants.DISABLE_COLUMN_ICON_PATH,
                                    this.getClass())
        );
        */

    }
    
    /** tell the object that implements this nterfaces to be enabled  */
    public void enable() {
        logger.debug("this node " + name + " should be enabled !!!!"); 
        iconPath = Constants.ENABLE_COLUMN_ICON_PATH;
        enabled = true;
        
        fireNodeIconPropertyChange(
                BeanUtils.loadImage(Constants.ENABLE_COLUMN_ICON_PATH,
                                    this.getClass())
        );
        AppRegister.getInstance().notifyConfigurationChange();
         
        /*
        firePropertyChange(super.NODE_ICON_PROPERTY_NAME, null, 
                        BeanUtils.loadImage(Constants.DISABLE_COLUMN_ICON_PATH,
                                    this.getClass())
        );
        */

    }
    
}
