/*
 * ConfigurationDeleteAction.java
 *
 * Created on March 7, 2003, 12:51 PM
 */

package cern.laser.guiplatform.actions.configuration;

import org.apache.log4j.Logger;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;

/**
 *
 * @author  pawlowsk
 */
//public class ConfigurationDeleteAction extends cern.gp.actions.support.BeanActionSupport {
public class ConfigurationDeleteAction extends cern.gp.actions.support.NodeAction {
//public class ConfigurationDeleteAction extends org.openide.actions.support.NodeAction {
    
    /** logger */
    static final Logger logger = 
        LogFactory.getLogger(ConfigurationDeleteAction.class.getName());
    
    
    private static final String deleteStr = NbBundle.getMessage(
                                         ConfigurationDeleteAction.class, 
                                        "LBL_ConfigurationDeleteAction_name");
    
    /** Creates a new instance of ConfigurationDeleteAction */
    //public ConfigurationDeleteAction() {
    //    super(ConfigurationDeleteCapability.class);
    //}
    
    public String getName() {
        return deleteStr;
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/delete.gif";
    }

    //public JMenuItem getPopupPresenter() {
    //    return new Actions.MenuItem(this, true);
    //}
   
    protected void performAction(cern.gp.nodes.GPNode[] activatedNodes) {
        User user = AppRegister.getInstance().getRegisteredUser();
        TopComponent.Registry registry = TopComponent.getRegistry();
        
        ConsoleConfigurationWindow confWindow = 
            (ConsoleConfigurationWindow) registry.getActivated();
        
        if ( confWindow == null )
            throw new NullPointerException("Configuratrion window not found");
        
         try {
            for (int i = 0; i < activatedNodes.length; i++) {
                user.removeConfiguration(activatedNodes[i].getName());
                confWindow.removeConfiguration(activatedNodes[i]);
            }        
        } catch (LaserConsoleException lce) {
            logger.error(lce);
            NotifyDescriptor.Message desc = new NotifyDescriptor.Message(
                "This configuraion cannot be deleted" + lce.toString(), NotifyDescriptor.ERROR_MESSAGE);
            DialogDisplayer.getDefault().notify(desc);
        }
    }
    
    /**
     * Checks if user can delete this configuration. It means that this particular
     * configuration beleongs to that user
     */
    protected boolean enable(org.openide.nodes.Node[] activatedNodes) {
        boolean result = true;
        boolean confLoaded = false;

        for (int i = 0; i < activatedNodes.length; i++) {
            org.openide.nodes.Node parentNode = activatedNodes[i].getParentNode();
            if ( parentNode != null ) {
                try {
                    if ( !parentNode.getName().equals(
                            AppRegister.getInstance().getRegisteredUser().getName()
                            ) )
                        result = false;
                    if ( activatedNodes[i].getName().equals(
                            AppRegister.getInstance().getLoadedConfiguration().getName()) )
                        confLoaded = true;
                } catch (LaserConsoleException lce) {
                    logger.error(lce);
                }
            }
        }

        return result && !confLoaded && super.enable(activatedNodes);
    }


}
