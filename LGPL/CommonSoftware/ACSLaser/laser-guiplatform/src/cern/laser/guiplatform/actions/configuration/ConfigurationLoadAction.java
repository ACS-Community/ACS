/*
 * ConfigurationLoadAction.java
 *
 * Created on March 6, 2003, 4:38 PM
 */

package cern.laser.guiplatform.actions.configuration;

import java.beans.IntrospectionException;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import alma.acs.container.ContainerServicesBase;

import cern.gp.nodes.GPNode;
import cern.laser.client.LaserException;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.guiplatform.configuration.ConfigurationBean;
import cern.laser.guiplatform.user.UserHandlerFactory;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;

/**
 * This is load configuration action.
 *
 * WARNING!!!!!!!! this class should probably extends cern.gp.actions.NodeAction
 *  instead of BeanActionSupport, because I want to know parent node name,
 *  when configuration is loaded, it is necessary to know user name for this
 *  configuration
 *
 * @author  pawlowsk
 */
public class ConfigurationLoadAction extends cern.gp.actions.support.NodeAction {
    
    
    /** logger */
    static final Logger logger =
    LogFactory.getLogger(ConfigurationLoadAction.class.getName());
    
    
    private final static String loadStr = NbBundle.getMessage(
    ConfigurationLoadAction.class,
    "LBL_ConfigurationLoadAction_name");
    
    private ContainerServicesBase contSvcs;
    
    /** Creates a new instance of ConfigurationLoadAction */
    public ConfigurationLoadAction(ContainerServicesBase contSvcs) {
    	this.contSvcs=contSvcs;
    }
    
    public String getName() {
        return loadStr;
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/save.gif";
    }
    
    
    protected void performAction(cern.gp.nodes.GPNode[] activatedNodes) {
        String confUserName =
        activatedNodes[0].getPeerNode().getParentNode().getName();
        String confName = activatedNodes[0].getName();
        
        try {
            User user = UserHandlerFactory.getHandler(contSvcs).getUser(confUserName,contSvcs);
            Configuration conf = user.getConfiguration(confName);
            
            
            TopComponent.Registry registry = TopComponent.getRegistry();
            
            ConsoleConfigurationWindow confWindow =
            (ConsoleConfigurationWindow) registry.getActivated();
            
            if ( confWindow == null )
                throw new NullPointerException("Configuratrion window not found");
            try {
                confWindow.updateConfigurationPanel(conf, confUserName );                
                confWindow.updateCategoryTreeExplorer();
            } catch (IntrospectionException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (CloneNotSupportedException e) {
                // never happen
                e.printStackTrace();
            }
            
        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            AcWindowManager.notifyError("Internal error.\n" +
            "See log files or contact console developers.");
        } catch (LaserException lce) {
            logger.error(lce, lce.fillInStackTrace());
            AcWindowManager.notifyError("Internal error.\n" +
            "See log files or contact console developers.");
        }
        
        
        
    }
    
    protected boolean enable(org.openide.nodes.Node[] activatedNodes) {
        
        logger.debug( " enable ");
        
        if ( activatedNodes.length > 0 ) {
            logger.debug(activatedNodes[0].getClass().toString());
            //logger.debug();
            logger.debug("configuration cookie: " + activatedNodes[0].getCookie(Configuration.class));
            logger.debug("configuration bean cookie: " + activatedNodes[0].getCookie(ConfigurationBean.class));
        }
        return activatedNodes.length == 1 &&
        // this is not good solution, but it is waste of time to debug netbeans or GP
        ((GPNode) activatedNodes[0]).getBean() instanceof ConfigurationBean &&
        super.enable(activatedNodes);
    }
    
    public org.openide.util.HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }
    
    
}
