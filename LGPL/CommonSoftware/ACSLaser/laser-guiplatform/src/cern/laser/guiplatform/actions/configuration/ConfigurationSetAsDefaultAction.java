/*
 * ConfigurationSetAsDefaultAction.java
 *
 * Created on March 7, 2003, 1:05 PM
 */

package cern.laser.guiplatform.actions.configuration;

import java.util.Iterator;

import org.apache.log4j.Logger;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import alma.acs.container.ContainerServicesBase;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.children.NodeList;
import cern.laser.client.LaserException;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConfigurationDuplicationException;
import cern.laser.console.LaserConfigurationNotFoundException;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.guiplatform.configuration.ConfigurationBean;
import cern.laser.guiplatform.user.UserHandlerFactory;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;

/**
 *
 * @author  pawlowsk
 */
//public class ConfigurationSetAsDefaultAction extends org.openide.util.actions.NodeAction {
public class ConfigurationSetAsDefaultAction extends cern.gp.actions.support.NodeAction {
    
    /** logger */
    static final Logger logger = 
        LogFactory.getLogger(ConfigurationSetAsDefaultAction.class.getName());

    private static final String setAsDefaultStr = NbBundle.getMessage(
                                        ConfigurationSetAsDefaultAction.class,
                                        "LBL_ConfigurationSetAsDefaultAction_name");
    
    private final ContainerServicesBase contSvcs;
    
    public ConfigurationSetAsDefaultAction(ContainerServicesBase contSvcs) {
    	this.contSvcs=contSvcs;
    }
    
    /** Creates a new instance of ConfigurationSetAsDefaultAction */
    //public ConfigurationSetAsDefaultAction() {
    //}
    
    public String getName() {
        return setAsDefaultStr;
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/default.gif";
    }

    //public JMenuItem getPopupPresenter() {
    //    return new Actions.MenuItem(this, true);
    //}
  
    //protected void performAction(org.openide.nodes.Node[] activatedNodes) {
    protected void performAction(cern.gp.nodes.GPNode[] activatedNodes) {
        String selectedConfigurationName = activatedNodes[0].getName();
        String configurationUserName =
            activatedNodes[0].getPeerNode().getParentNode().getName();
        
        logger.debug("selected configuration name: " + selectedConfigurationName);
        logger.debug("configuration user name: " + configurationUserName);

        try {
            User loggedUser = AppRegister.getInstance().getRegisteredUser();
            if ( !loggedUser.getName().equals(configurationUserName) )     
                copyAndSetAsDefaultConfiguration(selectedConfigurationName, 
                                             configurationUserName);
            else {          
                loggedUser.setDefaultConfiguration(selectedConfigurationName);

                // find old default node and set it to false
                for (Iterator iter = 
                        ((NodeList) activatedNodes[0].getParent().getNodeCollection()
                        ).iterator();
                        iter.hasNext(); ) 
                {
                    ConfigurationBean confBean = 
                        (ConfigurationBean) ((GPNode) iter.next()).getBean();
                    if ( confBean.getIsDefault() ) {
                        confBean.setIsDefault(false); 
                        break;
                    }

                }

                ((ConfigurationBean) activatedNodes[0].getBean()).setIsDefault(true);
            }

        } catch (LaserConfigurationNotFoundException lcnfe) {
            logger.warn(lcnfe, lcnfe.fillInStackTrace());
            copyAndSetAsDefaultConfiguration(selectedConfigurationName, 
                                             configurationUserName);
            
        } catch (LaserConfigurationDuplicationException lcde) {
            logger.warn(lcde, lcde.fillInStackTrace());
            
            // never happens

        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());            
            AcWindowManager.notifyError("Internal error.\n" +
                    "See log files or contact console developers.");
        } 


    }
    
    protected boolean enable(org.openide.nodes.Node[] activatedNodes) {
        //logger.debug("acitvated nodes count = " + activatedNodes.length);
        //return activatedNodes.length == 1;
        return activatedNodes.length == 1 && 
                // this is not good solution, but it is waste of time to debug netbeans or GP
                ((GPNode) activatedNodes[0]).getBean() instanceof ConfigurationBean &&  
                super.enable(activatedNodes);
    }

    public org.openide.util.HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }    

    //
    // -- helpers method -----------------------
    //
    /** This method copies configuration from one user to another
     * @param selectedConfName 
     * @param confUserName user name who has configuration with
     *                          selectedConfName
     */
    private void copyAndSetAsDefaultConfiguration(String selectedConfName,
                                                  String confUserName) {
        try {
            User loggedUser = AppRegister.getInstance().getRegisteredUser();
            User user =
                UserHandlerFactory.getHandler(contSvcs).getUser(confUserName,contSvcs);
            Configuration conf =
                user.getConfiguration(selectedConfName);

            boolean operationOK = false;
            String newConfName = conf.getName();
            while ( !operationOK ) {
                try {

                 
                    Configuration newConf =
                        loggedUser.createConfiguration(newConfName);

                    newConf.setBehaviour(conf.getBehaviour());
                    newConf.setSelection(conf.getSelection());
                    newConf.setHighlighted(conf.getHighlighted());
                    newConf.setAutoHighlighted(conf.getAutoHighlighted());
                    newConf.setAutoKlaxoned(conf.getAutoKlaxoned());
                    newConf.setInhibited(conf.getInhibited());
                    newConf.setMasked(conf.getMasked());
         

                    loggedUser.setDefaultConfiguration(newConf.getName());
                    // create ConfigurationBean and add to user explorer
                    ConfigurationBean confBean = new ConfigurationBean(newConf);   

                    TopComponent.Registry registry = TopComponent.getRegistry();
                    
                    ConsoleConfigurationWindow confWindow = 
                        (ConsoleConfigurationWindow) registry.getActivated();

                    if ( confWindow == null )
                        throw new NullPointerException("Configuratrion window not found");
         
                    confWindow.addConfigurationToList(confBean);

                    operationOK = true;

                } catch (LaserConfigurationDuplicationException lcde) {
                    NotifyDescriptor.InputLine desc = 
                        new NotifyDescriptor.InputLine(
                                NbBundle.getMessage(
                                    ConfigurationSetAsDefaultAction.class,
                                    "New_Configuration_name_string"),
                                NbBundle.getMessage( 
                                    ConfigurationSetAsDefaultAction.class,
                                    "Configuration_Already_Exists_string",
                                    newConfName),
                                 NotifyDescriptor.OK_CANCEL_OPTION,
                                 NotifyDescriptor.ERROR_MESSAGE);
                    
                    if ( DialogDisplayer.getDefault().notify(desc) ==
                            NotifyDescriptor.OK_OPTION )
                        newConfName = desc.getInputText();
                    else
                        operationOK = true;
                }
            }

        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            AcWindowManager.notifyError("Internal error.\n" +
                    "See log files or contact console developers.");
        } catch (LaserException le) {
            logger.error(le, le.fillInStackTrace());
            AcWindowManager.notifyError("Internal error.\n" +
                    "See log files or contact console developers.");

        } catch (java.beans.IntrospectionException ie) {
            logger.error(ie, ie.fillInStackTrace());
            AcWindowManager.notifyError("Internal error.\n" +
                    "See log files or contact console developers.");
        }

    }   
}
