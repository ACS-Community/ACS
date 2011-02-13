/*
 * ConfigurationWindowAction.java
 * This action opens console configuration window.
 *
 * Created on February 14, 2003, 3:44 PM
 */

package cern.laser.guiplatform.actions.configuration;

import java.awt.EventQueue;
import java.beans.IntrospectionException;

import org.apache.log4j.Logger;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import alma.acs.container.ContainerServicesBase;

import cern.laser.client.LaserException;
import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;

/**
 *
 * @author  pawlowsk
 */
public class ConfigurationWindowAction extends CallableSystemAction {
    
    /** logger */
    private static Logger logger =
    LogFactory.getLogger(ConfigurationWindowAction.class.getName());
    
    private final ContainerServicesBase contSvcs;
    
    public ConfigurationWindowAction(ContainerServicesBase contSvcs) {
    	this.contSvcs=contSvcs;
    }
    
    public void performAction() {
        String compName = NbBundle.getMessage(ConsoleConfigurationWindow.class,
        "LBL_ConsoleConfigurationWindow_component_name");
        
        final TopComponent top_ = AcWindowManager.findTopComponent(compName);
        
        if ( top_ != null ) {
            logger.debug("TopComponent: " + compName + " found");
            top_.requestFocus();
            //top.requestVisible();
        } else {
            try {
                final TopComponent top = new ConsoleConfigurationWindow(
                AppRegister.getInstance().getRegisteredUser(),
                AppRegister.getInstance().getLoadedConfiguration(),
                contSvcs
                );
                top.open();
                
                final Runnable doLoadAllCategoriesInTreeExplorer = new Runnable() {
                    public void run() {
                        try {
                            ((ConsoleConfigurationWindow)top).loadAllCategoriesInTreeExplorer();
                        }
                        catch (IntrospectionException ie) {
                            logger.error(ie.getMessage(), ie);
                            NotifyDescriptor d = new NotifyDescriptor.Message(
                            "Unable to create configuration window", NotifyDescriptor.ERROR_MESSAGE);
                            DialogDisplayer.getDefault().notify(d);
                        } catch (CloneNotSupportedException ie) {
                            logger.error(ie.getMessage(), ie);
                            NotifyDescriptor d = new NotifyDescriptor.Message(
                            "Unable to create configuration window", NotifyDescriptor.ERROR_MESSAGE);
                            DialogDisplayer.getDefault().notify(d);
                        }
                    }
                };
                
                EventQueue.invokeLater(doLoadAllCategoriesInTreeExplorer);
                
                
                //((ConsoleConfigurationWindow)top).loadAllCategoriesInTreeExplorer();
            } catch (LaserException le) {
                logger.error(le.getMessage(), le);
                logger.error(le.getRootCause().getMessage(), le.getRootCause());
                NotifyDescriptor d = new NotifyDescriptor.Message(
                "Unable to create configuration window", NotifyDescriptor.ERROR_MESSAGE);
                DialogDisplayer.getDefault().notify(d);
            } /*catch (IntrospectionException ie) {
                logger.error(ie.getMessage(), ie);
                NotifyDescriptor d = new NotifyDescriptor.Message(
                "Unable to create configuration window", NotifyDescriptor.ERROR_MESSAGE);
                DialogDisplayer.getDefault().notify(d);
            } catch (CloneNotSupportedException ie) {
                logger.error(ie.getMessage(), ie);
                NotifyDescriptor d = new NotifyDescriptor.Message(
                "Unable to create configuration window", NotifyDescriptor.ERROR_MESSAGE);
                DialogDisplayer.getDefault().notify(d);
            }*/
        }
    }
    
    /**
     * @return
     */
    public String getName() {
        //return "Displays the configuration window";
        //return "Configuration <category, filters, behavior>";
        return "Configuration";
    }
    
    /**
     * @return "list.gif" from image package
     */
    protected String iconResource() {
        return "org/openide/resources/actions/addWatch.gif";
    }
    
    /**
     * @return DEFAULT_HELP
     */
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ProfilesListAction.class);
    }
    
    protected void initialize() {
        super.initialize();
    }
    
}
