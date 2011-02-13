/*
 * LoadCategoryPanel.java
 *
 * Created on March 5, 2003, 5:07 PM
 */

package cern.laser.guiplatform.windows.configuration;

import java.awt.BorderLayout;
import java.beans.IntrospectionException;
import java.util.Collection;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.openide.util.actions.SystemAction;

import alma.acs.container.ContainerServicesBase;

import cern.gp.nodes.GPNode;
import cern.laser.client.LaserException;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.guiplatform.actions.configuration.ConfigurationDeleteAction;
import cern.laser.guiplatform.actions.configuration.ConfigurationLoadAction;
import cern.laser.guiplatform.actions.configuration.ConfigurationSetAsDefaultAction;
import cern.laser.guiplatform.configuration.ConfigurationBean;
import cern.laser.guiplatform.user.UserHandlerFactory;
import cern.laser.guiplatform.util.ImageUtility;
import cern.laser.guiplatform.util.actions.ActionUtils;
import cern.laser.guiplatform.windows.user.UserExplorer;

/**
 * This is window, where user can load, delete saved configuration
 * This window consist of ListExplorer and three buttons: LOAD, DELETE, SET AS DRFAULT.
 *
 * @author  pawlowsk
 */
public class LoadConfigurationPanel extends javax.swing.JPanel {

    private UserExplorer userListExpl = null;
     
    /** main window (something like mediator) */
    private ConsoleConfigurationWindow parentWindow = null;
    
    private User loggedUser = null;
    
    private ContainerServicesBase contSvcs;
    
    /** Creates a new instance of LoadCategoryPanel 
     *
     * @param parentWindows configuration window (mediator)
     * @param user logged user
     */
    public LoadConfigurationPanel(ConsoleConfigurationWindow parentWindow, User user, ContainerServicesBase contSvcs) 
        throws LaserException {
        super();
        this.parentWindow = parentWindow;
        this.contSvcs=contSvcs;
        loggedUser = user;
        initComponents();
    }
    
    private void initComponents() throws LaserException {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEtchedBorder());

        //setPreferredSize(new Dimension(250, 200));
        
        // label configuration
        
        JLabel confLabel = new JLabel(
                            "Configurations", 
                            ImageUtility.getPanelIcon(
                                this, 
                                "/cern/laser/guiplatform/images/users24_24.png"
                            ), 
                            JLabel.LEFT);
        add(confLabel, BorderLayout.NORTH);
        
        // initialize user explorer
        initUsersExplorer(loggedUser.getName());
        
        // craete buttons with actions
        JButton loadActionButton = new JButton(
                    ActionUtils.createActionForComp(userListExpl, 
                        SystemAction.get(ConfigurationLoadAction.class)));
 
        JButton deleteActionButton = new JButton(
                    ActionUtils.createActionForComp(userListExpl, 
                        SystemAction.get(ConfigurationDeleteAction.class)));

        JButton deleteAllActionButton =  new JButton(
                    ActionUtils.createActionForComp(userListExpl, 
                        SystemAction.get(ConfigurationSetAsDefaultAction.class)));
        
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new java.awt.GridLayout(3, 0));
        buttonPanel.add(loadActionButton);
        buttonPanel.add(deleteActionButton);
        buttonPanel.add(deleteAllActionButton);
        
        
        userListExpl.add(buttonPanel, BorderLayout.SOUTH);
        userListExpl.setPreferredSize(
            ConsoleConfigurationWindow.defaultLoadConfigurationPanelDimension);
        add(userListExpl, BorderLayout.CENTER);
    }
    
    /**
     * This method initializes explorer with users and theirs configurations
     *
     * @param userName name of user who is currnetly logged
     */
    private void initUsersExplorer(String userName) throws LaserException {
        Collection userList = UserHandlerFactory.getHandler(contSvcs).getUsers(); 
        userListExpl = new UserExplorer(userList, userName);
    }
    
    /**
     * This method adds new configurations to UserExplorer
     * @param configuration configuration
     * @throws IntrospectionException if something goes wrong
     */
    public void addNewConfiguration(ConfigurationBean configuration) 
        throws IntrospectionException {
        userListExpl.addConfiguration(configuration);
    }
    /**
     * Removes configuration
     */
    public void removeConfiguration(GPNode node) {
        userListExpl.removeConfiguration(node);
    }

    public List getDefinedConfigurationNames(User user) throws LaserConsoleException {
        return userListExpl.getDefinedConfigurationNames(user);
    }
    
}
