/*
 * ConsoleConfigurationPanel.java
 *
 * Created on February 20, 2003, 2:27 PM
 */

package cern.laser.guiplatform.windows.configuration;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.beans.IntrospectionException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import org.apache.log4j.Logger;
import org.openide.NotifyDescriptor;
import org.openide.util.NbBundle;

import alma.acs.container.ContainerServicesBase;

import cern.gp.nodes.GPNode;
import cern.gp.util.GPManager;
import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.FilterSelection;
import cern.laser.client.services.selection.Selection;
import cern.laser.console.Behaviour;
import cern.laser.console.CommentedAlarmMap;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConfigurationDuplicationException;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.alarms.AlarmSelectionHandlerFactory;
import cern.laser.guiplatform.configuration.ConfigurationBean;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.ImageUtility;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.util.ProxyBuffer;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.ActiveListExplorerPanel;
import cern.laser.guiplatform.windows.AlarmStatisticInfoPanel;
import cern.laser.guiplatform.windows.InstantFaultExplorer;
import cern.laser.guiplatform.windows.behaviour.ConfigureBehaviourPanel;
import cern.laser.guiplatform.windows.category.ChooseCategoryPanel;
import cern.laser.guiplatform.windows.filter.CreateFiltersPanel;

/**
 *
 * @author  pawlowsk
 */
public class ConsoleConfigurationPanel extends JPanel /*implements ActionListener*/ {
    
    private static Logger logger =
    LogFactory.getLogger(ConsoleConfigurationPanel.class.getName());
    
    
    /** main panel */
    private JTabbedPane mainPane = null;
    
    /** category panel */
    private ChooseCategoryPanel categoryPanel = null;
    
    /** category panel */
    private CreateFiltersPanel filtersPanel = null;
    
    /** behaviour panel, contains things like "New alarm behaviour" as well as
     * columns to be displayed
     */
    private ConfigureBehaviourPanel behaviourPanel = null;
    
    
    /** info label, tells wich configuration is currently loaded */
    private JLabel infoLabel = null;
    
    /** save button */
    private JButton saveButton = null;
    
    /** reset button */
    private JButton resetButton = null;
    
    /** apply button */
    private JButton applyButton = null;
    
    /** close button */
    private JButton closeButton = null;
    
    private Configuration configuration = null;
    
    /** main/parent window (something like mediator) */
    private ConsoleConfigurationWindow parentWindow = null;
    
    private boolean overwritten = false;    // if configuration should be overwritten
    // or created new one
    
    private SaveConfigurationOptionPane optionPane = null;
    
    private User loggedUser = null;
    
    private boolean ownConfiguration = true; // if lately Loaded config belongs to current user
    
    private ContainerServicesBase contSvcs;
    
    /** Creates a new instance of ConsoleConfigurationPanel
     * @param configuration configuration for which this panel is created
     */
    public ConsoleConfigurationPanel(ConsoleConfigurationWindow parentWindow,
    Configuration configuration, ContainerServicesBase contSvcs) throws LaserConsoleException {
        super();
        this.parentWindow = parentWindow;
        this.configuration = configuration;
        this.contSvcs=contSvcs;
        initComponents();
    }
    
    
    private void initComponents() throws LaserConsoleException {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEtchedBorder());
        
        
        JPanel infoPanel = new JPanel();
        infoLabel = new JLabel("Current configuration: " +
        configuration.getName());
        infoPanel.add(infoLabel);
        add(infoPanel, BorderLayout.NORTH);
        
        
        mainPane = new JTabbedPane();
        
        makeCategoryPanel();
        makeFiltersPanel();
        makeBehaviorPanel();
        
        add(mainPane, BorderLayout.CENTER);
        
        JPanel buttonOrderPanel = new JPanel(new FlowLayout());
        // crate buttons
        saveButton = new JButton("Save",
        ImageUtility.getPanelIcon(this,
        "/org/openide/resources/actions/saveAll.gif")
        );
        //saveButton.setEnabled(false);
        saveButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveButtonActionPerformed(evt);
            }
        });
        
        
        saveButton.setEnabled(false);
        AppRegister.getInstance().registerConfigurationChangeListeners(saveButton);
        
        applyButton = new JButton("Apply",
        ImageUtility.getPanelIcon(this,
        "/org/openide/resources/actions/execute.gif")
        );
        //applutButton.setEnabled(false);
        applyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                applyButtonActionPerformed(evt);
            }
        });
        
        
        resetButton = new JButton("Reset",
        ImageUtility.getPanelIcon(this,
        "/org/openide/resources/actions/clean.gif")
        );
        
        resetButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                resetButtonActionPerformed(evt);
            }
        });
        
        closeButton = new JButton("Close",
        ImageUtility.getPanelIcon(this,
        "/cern/laser/guiplatform/images/exit.gif")
        );
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                parentWindow.close();
            }
        });
        
        
        //behaviourPanel.add(saveButton, BorderLayout.SOUTH);
        buttonOrderPanel.add(saveButton);
        buttonOrderPanel.add(applyButton);
        buttonOrderPanel.add(resetButton);
        buttonOrderPanel.add(closeButton);
        
        add(buttonOrderPanel, BorderLayout.SOUTH);
    }
    
    /**
     * This method create tabbed pane for category browser
     */
    protected void makeCategoryPanel() {
        categoryPanel = new ChooseCategoryPanel(configuration,contSvcs);
        mainPane.addTab("Category tree", ImageUtility.getPanelIcon(this,
        "/cern/laser/guiplatform/images/view_tree.png"),
        categoryPanel, "Choose category");
    }
    
    /**
     * This method create tabbed pane for filters
     */
    protected void makeFiltersPanel() throws LaserConsoleException {
        filtersPanel = new CreateFiltersPanel(configuration);
        //filterPanel.registerConfigurationChangeListener(this);
        mainPane.addTab("Filters", ImageUtility.getPanelIcon(this,
        "/cern/laser/guiplatform/images/filters_icon.png"),
        filtersPanel, "Create filters");
        
        
    }
    
    /**
     * This method create tabbed pane for "behaviour"
     */
    protected void makeBehaviorPanel() throws LaserConsoleException {
        behaviourPanel = new ConfigureBehaviourPanel(configuration);
        
        mainPane.addTab("Behaviour", ImageUtility.getPanelIcon(this,
        "/cern/laser/guiplatform/images/behaviour_icon.png"),
        behaviourPanel, "Behaviour");
        
    }
    
    /**
     * This methods removes filter from defined filter list
     * @param node filter object
     */
    public void removeFilter(GPNode node) {
        filtersPanel.removeFilter(node);
        configurationChanged();
    }
    
    public void removeAllFilters() {
        filtersPanel.removeAllFilters();
        configurationChanged();
    }
    
    public void setInUpdateMode(String name, String operator, String value,
    String buttonName) {
        
        filtersPanel.setInUpdateMode(name, operator, value, buttonName);
    }
    public void updateCategoryTreeExplorer()
    throws IntrospectionException, CloneNotSupportedException {
        categoryPanel.updateCategoryTreeExplorer();
    }
    public void loadAllCategoriesInTreeExplorer() 
    throws IntrospectionException, CloneNotSupportedException {
        categoryPanel.loadAllCategoriesInTreeExplorer();
    }
    public void addCategory(GPNode node)
    throws IntrospectionException, CloneNotSupportedException {
        categoryPanel.addCategory(node);
        configurationChanged();
    }
    
    public void addCategoryWithoutChildren(GPNode node)
    throws IntrospectionException, CloneNotSupportedException {
        categoryPanel.addCategoryWithoutChildren(node);
        configurationChanged();
    }
    public void addSubCategories(GPNode node)
    throws IntrospectionException, CloneNotSupportedException {
        categoryPanel.addSubCategories(node);
        configurationChanged();
    }    
    
    public void removeCategoryWithoutChildren(GPNode node)
    throws IntrospectionException, CloneNotSupportedException {
        categoryPanel.removeCategoryWithoutChildren(node);
        configurationChanged();
    }    
    public void removeSubCategories(GPNode node)
    throws IntrospectionException, CloneNotSupportedException {
        categoryPanel.removeSubCategories(node);
        configurationChanged();
    }
    public void removeCategory(GPNode node) {
        categoryPanel.removeCategory(node);
        configurationChanged();
    }
    
    public void removeAllCategories() {
        categoryPanel.removeAllCategories();
        configurationChanged();
    }
    
    /**
     * Updates configuration panel, when user wants to load configuration
     * (ConfigurationLoad action)
     *
     * @param newConfiguration configuration to be loaded
     */
    public void updateConfiguration(Configuration newConfiguration, String ownerLogin) throws LaserConsoleException {
        logger.debug("update configuration, conf name: " +
        newConfiguration.getName());
        this.configuration = newConfiguration;
        categoryPanel.updateCategorySelection(newConfiguration);
        filtersPanel.updateFilterSelection(newConfiguration);
        behaviourPanel.updateBehaviour(newConfiguration);
        
        infoLabel.setText("Current configuration: " + configuration.getName());
        saveButton.setEnabled(false);
        
        // set if the config belong to user or not
        if( ownerLogin.compareTo(AppRegister.getInstance().getRegisteredUser().getName())==0 ) {
            ownConfiguration = true;
        }
        else {
            ownConfiguration = false;
        }
    }
    
    /** method used when loaded configuration has changed */
    public void configurationChanged() {
        //saveButton.setEnabled(true);
        AppRegister.getInstance().notifyConfigurationChange();
    }
    
    private void applyButtonActionPerformed(java.awt.event.ActionEvent evt) {
        logger.debug("Configuration should be applied:");
        
        Configuration confTemp = null;
        String configurationName = null;
        Selection selection = null;
        Behaviour behaviour = null;
        
        loggedUser = AppRegister.getInstance().getRegisteredUser();
        
        if ( saveButton.isEnabled() || ownConfiguration==false) {     // new configuration should be created
            logger.debug("new configuration " + Constants.RECENTLY_APPLIED_CONFIGURATION + " should be created" );
            ownConfiguration = true;
            if ( (confTemp = createConfiguration(
            Constants.RECENTLY_APPLIED_CONFIGURATION, false)) == null )
                return;
            logger.debug("new configuration " + Constants.RECENTLY_APPLIED_CONFIGURATION + " was be created" );
            
        } else {        // currently loaded conf is used
            String confname = null;
            try {
                confname = configuration.getName();
            } catch (LaserConsoleException lce) {
                logger.error(lce, lce.fillInStackTrace());
                logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
            }
            logger.debug("conf: " + confname + " should be applied");
            confTemp = configuration;
        }
        
        
        // disconnet from bussiens tier
        AlarmSelectionHandler jms_selectionHandler = null;
        logger.debug("disconnet from bussiens tier");
        
        try {
            jms_selectionHandler = AlarmSelectionHandlerFactory.getHandler(contSvcs);
            jms_selectionHandler.resetSelection();
            logger.debug("resetSelection");
        } catch (LaserException le) {
            logger.error(le, le.fillInStackTrace());
            logger.error(le.getRootCause(), le.getRootCause().fillInStackTrace());
            NotifyDescriptor.Message message = new NotifyDescriptor.Message(
            "Internal error. Cannot reset selection.\n",
            NotifyDescriptor.ERROR_MESSAGE);
            GPManager.notify(message);
            return;
        }
        
        // disable ProxyBuffer
        cern.laser.guiplatform.util.ProxyBuffer.getDefault().disable();
        logger.debug("disabled ProxyBuffer");
        
        // claen node managers and lists
        // immediately after diconnection for BL
        AlarmContainer.getDefault().clearNodeManagers();
        logger.debug("cleaned NodeManagers");
        
        // register new loaded configuration
        java.util.Map activeAlarms = null;
        try {
            AppRegister.getInstance().registerLoadedConfiguration(confTemp);
            selection  = confTemp.getSelection();
            behaviour = confTemp.getBehaviour();
            configurationName = confTemp.getName();
            // make new selection
            activeAlarms = jms_selectionHandler.select(selection,
            ProxyBuffer.getDefault());
            // reload Alarm container
            AlarmContainer.getDefault().reloadContainer(activeAlarms, confTemp);
            infoLabel.setText("Current configuration: " + confTemp.getName());
        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
            
            // here console info panel can display into, that console
            // now is disconnected and should be reloaded (logout, login)
            
            NotifyDescriptor.Message message = new NotifyDescriptor.Message(
            "Connection error. Unable to register new configuration, or reload alarm container.\n" +
            "Please, logout and try to login again later",
            NotifyDescriptor.ERROR_MESSAGE);
            GPManager.notify(message);
            return;
        } catch (LaserException le) {
            logger.error(le, le.fillInStackTrace());
            logger.error(le.getRootCause(), le.getRootCause().fillInStackTrace());
            
            NotifyDescriptor.Message message = new NotifyDescriptor.Message(
            "Connection error. Unable to select new fault states.\n" +
            "Please, logout and try to login again later",
            NotifyDescriptor.ERROR_MESSAGE);
            GPManager.notify(message);
            return;
        }
        
        
        
        // set coulumns to display for all lists
        String activeListTopName = NbBundle.getMessage(
        ActiveListExplorerPanel.class,
        "LBL_Active_list_component_name");
        AcWindowManager.setTableColumns(activeListTopName , behaviour.getColumnsToDisplay());
        
        /*
        String compName =  NbBundle.getMessage(
                                        ActiveListExplorerPanel.class,
                                        "LBL_Inhibit_list_component_name");
        AcWindowManager.setTableColumns(compName , behaviour.getColumnsToDisplay());
         
        compName =  NbBundle.getMessage(
                                        ActiveListExplorerPanel.class,
                                        "LBL_Mask_list_component_name");
        AcWindowManager.setTableColumns(compName , behaviour.getColumnsToDisplay());
         */
        String compName =  NbBundle.getMessage(InstantFaultExplorer.class,
        "LBL_Instant_list_component_name");
        AcWindowManager.setTableColumns(compName , behaviour.getColumnsToDisplay());
        
        // update AlarmStatisticInfoPanel
        org.openide.windows.TopComponent activeExp = AcWindowManager.findTopComponent(activeListTopName);
        java.awt.Component component = activeExp.getComponent(activeExp.getComponentCount()-1);
        ((AlarmStatisticInfoPanel)
        component).updatePanel(
        configurationName,
        behaviour,
        selection.getReducedMaskedSelection());
        
        // enable Proxy buffer
        ProxyBuffer.getDefault().enable();
        
        
    }
    
    private void saveButtonActionPerformed(java.awt.event.ActionEvent evt) {
        
        logger.debug("Save button pressesd, user wants to save" +
        " your configuration");
        
        loggedUser = AppRegister.getInstance().getRegisteredUser();
        
        
        try {
            // get information from optionPane
            optionPane = new SaveConfigurationOptionPane(this.parentWindow,
            parentWindow.getDefinedConfigurations(loggedUser));
        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
            NotifyDescriptor.Message message = new NotifyDescriptor.Message(
            "Internal error.\n" +
            "Look into log file",
            NotifyDescriptor.ERROR_MESSAGE);
            GPManager.notify(message);
            return;
        }
        
        if ( optionPane.shouldBeSaved() ) {
            Configuration confToBeSaved = null;
            if ( (confToBeSaved = createConfiguration(
            optionPane.getConfigurationName(),
            optionPane.shouldBeDefault())) == null )
                return;
            
            //if ( !overwritten ) {
            try {
                ConfigurationBean configurationBean = new ConfigurationBean(confToBeSaved);
                parentWindow.addConfigurationToList(configurationBean);
            } catch (LaserConsoleException lce) {
                logger.error(lce, lce.fillInStackTrace());
                logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                //NotifyDescriptor.Message message = new NotifyDescriptor.Message(
                //                        "Configration craated, but unable to add this to explorer.",
                //                        NotifyDescriptor.ERROR_MESSAGE);
                //GPManager.notify(message);
            } catch (java.beans.IntrospectionException ie) {
                logger.error(ie, ie.fillInStackTrace());
            }
            //} else {
            //}
            
            configuration = confToBeSaved;
            infoLabel.setText("Current configuration: " + optionPane.getConfigurationName());
            saveButton.setEnabled(false);
            
            
            
        } // end configuration should be saved
    }
    
    /**
     * @param confName configuration name
     * @param _default is configuration should be default
     * @return configuration if everything is ok, null in case of error
     */
    private Configuration createConfiguration(String confName, boolean _default) {
        Configuration confToBeSaved = null;
        Behaviour previousBehaviour = null;
        Selection previousSelection = null;
        CommentedAlarmMap previousMasked = null;
        CommentedAlarmMap previousInhibited = null;
        CommentedAlarmMap previousHighlighted = null;
        CommentedAlarmMap previousAutoHighlighted = null;
        CommentedAlarmMap previousAutoKlaxoned = null;
        CommentedAlarmMap previousAcknowledged = null;
        
        boolean isPreviousStatusData = false;
        
        if( configuration != null) {
            try {
                previousMasked = configuration.getMasked();
                previousInhibited = configuration.getInhibited();
                previousHighlighted = configuration.getHighlighted();
                previousAutoHighlighted = configuration.getAutoHighlighted();
                previousAutoKlaxoned = configuration.getAutoKlaxoned();
                previousAcknowledged = configuration.getAcknowledged();
                
                isPreviousStatusData = true;
            }
            catch(LaserConsoleException lce) {
                logger.error(lce, lce.fillInStackTrace());
                logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                NotifyDescriptor.Message message = new NotifyDescriptor.Message(
                "Unable to create configuration.",
                NotifyDescriptor.ERROR_MESSAGE);
                GPManager.notify(message);
                return null;
            }
        }
        //
        try {
            confToBeSaved =
            loggedUser.createConfiguration(confName);
            
        } catch (LaserConfigurationDuplicationException lcde) {
            // when configuration should be overwritten
            overwritten = true;
            try {
                confToBeSaved = loggedUser.getConfiguration(confName);
                previousSelection = confToBeSaved.getSelection();
                previousBehaviour = confToBeSaved.getBehaviour();
            }
            catch (LaserConsoleException lce) {
                logger.error(lce, lce.fillInStackTrace());
                logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                NotifyDescriptor.Message message = new NotifyDescriptor.Message(
                "Unable to get configuration: " +
                confName,
                NotifyDescriptor.ERROR_MESSAGE);
                GPManager.notify(message);
                return null;
            }
            catch (LaserConnectionException lce) {
                logger.error(lce, lce.fillInStackTrace());
                logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                NotifyDescriptor.Message message = new NotifyDescriptor.Message(
                "Unable to get configuration: " +
                confName,
                NotifyDescriptor.ERROR_MESSAGE);
                GPManager.notify(message);
                return null;
            }
        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
            NotifyDescriptor.Message message = new NotifyDescriptor.Message(
            "Unable to create configuration.",
            NotifyDescriptor.ERROR_MESSAGE);
            GPManager.notify(message);
            return null;
        }
        
        // prepare Selection
        Selection selection = null;
        try {
            selection = ((AlarmSelectionHandler)
            AlarmSelectionHandlerFactory.getHandler(contSvcs)).createSelection();
        } catch (LaserException le) {
            logger.error(le, le.fillInStackTrace());
            logger.error(le.getRootCause(), le.getRootCause().fillInStackTrace());
            NotifyDescriptor.Message message = new NotifyDescriptor.Message(
            "Configuration can't be created.\nUnable to create selection.",
            NotifyDescriptor.ERROR_MESSAGE);
            GPManager.notify(message);
            return null;
        }
        
        CategorySelection category_selection = selection.createCategorySelection();
        category_selection.addAll(categoryPanel.getChoosenCategories());
        selection.setCategorySelection(category_selection);
        
        // set reduced mask filtering
        selection.setReducedMaskedSelection(behaviourPanel.isReducedMaskedFlagOn());
        
        
        // prepare filter selection
        FilterSelection filter_selection = selection.createFilterSelection();
        filter_selection.addAll(filtersPanel.getDefinedFilters());
        
        selection.setFilterSelection(filter_selection);
        
        // prepare Behaviour
        Behaviour behaviour = confToBeSaved.createBehaviour();
        behaviour.setAlarmAutoKlaxon(behaviourPanel.isAlarmAutoKlaxon());
        behaviour.setAlarmAutoTerminated(behaviourPanel.isAlarmAutoTerminated());
        behaviour.setAlarmDistinguished(behaviourPanel.isAlarmDistinguished());
        behaviour.setDailyPrinter(behaviourPanel.getDailyPrinter());
        behaviour.setDailyPrinting(behaviourPanel.isDailyPrinting());
        behaviour.setKlaxonVolume(behaviourPanel.getKlaxonVolume());
        behaviour.setColumnsToDisplay(behaviourPanel.getColumnsToDisplay());
        
        
        // not implemented yet in laser-console API
        //behaviour.setActiveListFont();
        try {
            confToBeSaved.setBehaviour(behaviour);
            confToBeSaved.setSelection(selection);
            if ( isPreviousStatusData ) {
                confToBeSaved.setMasked(previousMasked);
                confToBeSaved.setInhibited(previousInhibited);
                confToBeSaved.setHighlighted(previousHighlighted);
                confToBeSaved.setAutoHighlighted(previousAutoHighlighted);
                confToBeSaved.setAutoKlaxoned(previousAutoKlaxoned);
                confToBeSaved.setAcknowledged(previousAcknowledged);
            }
        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
            NotifyDescriptor.Message message = new NotifyDescriptor.Message(
            "Unable to create configuration.\n" +
            "Unable to set behaviour and selection",
            NotifyDescriptor.ERROR_MESSAGE);
            GPManager.notify(message);
            
            
            // rollback - but here maight be problems
            if ( overwritten ) {
                try {
                    confToBeSaved.setBehaviour(previousBehaviour);
                    confToBeSaved.setSelection(previousSelection);
                } catch (LaserConsoleException lce_) {
                    logger.error(lce_, lce_.fillInStackTrace());
                    logger.error(lce_.getRootCause(), lce_.getRootCause().fillInStackTrace());
                    message = new NotifyDescriptor.Message(
                    "ERROR. This configuration should be deleted.",
                    NotifyDescriptor.ERROR_MESSAGE);
                    GPManager.notify(message);
                }
            }
            
            return null;
        }
        
        if ( _default )
            try {
                loggedUser.setDefaultConfiguration(confName);
            } catch (LaserConsoleException lce) {
                logger.error(lce, lce.fillInStackTrace());
                logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                NotifyDescriptor.Message message = new NotifyDescriptor.Message(
                "Problem with setting this configuration as default.",
                NotifyDescriptor.ERROR_MESSAGE);
                GPManager.notify(message);
                return null;
            }
        
        return confToBeSaved;
    }
    
    private void resetButtonActionPerformed(java.awt.event.ActionEvent evt) {
        ((ConfigureBehaviourPanel)behaviourPanel).reset();
        ((ChooseCategoryPanel)categoryPanel).reset();
        filtersPanel.reset();
        configurationChanged();
    }
    
    //
    // -- inner class ----------------------------------------
    //
    //public interface TestInterface {
    //    public void configurationChanged();
    //}
    
    /**
     * Dialog, where user can write configuration name.
     * This class shows JOptionPane with appropriate components
     * i. e. JRadioButton setAsDefault
     */
    //private class SaveConfigurationOptionPane extends JPanel {
    private class SaveConfigurationOptionPane {
        
        /** set as defautl radio */
        private JRadioButton setAsDefault = null;
        /** this flag indicates if this configuration should be setted ad default*/
        private boolean isDefault = false;
        
        private JTextField confNameField = null;
        /** configuration name */
        private String confName = null;
        /** if configuration should be saved */
        boolean configurationSaved = false;
        
        /** panel, which contains radio button "set as default" and
         * text field for configuration name
         */
        private JPanel panel = null;
        
        /** parent container */
        private Container parentContainer = null;
        /** exixted configuration names */
        private List existedConfNames = null;
        
        
        /** dialog where JOptionPane is displayed */
        private JDialog dialog = null;
        private JOptionPane optionPane = null;
        
        final String saveStr = NbBundle.getMessage(
        ConsoleConfigurationPanel.class,
        "SaveConfigurationOptionPane_save_string");
        final String cancelStr  = NbBundle.getMessage(
        ConsoleConfigurationPanel.class,
        "SaveConfigurationOptionPane_cancel_string");
        
        
        
        /**
         * Constructor
         *
         * @params container parent container
         * @params currnetConfigurationNames list with saved configuration names
         */
        public SaveConfigurationOptionPane(Container container,
        List currentConfigurationNames) {
            
            this.parentContainer = container;
            this.existedConfNames = currentConfigurationNames;
            initComponents();
        }
        
        /**
         * This method initializes all required components
         */
        
        private void initComponents() {
            //Object options [] = {"Save", "Cancel"};
            Object options [] = {saveStr, cancelStr};
            panel = new JPanel();
            panel.setLayout(new BorderLayout());
            
            //Border etched = BorderFactory.createEtchedBorder();
            //TitledBorder titleBorder = BorderFactory.createTitledBorder(etched,
            //                "Here you can save your configuration");
            //panel.setBorder(titleBorder);
            JLabel label = new JLabel("Here you can save your configuration");
            panel.add(label, BorderLayout.NORTH);
            setAsDefault = new JRadioButton("Set as default");
            panel.add(setAsDefault, BorderLayout.CENTER);
            
            JPanel panel_temp = new JPanel();
            JLabel label_temp = new JLabel("Enter your configuration's name");
            confNameField = new JTextField(30);
            panel_temp.setLayout(new GridLayout(2, 1));
            panel_temp.add(label_temp);
            panel_temp.add(confNameField);
            panel.add(panel_temp, BorderLayout.SOUTH);
            
            optionPane = new JOptionPane(panel,
            JOptionPane.INFORMATION_MESSAGE,
            JOptionPane.OK_CANCEL_OPTION,
            null, options, options[0]);
            
            optionPane.setPreferredSize(new Dimension(300, 200));
            
            
            dialog = new JDialog();
            dialog.setModal(true);
            dialog.setTitle("Save configuration");
            
            dialog.setLocationRelativeTo(this.parentContainer);
            dialog.setContentPane(optionPane);
            /*
            dialog.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
            dialog.addWindowListener(
                new WindowAdapter() {
                    public void windowClosing(WindowEvent we) {
                    /*
             * Instead of directly closing the window,
             * we're going to change the JOptionPane's
             * value property.
             *
             
             
                    }
            });
             */
            
            optionPane.addPropertyChangeListener(
            new PropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent e) {
                    
                    String prop = e.getPropertyName();
                    
                    if ( dialog.isVisible() && (e.getSource() == optionPane)
                    && (prop.equals(JOptionPane.VALUE_PROPERTY) ||
                    prop.equals(JOptionPane.INPUT_VALUE_PROPERTY))
                    ) {
                        Object value = optionPane.getValue();
                        if (value == JOptionPane.UNINITIALIZED_VALUE) {
                            //ignore reset
                            return;
                        }
                        
                        // Reset the JOptionPane's value.
                        // If you don't do this, then if the user
                        // presses the same button next time, no
                        // property change event will be fired.
                        optionPane.setValue(
                        JOptionPane.UNINITIALIZED_VALUE);
                        
                        // here you can check something before closing
                        // the window
                        
                        if ( value.equals(saveStr) ) {      // user clicks save button
                            String confNameTemp = confNameField.getText();
                            if ( confNameTemp == null ||
                            confNameTemp.length() < 1 )    // user does not give conf name
                            {
                                JOptionPane.showMessageDialog(
                                optionPane,
                                "You should give " +
                                "configuration name !!!",
                                "Try again",
                                JOptionPane.ERROR_MESSAGE);
                                
                            } else {  // user gives configuration name
                                
                                if ( existedConfNames.contains(confNameTemp) ) {
                                    int n = JOptionPane.showConfirmDialog(
                                    optionPane,
                                    "\"" + confNameTemp + "\"" +
                                    " already exists and will be overwritten !" +
                                    " Is this OK for you ? ",
                                    " Warning ",
                                    JOptionPane.WARNING_MESSAGE
                                    );
                                    if ( n == JOptionPane.YES_OPTION ) {
                                        logger.debug(" conf name can be overwritten !! ");
                                        
                                        if ( setAsDefault.isSelected() )
                                            isDefault = true;
                                        configurationSaved = true;
                                        confName = confNameTemp;
                                        dialog.setVisible(false);
                                    }
                                } else {
                                    
                                    if ( setAsDefault.isSelected() )
                                        isDefault = true;
                                    configurationSaved = true;
                                    confName = confNameTemp;
                                    dialog.setVisible(false);
                                }
                                //dialog.setVisible(false);
                            }
                        } else {        // user clicks cancel button
                            logger.debug("Cancel button");
                            isDefault = false;
                            configurationSaved = false;
                            confName = null;
                            dialog.setVisible(false);
                        }
                        //dialog.setVisible(false);
                    }
                }
            }
            );
            //dialog.setSize(new Dimension(200, 200));
            dialog.pack();
            dialog.setVisible(true);
        }
        
        /**
         * This method returns
         */
        public boolean shouldBeDefault() {
            return this.isDefault;
        }
        
        /**
         * This method returns configuration name.
         * If configuration should not be saved ("Cancel" button), configuration
         * name is null
         */
        public String getConfigurationName() {
            return confName;
        }
        
        /**
         * Thie method indicates if configurations should be saved.
         * If user clicks "Cancel" button or closes window explicity,
         * then configurations should not be saved, otherwise configuration
         * shuold be saved
         *
         * @return true when user clicks "Save" button
         *          false when "Cancel" or closes window explicity.
         */
        public boolean shouldBeSaved() {
            return configurationSaved;
            
        }
    }
    
}
