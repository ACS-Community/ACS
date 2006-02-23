/*
 * CreateFiltersPanel.java
 *
 * Created on March 19, 2003, 12:04 PM
 */

package cern.laser.guiplatform.windows.filter;

import java.awt.Color;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;
import org.openide.explorer.propertysheet.PropertySheetSettings;
import org.openide.options.SystemOption;

import cern.gp.nodes.GPNode;
import cern.laser.client.services.selection.Filter;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.guiplatform.filters.FilterItemBean;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This windows allows user to create, update and remove filters for given 
 * configuration
 *
 *
 * @author  pawlowsk
 */
public class CreateFiltersPanel extends javax.swing.JPanel {
   

    final static Logger logger = 
        LogFactory.getLogger(CreateFiltersPanel.class.getName());
    // variables doclaration
    
    /** panel where user can create new filter */
    private SingleFilterItem createFilterItem = null;
    /** on this panel, thers is list with defined filters */
    private CreatedFiltersExplorer createdFiltersExpl = null;
    
    
    // this should be palced in other class (probably in main class)
    static {
     // change the color of the fonts for read-only properties:
        PropertySheetSettings pss = 
            (PropertySheetSettings)SystemOption.findObject(PropertySheetSettings.class,true);
       	pss.setDisabledPropertyColor(Color.black);
        pss.setValueColor(Color.blue);
    }
    

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton deleteAllButton;
    private javax.swing.JPanel tableListSouthPanel;
    private javax.swing.JPanel buttonPanel;
    private javax.swing.JPanel staticInfoPanel;
    private javax.swing.JPanel headerPanel;
    private javax.swing.JPanel filtrListPanel;
    private javax.swing.JButton addButton1;
    private javax.swing.JButton updateButton;
    private javax.swing.JPanel centralPanel;
    private javax.swing.JPanel southPanel;
    //private javax.swing.JLabel jLabel1;
    private javax.swing.JButton deleteButton;
    private javax.swing.JPanel tableListPanel;
    // End of variables declaration//GEN-END:variables

    private Configuration configuration = null;
    /** Creates a new instance of CreateFiltersPanel 
     * @param configuration configuration which contains filters to be shown
     */
    public CreateFiltersPanel(Configuration configuration) throws LaserConsoleException {
   		super();
        
        System.out.println("### CreateFiltersPanel::CreateFiltersPanel()");
        this.configuration = configuration;
        initComponents();
    }
  
    /** This method initializes all components for this panel 
     * @param filters list with defined filtes, this <code>List</code> contains
     *      <code>FilterItemBean</code> objects
     */
    private void initComponents() throws LaserConsoleException {
        java.awt.GridBagConstraints gridBagConstraints;
        
        System.out.println("### CreateFiltersPanel::initComponents");
        headerPanel = new javax.swing.JPanel();
        centralPanel = new javax.swing.JPanel();
        staticInfoPanel = new javax.swing.JPanel();
        filtrListPanel = new javax.swing.JPanel();
        tableListPanel = new javax.swing.JPanel();
        tableListSouthPanel = new javax.swing.JPanel();
        buttonPanel = new javax.swing.JPanel();
        addButton1 = new javax.swing.JButton();
        southPanel = new javax.swing.JPanel();

        setLayout(new java.awt.BorderLayout());

        setPreferredSize(new java.awt.Dimension(500, 400));
        add(headerPanel, java.awt.BorderLayout.NORTH);

        centralPanel.setLayout(new java.awt.GridLayout(0, 1));

        centralPanel.setPreferredSize(new java.awt.Dimension(450, 194));
        staticInfoPanel.setLayout(new java.awt.BorderLayout());

        staticInfoPanel.setBorder(new javax.swing.border.TitledBorder("Filters"));
        filtrListPanel.setLayout(new java.awt.BorderLayout());

        filtrListPanel.setBorder(new javax.swing.border.EtchedBorder());
        
        // init created filters explorer
       
       
        List filters = java.util.Arrays.asList(
                configuration.getSelection().getFilterSelection().list());
        createdFiltersExpl = new CreatedFiltersExplorer(filters);
       
        tableListPanel.add(createdFiltersExpl);

        filtrListPanel.add(tableListPanel, java.awt.BorderLayout.CENTER);

        tableListSouthPanel.setLayout(new java.awt.BorderLayout());
        /*
        updateButton = ActionUtils.createJButton(EditFilterAction.class);
        //updateButton.setPreferredSize(new java.awt.Dimension(87, 27));
        buttonPanel.add(updateButton);

        deleteButton = ActionUtils.createJButton(DeleteFilterAction.class);
        buttonPanel.add(deleteButton);

        deleteAllButton = ActionUtils.createJButton(DeleteAllFiltersAction.class); 
        buttonPanel.add(deleteAllButton);
        */
        tableListSouthPanel.add(buttonPanel, java.awt.BorderLayout.EAST);

        filtrListPanel.add(tableListSouthPanel, java.awt.BorderLayout.SOUTH);

        staticInfoPanel.add(filtrListPanel, java.awt.BorderLayout.CENTER);

        createFilterItem = new SingleFilterItem();
        createFilterItem.setFitlerExplorer(createdFiltersExpl);
        
        // register actions in register
        //AppRegister.getInstance().registerObject(SingleFilterItem.class.getName(),
        //                                        createFilterItem);
        //AppRegister.getInstance().registerObject(
        //                        CreatedFiltersExplorer.class.getName(), 
        //                        createdFiltersExpl);
        //DeleteFilterAction.setContext(createdFiltersExpl);
        //DeleteAllFiltersAction.setContext(createdFiltersExpl);

        staticInfoPanel.add(createFilterItem, java.awt.BorderLayout.NORTH);

        centralPanel.add(staticInfoPanel);

        add(centralPanel, java.awt.BorderLayout.CENTER);

        southPanel.setLayout(new java.awt.BorderLayout());

        add(southPanel, java.awt.BorderLayout.SOUTH);
    }
    
    /**
     * This method returns all defined filters
     * @param <code>List</code> with <code>FilterItemBean</code>objects
     */
    public List getDefinedFilters() {
        return createdFiltersExpl.getDefinedFilters();
    }
    
    /**
     * This method resets this panel
     */
    public void reset() {
        createdFiltersExpl.removeAllFilters();
    }
    
    /**
     * @param node filter to be removed
     */
    public void removeFilter(GPNode node) {
        createdFiltersExpl.removeFilter(node);
        FilterItemBean filterBean = (FilterItemBean) node.getBean();
        createFilterItem.setMode(filterBean.getName(),
                filterBean.getOperator(), filterBean.getValue());
    }
    
    /** */
    public void removeAllFilters() {
        createdFiltersExpl.removeAllFilters();
        createFilterItem.resetPanel();
    }


    public void setInUpdateMode(String name, String operator, String value, 
                                String buttonName) {
     
        createFilterItem.setInUpdateMode(name, operator, value, buttonName);
    }


    /**
     * used when configuration is reloaded (ConfigurationLoadAction)
     * @param newConfiguration new configuration to be shown
     */
    public void updateFilterSelection(Configuration newConfiguration)
        throws LaserConsoleException {
            
        configuration = newConfiguration; 
        createdFiltersExpl.removeAllFilters(); 

        List filters = java.util.Arrays.asList(
                newConfiguration.getSelection().getFilterSelection().list());
        for (Iterator iter = filters.iterator(); iter.hasNext(); ) {
            Filter filter = (Filter) iter.next();
            try {
                createdFiltersExpl.addFilter(filter);
            } catch (java.beans.IntrospectionException ie) {
                logger.error(ie, ie.fillInStackTrace());
            }
        }

    }


}
