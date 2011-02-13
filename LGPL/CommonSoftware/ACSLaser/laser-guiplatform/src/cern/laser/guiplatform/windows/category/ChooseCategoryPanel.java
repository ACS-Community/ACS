/*
 * ChooseCategoryPanel.java
 *
 * Created on February 21, 2003, 5:18 PM
 */

package cern.laser.guiplatform.windows.category;

//import cern.gp.actions.support.ActionUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.beans.IntrospectionException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.apache.log4j.Logger;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.util.actions.SystemAction;

import alma.acs.container.ContainerServicesBase;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.children.NodeCollection;
import cern.gp.nodes.children.NodeList;
import cern.laser.client.LaserException;
import cern.laser.client.data.Category;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.guiplatform.actions.category.AddCategoryAction;
import cern.laser.guiplatform.actions.category.AddSubCategoriesAction;
import cern.laser.guiplatform.actions.category.RemoveCategoryAction;
import cern.laser.guiplatform.actions.category.RemoveSubCategoriesAction;
import cern.laser.guiplatform.category.CategoryBean;
import cern.laser.guiplatform.category.CategoryBrowsingHandlerFactory;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.util.actions.ActionUtils;

/**
 *
 * @author  pawlowsk
 */
public class ChooseCategoryPanel extends javax.swing.JPanel {
    
    private static Logger logger =
    LogFactory.getLogger(ChooseCategoryPanel.class.getName());
    
    /** Panel where are buttons (add, add all, remove, remove all)*/
    private JPanel buttonsPanel = null;
    
    
    /** configuration variables */
    private final static String configureTreeName = "Available categories";
    private final static String choosenConfigurationName = "Choosen categories";
    
    
    /** tree explorer which dispaly category tree */
    private CategoryTreeExplorer categoryTreeExpl = null;
    /** list explorer with choosen categories */
    private ChoosenCategoryExplorer choosenCategoryExpl = null;
    
    /** configuration to be shown */
    private Configuration configuration = null;
    
    private Category [] choosenCategories = null;
    
    private final ContainerServicesBase contSvcs;
    /**
     * Creates a new instance of ChooseCategoryPanel
     * @param configuration configuration to be shown
     */
    public ChooseCategoryPanel(Configuration configuration, ContainerServicesBase contSvcs) {
        super();
        this.configuration = configuration;
        this.contSvcs=contSvcs;
        initComponents();
    }
    public ChooseCategoryPanel(Category [] choosenCategories, ContainerServicesBase contSvcs) {
        super();
        this.choosenCategories = choosenCategories;
        this.contSvcs=contSvcs;
        initComponents();
    }
    /**
     * Created a new instance of ChooseCategoryPanel
     * @param
     */
    
    
    private void initComponents() {
        
        setLayout(new GridLayout(0, 3));
        
        try {
            
            // start: choosen category panel
            Category [] categories;
            if (configuration!=null)
                categories = configuration.getSelection().getCategorySelection().list();
            else
                categories = choosenCategories;
            choosenCategoryExpl = createChoosenCategoryList(Arrays.asList(categories));
            choosenCategoryExpl.add(new JLabel(choosenConfigurationName), BorderLayout.NORTH);
            // end: choosen category panel
            
            // start: category tree
            categoryTreeExpl = createCategoryTree();
            categoryTreeExpl.add(new JLabel(configureTreeName), BorderLayout.NORTH);
            // end: category tree
            
            // start: button panel
            JPanel panel = new JPanel();
            panel.setLayout(new BorderLayout());
            
            buttonsPanel = new JPanel();
            buttonsPanel.setPreferredSize(new Dimension(200, 400));
            GridBagLayout buttonLayout = new GridBagLayout();
            GridBagConstraints buttonConstr = new GridBagConstraints();
            buttonsPanel.setLayout(buttonLayout);
            
            /*
            JButton addButton = new JButton(
                    ActionUtils.createActionForComp(
                        categoryTreeExpl,
                        SystemAction.get(CategoryCopyOverNodesAction.class)));
             
            addButton.setPreferredSize(new Dimension(200, 30));
            buttonConstr.gridx = 0;
            buttonConstr.gridy = 0;
            //buttonConstr.weighty = 0.75;
            //buttonConstr.anchor = GridBagConstraints.NORTH;
            buttonConstr.insets = new Insets(2,0,0,0);  //top padding
            buttonLayout.setConstraints(addButton, buttonConstr);
            buttonsPanel.add(addButton);
             
            //JButton removeButton = ActionUtils.createJButton(
            //                            DeleteChoosenCategoryAction.class);
            JButton removeButton = new JButton(
                    ActionUtils.createActionForComp(
                        choosenCategoryExpl,
                        SystemAction.get(DeleteChoosenCategoryAction.class)));
             
            removeButton.setPreferredSize(new Dimension(200, 30));
            buttonConstr.gridx = 0;
            buttonConstr.gridy = 1;
            //buttonConstr.weighty = 0.75;
            buttonLayout.setConstraints(removeButton, buttonConstr);
            buttonsPanel.add(removeButton);
             
            //JButton addAllButton = ActionUtils.createJButton(
            //                                CategoryCopyAllOverNodesAction.class);
            JButton addAllButton = new JButton(
                    ActionUtils.createActionForComp(
                        categoryTreeExpl,
                        SystemAction.get(CategoryCopyAllOverNodesAction.class)));
             
            addAllButton.setPreferredSize(new Dimension(200, 30));
            buttonConstr.gridx = 0;
            buttonConstr.gridy = 2;
            buttonConstr.insets = new Insets(10,0,0,0);  //top padding
            buttonLayout.setConstraints(addAllButton, buttonConstr);
            buttonsPanel.add(addAllButton);
             
             
            //JButton removeAllButton = ActionUtils.createJButton(
            //                                DeleteAllChoosenCategoryAction.class);
            JButton removeAllButton = new JButton(
                    ActionUtils.createActionForComp(
                        choosenCategoryExpl,
                        SystemAction.get(DeleteAllChoosenCategoryAction.class)));
             
            removeAllButton.setPreferredSize(new Dimension(200, 30));
            buttonConstr.gridx = 0;
            buttonConstr.gridy = 3;
            buttonConstr.insets = new Insets(2,0,0,0);  //top padding
            buttonLayout.setConstraints(removeAllButton, buttonConstr);
            buttonsPanel.add(removeAllButton);
             */
            // Add Category Button
            
            JButton addCategoryButton = new JButton(
            ActionUtils.createActionForComp(
            categoryTreeExpl,
            SystemAction.get(AddCategoryAction.class)));
            
            addCategoryButton.setPreferredSize(new Dimension(200, 30));
            buttonConstr.gridx = 0;
            buttonConstr.gridy = 0;
            buttonConstr.insets = new Insets(10,0,0,0);  //top padding
            buttonLayout.setConstraints(addCategoryButton, buttonConstr);
            buttonsPanel.add(addCategoryButton);
            
            // Add SubCategories Button
            
            JButton addSubCategoriesButton = new JButton(
            ActionUtils.createActionForComp(
            categoryTreeExpl,
            SystemAction.get(AddSubCategoriesAction.class)));
            
            addSubCategoriesButton.setPreferredSize(new Dimension(200, 30));
            buttonConstr.gridx = 0;
            buttonConstr.gridy = 1;
            buttonConstr.insets = new Insets(2,0,0,0);  //top padding
            buttonLayout.setConstraints(addSubCategoriesButton, buttonConstr);
            buttonsPanel.add(addSubCategoriesButton);
            
            
            // Remove Category Button
            
            JButton removeCategoryButton = new JButton(
            ActionUtils.createActionForComp(
            categoryTreeExpl,
            SystemAction.get(RemoveCategoryAction.class)));
            
            removeCategoryButton.setPreferredSize(new Dimension(200, 30));
            buttonConstr.gridx = 0;
            buttonConstr.gridy = 2;
            buttonConstr.insets = new Insets(10,0,0,0);  //top padding
            buttonLayout.setConstraints(removeCategoryButton, buttonConstr);
            buttonsPanel.add(removeCategoryButton);
            
            // Remove SubCategories Button
            
            JButton removeSubCategoriesButton = new JButton(
            ActionUtils.createActionForComp(
            categoryTreeExpl,
            SystemAction.get(RemoveSubCategoriesAction.class)));
            
            removeSubCategoriesButton.setPreferredSize(new Dimension(200, 30));
            buttonConstr.gridx = 0;
            buttonConstr.gridy = 4;
            buttonConstr.insets = new Insets(2,0,0,0);  //top padding
            buttonLayout.setConstraints(removeSubCategoriesButton, buttonConstr);
            buttonsPanel.add(removeSubCategoriesButton);
            
            // end: buttons panel
            
            add(categoryTreeExpl);
            
            panel.add(buttonsPanel, BorderLayout.NORTH);
            add(panel);
            add(choosenCategoryExpl);
            
        } catch (LaserConsoleException lce) {
            logger.debug(lce, lce.getRootCause().fillInStackTrace());
            NotifyDescriptor d = new NotifyDescriptor.Message("Problem with connection " +
            "business layer.\n Try again later.\n See log file.");
            DialogDisplayer.getDefault().notify(d);
        } catch (LaserException le) {
            LogFactory.logException(logger, le);
            logger.debug(le, le.fillInStackTrace());
            logger.debug(le, le.getRootCause().fillInStackTrace());
            NotifyDescriptor d = new NotifyDescriptor.Message("Problem with connection " +
            "business layer.\n Try again later.");
            DialogDisplayer.getDefault().notify(d);
        }
    }
    
    /**
     * This method creates category tree
     *
     */
    private CategoryTreeExplorer createCategoryTree() throws LaserException {
        logger.debug("createCategoryRoot()");
        CategoryBrowsingHandler handler = CategoryBrowsingHandlerFactory.getHandler(contSvcs);
        Category categoryRoot = handler.getCategoryTreeRoot();
        
        logger.debug("categoryRoot="+categoryRoot);
        
        CategoryTreeExplorer categoryTreeTemp = new CategoryTreeExplorer(categoryRoot,contSvcs);
        logger.debug("categoryTree="+categoryTreeTemp);
        
        /*
        JPanel panel = ActionUtils.createJButtonPanel(
            new Class[] {
                cern.laser.guiplatform.categorybeans.CategoryCopyOverNodesAction.class,
                cern.laser.guiplatform.categorybeans.CategoryCopyAllOverNodesAction.class
            }
        );
         */
        return categoryTreeTemp;
    }
    /**
     * This method creates list with choosen categories for given user
     * @param categories <code>List</code> with <code>Category</code> objects
     */
    private ChoosenCategoryExplorer createChoosenCategoryList(List categories) {
        ChoosenCategoryExplorer choosenCategoryExpl =
        new ChoosenCategoryExplorer(categories);
        /*
        JPanel panel = ActionUtils.createJButtonPanel(
            new Class[] {
                cern.laser.guiplatform.categorybeans.DeleteChoosenCategoryAction.class,
                cern.laser.guiplatform.categorybeans.DeleteAllChoosenCategoryAction.class
            }
        );
         */
        return choosenCategoryExpl;
    }
    
    /************************************************************************
     *   Methods for communications with other windows
     *************************************************************************/
    /**
     * This method returns list with categories choosen by user
     */
    /*
    public GPNode[] getChoosenCategory() {
     
        //ExplorerManager manager = this.choosenCategoryExpl.getExplorerManager();
        //return manager.getSelectedNodes();
     
        return this.choosenCategoryExpl.getChoosenCategory();
    }
     */
    /** methods for setting category selection
     * @return <code>Collection</code> with choosen categories
     */
    public Collection getChoosenCategories() {
        return choosenCategoryExpl.getChoosenCategories();
    }
    public Category [] getChoosenCategoriesArray() {
        Collection choosenCategoryBeans = getChoosenCategories();
        Category [] categories = new Category[ choosenCategoryBeans.size() ];
        int i = 0;
        for ( Iterator iter = choosenCategoryBeans.iterator(); iter.hasNext(); i++) {
            //logger.debug("class = "+iter.next().getClass().getName() );
            Object item = iter.next();
            if ( item instanceof Category) {
                categories[i] = (Category) item;
            }
            else {
                categories[i] = ((CategoryBean) item).getCategory();
            }
        }
        return categories;
    }
    public void updateCategoryTreeExplorer() {
        updateSelection();
        updateState();
    }
    public void loadAllCategoriesInTreeExplorer() {        
        categoryTreeExpl.expandAndColapseTree();
        updateCategoryTreeExplorer();        
    }
    /**
     * This method reset this panel
     */
    public void reset() {
        choosenCategoryExpl.removeAllCategories();
    }
    public void addCategory(GPNode node) throws IntrospectionException, CloneNotSupportedException {
        choosenCategoryExpl.addCategory(node);
    }
    
    public void addCategoryWithoutChildren(GPNode node) throws IntrospectionException, CloneNotSupportedException {
        choosenCategoryExpl.addCategoryWithoutChildren(node);
    }
    
    public void addSubCategories(GPNode node) throws IntrospectionException, CloneNotSupportedException {
        choosenCategoryExpl.addSubCategories(node);
    }
    
    public void removeCategoryWithoutChildren(GPNode node) throws IntrospectionException, CloneNotSupportedException {
        choosenCategoryExpl.removeCategoryWithoutChildren(node);
    }
    
    public void removeSubCategories(GPNode node) throws IntrospectionException, CloneNotSupportedException {
        choosenCategoryExpl.removeSubCategories(node);
    }
    public void removeCategory(GPNode node) {
        choosenCategoryExpl.removeCategory(node);
    }
    
    public void removeAllCategories() {
        choosenCategoryExpl.removeAllCategories();
    }
    
    public void updateCategorySelection(Configuration newConfiguration) throws LaserConsoleException {
        Category [] newCategories =
        newConfiguration.getSelection().getCategorySelection().list();
        choosenCategoryExpl.updateChoosenCategories(Arrays.asList(newCategories));
        
        updateCategoryTreeExplorer();
        
        //categoryTreeExpl.firePropertyChange("",null,null);
    }
    
    public void updateState() {
        try {
            GPNode root = this.categoryTreeExpl.getRootNode();
            updateState(root);
        }
        catch (CloneNotSupportedException ex) {}
    }
    
    private int updateState(GPNode gpNode) throws CloneNotSupportedException {
        CategoryBean cat = (CategoryBean) gpNode.getBean();
        int thisNodeState;
        if ( cat.isSelected()) {
            thisNodeState = CategoryBean.STATE_CHILDREN_ALL_SELECTED;
        }
        else {
            thisNodeState = CategoryBean.STATE_CHILDREN_NOT_SELECTED;
        }
        if( ! gpNode.getPeerNode().isLeaf() ) {
            NodeCollection nodeColl = gpNode.getNodeCollection();
            CategoryTreeExplorer.CategoryTreeNodeManager manager =
            (CategoryTreeExplorer.CategoryTreeNodeManager)nodeColl.getChildrenManager();
            
            NodeList nodeList = manager.getNodeList();
            
            boolean firstChildren = true;
            if ( nodeList != null ) {
                Iterator iter = nodeList.iterator();
                while ( iter.hasNext() ) {
                    GPNode childrenNode = (GPNode) iter.next();
                    int childrenNodeState = updateState(childrenNode);
                    if (firstChildren) {
                        firstChildren = false;
                        thisNodeState = childrenNodeState;
                    }
                    else {
                        switch(childrenNodeState){
                            case CategoryBean.STATE_CHILDREN_NOT_SELECTED:
                                switch(thisNodeState){
                                    case CategoryBean.STATE_CHILDREN_NOT_SELECTED:
                                        thisNodeState = CategoryBean.STATE_CHILDREN_NOT_SELECTED;
                                        break;
                                    case CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED:
                                        thisNodeState = CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED;
                                        break;
                                    case CategoryBean.STATE_CHILDREN_ALL_SELECTED:
                                        thisNodeState = CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED;
                                        break;
                                }
                                break;
                            case CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED:
                                switch(thisNodeState){
                                    case CategoryBean.STATE_CHILDREN_NOT_SELECTED:
                                        thisNodeState = CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED;
                                        break;
                                    case CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED:
                                        thisNodeState = CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED;
                                        break;
                                    case CategoryBean.STATE_CHILDREN_ALL_SELECTED:
                                        thisNodeState = CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED;
                                        break;
                                }
                                break;
                            case CategoryBean.STATE_CHILDREN_ALL_SELECTED:
                                switch(thisNodeState){
                                    case CategoryBean.STATE_CHILDREN_NOT_SELECTED:
                                        thisNodeState = CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED;
                                        break;
                                    case CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED:
                                        thisNodeState = CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED;
                                        break;
                                    case CategoryBean.STATE_CHILDREN_ALL_SELECTED:
                                        thisNodeState = CategoryBean.STATE_CHILDREN_ALL_SELECTED;
                                        break;
                                }
                                break;
                        }
                    }
                }
            }
        }
        
        cat.setChildrenSelectionState(thisNodeState);
        
        if ( (! cat.isSelected()) && thisNodeState == CategoryBean.STATE_CHILDREN_ALL_SELECTED ) {
            return CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED;
        }
        else {
            if ( cat.isSelected() && thisNodeState == CategoryBean.STATE_CHILDREN_NOT_SELECTED ) {
                return CategoryBean.STATE_CHILDREN_PARTIALY_SELECTED;
            }
            else {                
                return thisNodeState;
            }
        }
    }
    public void updateSelection() {
        try {
            GPNode root = this.categoryTreeExpl.getRootNode();
            updateSelection(root);
        }
        catch (CloneNotSupportedException ex) {
            
        }
    }
    
    private void updateSelection(GPNode gpNode) throws CloneNotSupportedException {
        CategoryBean cat = (CategoryBean) gpNode.getBean();
        
        // set the selection
        if ( this.choosenCategoryExpl.contains(gpNode) ){
            cat.setSelected(true);
        }
        else {
            cat.setSelected(false);
        }
        
        // set state recursively on children nodes
        if( ! gpNode.getPeerNode().isLeaf() ) {
            NodeCollection nodeColl = gpNode.getNodeCollection();
            CategoryTreeExplorer.CategoryTreeNodeManager manager =
            (CategoryTreeExplorer.CategoryTreeNodeManager)nodeColl.getChildrenManager();
            
            NodeList nodeList = manager.getNodeList();
            
            if ( nodeList != null ) {
                Iterator iter = nodeList.iterator();
                while ( iter.hasNext() ) {
                    GPNode childrenNode = (GPNode) iter.next();
                    updateSelection( childrenNode );
                }
            }
        }
    }
}
