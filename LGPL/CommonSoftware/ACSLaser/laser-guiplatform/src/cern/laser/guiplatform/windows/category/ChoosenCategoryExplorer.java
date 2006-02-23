
/*
 * ChoosenCategoryExplorer.java
 *
 * Created on March 7, 2003, 3:40 PM
 */

package cern.laser.guiplatform.windows.category;

import java.beans.IntrospectionException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;
import org.openide.nodes.Children;
import org.openide.nodes.Node;

import cern.gp.beans.BeanSupport;
import cern.gp.nodes.GPNode;
import cern.gp.nodes.NodeFactory;
import cern.gp.nodes.children.ChildrenMapManager;
import cern.gp.nodes.children.NodeCollection;
import cern.gp.nodes.children.NodeList;
import cern.gp.nodes.children.NodeMap;
import cern.gp.nodes.impl.GPBeanNode;
import cern.laser.client.data.Category;
import cern.laser.guiplatform.category.CategoryBean;
import cern.laser.guiplatform.util.LogFactory;

/**
 * Explorer with categories choosen by given user
 * @author  pawlowsk
 */
public class ChoosenCategoryExplorer extends cern.gp.explorer.ListExplorer {
    
    
    private static final Logger logger =
    LogFactory.getLogger(ChoosenCategoryExplorer.class.getName());
    
    /** user node manager */
    private ChoosenCategoryNodeManager nodeManager = null;
    
    /** Creates a new instance of ChoosenCategoryExplorer
     *
     * @param choossenCategories list
     */
    public ChoosenCategoryExplorer(List choosenCategories) {
        super();
        nodeManager = new ChoosenCategoryNodeManager(
        choosenCategories);
        try {
            setRootNode(NodeFactory.createNode(new Object(), nodeManager));
        } catch (IntrospectionException e) {
            logger.error(e, e.fillInStackTrace());
        }
        //getTreeView().setRootVisible(false);
    }
    
    public boolean contains( GPNode gpNode) {
        try {
            CategoryBean cat = (CategoryBean)((CategoryBean) gpNode.getBean()).clone();
            return nodeManager.contains((BeanSupport) cat);
        }
        catch ( CloneNotSupportedException ex) {
            return false;
        }
    }
    // add, remove node method
    /**
     * This method adds category to ChoosenCategoryExplorer only once
     */
    public void addCategory(BeanSupport node) throws IntrospectionException {
        nodeManager.addCategory(node);
    }
    public void removeCategory(GPNode node) {
        nodeManager.removeCategory(node);
        ((CategoryBean) node.getBean()).setSelected(false);
    }
    
    public void removeAllCategories() {
        nodeManager.removeAllCategories();
    }
    /**
     * This is recursive method. This methods add GPNode and all children
     * to choosen category explorer.
     */
    public void addCategory(GPNode gpNode) throws IntrospectionException, CloneNotSupportedException {
        addCategoryWithoutChildren(gpNode);
        addSubCategories(gpNode);
        
    }
    
    /**
     * This methods adds Category
     * to choosen category explorer without children.
     */
    public void addCategoryWithoutChildren(GPNode gpNode) throws IntrospectionException, CloneNotSupportedException {
        CategoryBean cat = (CategoryBean)((CategoryBean) gpNode.getBean()).clone();
        addCategory((BeanSupport) cat);
        ((CategoryBean) gpNode.getBean()).setSelected(true);
    }
    
    public void addSubCategories(GPNode gpNode) throws IntrospectionException, CloneNotSupportedException {
        
        CategoryBean cat = (CategoryBean)((CategoryBean) gpNode.getBean()).clone();
        
        // TODO: change activatedNodes[ix].getPeerNode().isLeaf();
        // to activatedNodes[ix].isLeaf();
        if ( !gpNode.getPeerNode().isLeaf() ){      // node is not leaf
            
            //test
            Children children = gpNode.getPeerNode().getChildren();
            logger.debug("before getChildren test 5");
            for (java.util.Enumeration e = children.nodes(); e.hasMoreElements(); ) {
                logger.debug(((org.openide.nodes.Node) e.nextElement()).getName());
            }
            logger.debug("after getChildren test 5");
            
            
            
            NodeCollection nodeColl = gpNode.getNodeCollection();
            CategoryTreeExplorer.CategoryTreeNodeManager manager =
            (CategoryTreeExplorer.CategoryTreeNodeManager)nodeColl.getChildrenManager();
            
            NodeList nodeList = manager.getNodeList();
            
            if ( nodeList != null ) {   // it means that tree is expanded
                Iterator iter = nodeList.iterator();
                while ( iter.hasNext() ) {
                    GPNode node = (GPNode) iter.next();
                    this.addCategory(node);
                }
            }
        }
    }
    public void removeCategoryWithoutChildren(GPNode gpNode) throws IntrospectionException, CloneNotSupportedException {
        removeCategory(gpNode);
    }
    public void removeSubCategories(GPNode gpNode) throws IntrospectionException, CloneNotSupportedException {
        removeSubCategories(gpNode, false);
    }
    public void removeSubCategories(GPNode gpNode, boolean removeThisNode) throws IntrospectionException, CloneNotSupportedException {
        CategoryBean cat = (CategoryBean)((CategoryBean) gpNode.getBean()).clone();
        
        if ( removeThisNode ) {
            removeCategory(gpNode);
        }
        
        // TODO: change activatedNodes[ix].getPeerNode().isLeaf();
        // to activatedNodes[ix].isLeaf();
        if ( !gpNode.getPeerNode().isLeaf() ){      // node is not leaf
            
            //test
            Children children = gpNode.getPeerNode().getChildren();
            logger.debug("before getChildren test 5");
            for (java.util.Enumeration e = children.nodes(); e.hasMoreElements(); ) {
                logger.debug(((org.openide.nodes.Node) e.nextElement()).getName());
            }
            logger.debug("after getChildren test 5");
            
            
            
            NodeCollection nodeColl = gpNode.getNodeCollection();
            CategoryTreeExplorer.CategoryTreeNodeManager manager =
            (CategoryTreeExplorer.CategoryTreeNodeManager)nodeColl.getChildrenManager();
            
            NodeList nodeList = manager.getNodeList();
            
            if ( nodeList != null ) {   // it means that tree is expanded
                Iterator iter = nodeList.iterator();
                while ( iter.hasNext() ) {
                    GPNode node = (GPNode) iter.next();
                    this.removeSubCategories(node, true);
                }
            }
        }
    }
    
    /**
     * This method returns all categories choosen by user.
     */
    /*
    public GPNode [] getChoosenCategory() {
        return nodeManager.getChoosenCategories();
    }
     */
    /** */
    public Collection getChoosenCategories() {
        return nodeManager.getChoosenCategories();
    }
    
    /**
     * used when user want to load new cofiguration (ConfiguratonLoad action)
     * @param newCategories <code>List</code> with <code>Category</code>
     * objects
     */
    public void updateChoosenCategories(List newCategories) {
        nodeManager.removeAllCategories();
        
        try {
            for (Iterator iter = newCategories.iterator(); iter.hasNext(); ) {
                Category category = (Category) iter.next();
                CategoryBean bean = new CategoryBean(category);
                bean.setCloned();
                nodeManager.addCategory(bean);
            }
        } catch (IntrospectionException ex) {
            logger.error(ex, ex.fillInStackTrace());
        }
        
        
        
    }
    
    /**********************************************************************
     *  nodeNanager class
     *
     ************************************************************************/
    private class ChoosenCategoryNodeManager implements ChildrenMapManager {
        
        private final Logger logger =
        LogFactory.getLogger(ChoosenCategoryNodeManager.class.getName());
        
        /** choosen conf */
        private NodeMap categoryNodeMap = null;
        
        private List categoryList = null;
        /** logged user name */
        private String userName = null;
        
        /**
         * @param categoryList <code>List</code> with <code>Category</code>
         *          objects
         */
        public ChoosenCategoryNodeManager(List categoryList) {
            this.categoryList = categoryList;
        }
        
        public boolean contains( BeanSupport bean ) {
            return categoryNodeMap.keySet().contains(bean.getName());
        }
        public void initChildrenMap(cern.gp.nodes.children.NodeMap nodeMap) {
            logger.debug(getClass().getName() + "initChildrenList(NodeMao nodeMap)" );
            categoryNodeMap = nodeMap;
            if ( categoryList == null ) {
                logger.debug(" user do not have choosen category !!!");
                return;
            }
            try {
                Iterator iter = categoryList.iterator();
                while ( iter.hasNext() ) {
                    Category category = (Category) iter.next();
                    CategoryBean bean = new CategoryBean(category);
                    bean.setCloned();
                    GPNode node = NodeFactory.createNode(bean);
                    categoryNodeMap.addNode(node);
                }
            } catch (IntrospectionException ex) {
                logger.error(ex, ex.fillInStackTrace());
            }
        }
        
        public java.util.Comparator getComparator() {
            return new CategoryComparator();
        }
        
        
        // add remove method
        /**
         * This method adds category to ChoosenCategoryExplorer only once
         */
        public void addCategory(BeanSupport child) throws IntrospectionException {
            if ( !categoryNodeMap.keySet().contains(child.getName()) )
                categoryNodeMap.addNode(child.getName(), NodeFactory.createNode(child));
        }
        public void removeCategory(GPNode node) {
            Node nbNode = node.getPeerNode();
            categoryNodeMap.removeNode(node.getName());
            /*
            try {
                nbNode.destroy();
            } catch (java.io.IOException ie) {
                logger.error(ie, ie.fillInStackTrace());
            }
             */
        }
        
        public void removeAllCategories() {
            
            // this is done because GP NodeMap does not have velues() method
            // so I can not do something like this:
            // nodeMap.values.itertor()
            // synchronized(nodeMap) {
            //  iterator.remove();
            // }
            // and I have to use nodeMap.removeNodes(Object[] keys) method
            java.util.List nbNodes = new java.util.ArrayList();
            for(java.util.Iterator iter = categoryNodeMap.keySet().iterator();
            iter.hasNext(); ) {
                org.openide.nodes.Node node =
                categoryNodeMap.getNode(iter.next()).getPeerNode();
                nbNodes.add(node);
            }
            categoryNodeMap.removeNodes(categoryNodeMap.keySet().toArray());
            
            for(int i = 0; i < nbNodes.size(); i++) {
                org.openide.nodes.Node node = (org.openide.nodes.Node) nbNodes.remove(i);
                try {
                    node.destroy();
                } catch (java.io.IOException ie) {
                    logger.error(ie, ie.fillInStackTrace());
                }
            }
            
            
        }
        //public void addAllCategory
        /**
         * This method return choosen categories.
         */
        /*
        public GPNode [] getChoosenCategories() {
            java.util.Set set = categoryNodeMap.keySet();
            GPNode [] nodes = new GPNode[set.size()];
            java.util.Iterator iter = set.iterator();
            int counter = 0;
            while ( iter.hasNext() ) {
                nodes[counter] = categoryNodeMap.getNode(iter.next());
                counter++;
            }
            return nodes;
        }
         */
        /** */
        public Collection getChoosenCategories() {
            Collection categories = new ArrayList();
            java.util.Set set = categoryNodeMap.keySet();
            //categoryNodeMap.removeNodes(categoryNodeMap.keySet().toArray());
            
            java.util.Iterator iter = set.iterator();
            while ( iter.hasNext() ) {
                categories.add(((CategoryBean) categoryNodeMap.getNode(iter.next()).getBean()).getCategory());
            }
            
            return categories;
            
        }
        /**
         * This method returns list with nodes
         */
    } // end private ChoosenCategoryNodeManager
    
    /**
     * A comparator class
     */
    class CategoryComparator implements Comparator {
        public int compare(Object one, Object two) {
            if (!(one instanceof GPBeanNode) || !(two instanceof GPBeanNode)) {
                return 0;
            }
            
            // both one and two are SimpleDemoBeans, convert them for readability of code
            GPBeanNode beanOne = (GPBeanNode) one;
            GPBeanNode beanTwo = (GPBeanNode) two;
            
            // isolate the index at the end of the String returned by getName() and convert it to an int:
            String beanOneName = beanOne.getName();
            String beanTwoName = beanTwo.getName();
            
            return beanOneName.compareTo(beanTwoName);
        }
    }
}   // end ChoosenCategoryExplorer



