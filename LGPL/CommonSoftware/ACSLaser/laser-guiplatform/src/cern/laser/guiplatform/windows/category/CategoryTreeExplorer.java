
/*
 * CategoryTreeExplorer.java
 *
 * Created on March 10, 2003, 3:07 PM
 */

package cern.laser.guiplatform.windows.category;



import java.beans.IntrospectionException;
import java.util.Collection;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;

import javax.swing.JTree;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import org.apache.log4j.Logger;

import alma.acs.container.ContainerServicesBase;

import cern.gp.nodes.NodeFactory;
import cern.gp.nodes.children.ChildrenListManager;
import cern.gp.nodes.children.NodeList;
import cern.gp.nodes.impl.GPBeanNode;
import cern.laser.client.LaserException;
import cern.laser.client.data.Category;
import cern.laser.guiplatform.category.CategoriesPreLoader;
import cern.laser.guiplatform.category.CategoryBean;
import cern.laser.guiplatform.category.CategoryBrowsingHandlerFactory;
import cern.laser.guiplatform.util.LogFactory;




/**
 * This is category explorer.
 *
 * @author  pawlowsk
 */
public class CategoryTreeExplorer extends cern.gp.explorer.TreeExplorer {
    
    /** logger*/
    private static Logger logger =
    LogFactory.getLogger(CategoryTreeNodeManager.class.getName());
    /** category node manager */
    private CategoryTreeNodeManager nodeManager = null;
    
    private final ContainerServicesBase contSvcs;
    
    /** Creates a new instance of CategoryTreeExplorer */
    public CategoryTreeExplorer(Category rootCategory, ContainerServicesBase contSvcs) {
        super();
        nodeManager = new CategoryTreeNodeManager(rootCategory);
        this.contSvcs=contSvcs;
        try {
            setRootNode(NodeFactory.createNode(new CategoryBean(rootCategory), nodeManager));
        } catch (IntrospectionException e) { e.printStackTrace() ;}
        //expandAll(getTreeAccess().getTree(), true);
        //expandAndColapseTree();
    }
    
    public void expandAndColapseTree() {
        logger.debug("expandAndColapseTree");
        int rc = 0;
        do {
            rc = getTreeAccess().getTree().getRowCount();
            for (int x=rc;x>=0;x--){
                getTreeAccess().getTree().expandRow(x);
            }
        }
        while (rc != getTreeAccess().getTree().getRowCount()); // Until there are no more rows to get. i.e the last pass expanded no more new nodes.
        
        //getTreeAccess().getTree().collapseRow( 0 );
        expandAll( false );
    }
    // If expand is true, expands all nodes in the tree.
    // Otherwise, collapses all nodes in the tree.
    public void expandAll( boolean expand) {
        JTree tree = getTreeAccess().getTree();
        TreeNode root = (TreeNode)tree.getModel().getRoot();
        
        // Traverse tree from root
        expandAll(tree, new TreePath(root), expand);
    }
    private void expandAll(JTree tree, TreePath parent, boolean expand) {
        // Traverse children
        //logger.debug("tree="+tree);
        //logger.debug("parent="+parent);
        //logger.debug("expand="+expand);
        TreeNode node = (TreeNode)parent.getLastPathComponent();
        //logger.debug("node="+node);
        //logger.debug("node.getChildCount()="+node.getChildCount());
        if (node.getChildCount() >= 0) {
            for (Enumeration e=node.children(); e.hasMoreElements(); ) {
                TreeNode n = (TreeNode)e.nextElement();
                //logger.debug("n="+n);
                TreePath path = parent.pathByAddingChild(n);
                //logger.debug("path="+path);
                expandAll(tree, path, expand);
            }
        }
        
        // Expansion or collapse must be done bottom-up
        if (expand) {
            tree.expandPath(parent);
        } else {
            tree.collapsePath(parent);
        }
    }
    /*public void expandSelectedPaths(GPNode node) {
        getTreeAccess().getTreeView().expandAll();
        getTreeAccess().getTreeView().collapseNode(
    }*/
    /*
    private void initActions() {
        this.getActionMap();
    }
     */
    
    /**********************************************************************
     *  nodeNanager
     *
     ************************************************************************/
    public class CategoryTreeNodeManager implements ChildrenListManager {
        
        /** logger*/
        private Logger logger =
        LogFactory.getLogger(CategoryTreeNodeManager.class.getName());
        
        private NodeList nodeList = null;
        /** category root */
        private Category categoryRoot = null;
        
        public CategoryTreeNodeManager(Category categoryRoot) {
            this.categoryRoot = categoryRoot;
        }
        
        public void initChildrenList(NodeList nodeList) {
            logger.debug("initChildrenList");
            this.nodeList = nodeList;
            
            Collection children = null;
            try {
                if( CategoriesPreLoader.getInstance(contSvcs).isLoaded(categoryRoot)) {
                    children = CategoriesPreLoader.getInstance(contSvcs).getChildren(categoryRoot);
                }
                else {
                    children = CategoryBrowsingHandlerFactory.getHandler(contSvcs).getChildren(categoryRoot);
                }
                Iterator iter = children.iterator();
                while ( iter.hasNext() ) {
                    Category cat = (Category) iter.next();
                    try {
                        if ( !cat.isLeaf() )
                            nodeList.addNode(NodeFactory.createNode(
                            new CategoryBean(cat),
                            new CategoryTreeNodeManager(cat)
                            ));
                        else
                            nodeList.addNode(NodeFactory.createNode(
                            new CategoryBean(cat)));
                        
                    } catch (IntrospectionException e) {
                        logger.error(e, e.fillInStackTrace());
                        //} catch (LaserException e) {
                        //    e.printStackTrace();
                    }
                }
            } catch (LaserException e) {
                logger.error(e.getRootCause());
                // TODO NotifyDescriptor
            }
        }
        
        /**
         * Do not compare beans
         */
        public java.util.Comparator getComparator() {
            return new CategoryComparator();
        }
        
        public NodeList getNodeList() {
            return this.nodeList;
        }
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
        
    } // end private CategoryTreeNodeManager
    
}
