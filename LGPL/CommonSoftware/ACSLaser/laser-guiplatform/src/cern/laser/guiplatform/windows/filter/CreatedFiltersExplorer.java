
/*
 * CreatedFiltersExplorer.java
 *
 * Created on March 20, 2003, 4:57 PM
 */

package cern.laser.guiplatform.windows.filter;

import java.beans.IntrospectionException;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import cern.gp.beans.BeanSupport;
import cern.gp.nodes.GPNode;
import cern.gp.nodes.NodeFactory;
import cern.gp.nodes.children.ChildrenMapManager;
import cern.gp.nodes.children.NodeMap;
import cern.laser.client.services.selection.Filter;
import cern.laser.guiplatform.filters.FilterItemBean;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This is crated filter explorer. This filters are created by user.
 *
 * @author  pawlowsk
 */
public class CreatedFiltersExplorer extends cern.gp.explorer.ListTableExplorer {
   
    protected static Logger logger = 
        LogFactory.getLogger(CreatedFiltersExplorer.class.getName());
    
    /** created filters node manager */
    private CreatedFiltersNodeManager nodeManager = null;

    /** Creates a new instance of CreatedFiltersExplorer 
     * @param createdFilters list, which contains<code>Filter</code>
     *          objects
     */
    public CreatedFiltersExplorer(List createdFilters) {
        super();
        nodeManager = new CreatedFiltersNodeManager(createdFilters);
        try {
            setRootNode(NodeFactory.createNode(new Object(), nodeManager));
            //setRootNode(NodeFactory.createNode(new FilterItemBean(), nodeManager));
            setTableColumns(new FilterItemBean(), new String [] {"operator", "value"});
            //setTableColumns(FilterItemBean.class, new String [] {"operator", "value"});
        } catch (IntrospectionException e) { e.printStackTrace(); }
        
    }

    /**
     * This method adds filter to filter list
     * (name operator value)
     *
     * @param name name
     * @param operator operator (i.e. like, contains, etc)
     * @param value value
     */
    /*
    public void addFilter(String name, String operator, String value) 
        throws IntrospectionException {
        logger.debug(getClass().getName() + 
                " addFilter(String name, String operator, String value)" +
                name + " " + operator + " " + value
        ); 
        nodeManager.addFilter(new FilterItemBean(name, operator, value));    
    }
    */
    
    /**
     * @param filter to be added
     */
    public void addFilter(Filter filter) throws IntrospectionException {
        nodeManager.addFilter(new FilterItemBean(filter));    
    }

    /**
     * This method deletes filter from list
     * @param name name
     * @param operator operator (i.e. like, contains, etc)
     * @param value value
     */
    
    public void removeFilter(String name, String operator, String value) {
        nodeManager.removeFilter(name, operator, value);
    }
    
    /**
     * @param filter filter to be removed
     */
    public void removeFilter(Filter filter) {
        nodeManager.removeFilter(filter);
    }
    /** This method removes filters from filter list
     *
     * @param 
     */
    public void removeFilter(GPNode filter) {
        nodeManager.removeFilter(filter);
    }
    
    /** This method removes all filters
     */
    public void removeAllFilters() {
        nodeManager.removeAllFilters();
    }

    /** This method returns all defined filters
     * 
     * @return Collection wiht all defined fitlers or empty list
     */
    public List getDefinedFilters() {
        return nodeManager.getDefinedFilters();
    }
       

    /**************************************************************************
     *   nodeNanager class
     *
     *************************************************************************/
    private class CreatedFiltersNodeManager implements ChildrenMapManager {
        
        final Logger logger =
            LogFactory.getLogger(CreatedFiltersNodeManager.class.getName());

        /** choosen conf */
        private NodeMap filtersNodeMap = null;
     
        /** list with defined filters, this list should contain 
         * <code>FilterItemBean</code> 
         */
        private List filtersList = null;
        
        /** empty list */
        private final List EMPTY_LIST = new java.util.ArrayList();
        
        /** Constructor 
         * @param filters list with FiltersItemBean
         */
        public CreatedFiltersNodeManager(List filters) {
            this.filtersList = filters;
        }


        public java.util.Comparator getComparator() {
            // do not compare
            return null;
        }
        
        public void initChildrenMap(cern.gp.nodes.children.NodeMap nodeMap) {
            filtersNodeMap = nodeMap;
            try {
                java.util.Iterator iter = filtersList.iterator();
                while ( iter.hasNext() ) {
                    Filter filter = (Filter) iter.next();
                    BeanSupport bean = new FilterItemBean(filter);
                    GPNode node = NodeFactory.createNode(bean);
                    FilterItemBean nodeFilter = (FilterItemBean) node.getBean();
                    filtersNodeMap.addNode(nodeFilter.getName() + 
                                            nodeFilter.getOperator() + 
                                            nodeFilter.getValue(), 
                                            node);
                }
            } catch (IntrospectionException ex) { 
                logger.error(ex, ex.fillInStackTrace()); 
            }
            
        }

        /**
         * This method adds filter to filter list
         * @param filter <code>FilterItemBean</code> object 
         */
        public void addFilter(FilterItemBean filter) 
            throws IntrospectionException {  
            filtersNodeMap.addNode(
                filter.getName() + filter.getOperator() + filter.getValue(), 
                NodeFactory.createNode(filter)
            );  
        }

        /**
         * This method removes node from list
         *
         * @param node node, which should be removed
         */
        public void removeFilter(GPNode node) {
            org.openide.nodes.Node nbNode = node.getPeerNode();
            FilterItemBean bean = (FilterItemBean) node.getBean();
            
            // test
            java.util.Set set = filtersNodeMap.keySet();
            if ( set.contains(bean.getName() + bean.getOperator() + 
                                        bean.getValue()) )
                logger.debug(bean.getName() + bean.getOperator() + 
                                        bean.getValue() + " IS IN EXPLORER !!");
            else 
                logger.debug(bean.getName() + bean.getOperator() + 
                                        bean.getValue() + " IS NOT IN EXPLORER !!");
            
            filtersNodeMap.removeNode(bean.getName() + bean.getOperator() + 
                                        bean.getValue());

            try {
                nbNode.destroy();
            } catch (java.io.IOException ie) {
                logger.error(ie, ie.fillInStackTrace());
            }
        }

        /**
         * This method deletes filter from list
         * @param name name
         * @param operator operator (i.e. like, contains, etc)
         * @param value value
         */
        public void removeFilter(String name, String operator, String value) {
            GPNode gpNode = filtersNodeMap.removeNode(name + operator + value);
            try {
                gpNode.getPeerNode().destroy();
            } catch (java.io.IOException ie) {
                logger.error(ie, ie.fillInStackTrace());
            }
         }    

        public void removeFilter(Filter filter) {
            GPNode gpNode = filtersNodeMap.removeNode(filter.getProperty() +
                    filter.getOperator() + filter.getValue());
            try {
                gpNode.getPeerNode().destroy();
            } catch (java.io.IOException ie) {
                logger.error(ie, ie.fillInStackTrace());
            }
        }
        /**
         * This method removes all filters
         */
        public void removeAllFilters() {
            // this is done because GP NodeMap does not have velues() method
            // so I can not do something like this:
            // nodeMap.values.itertor()
            // synchronized(nodeMap) {
            //  iterator.remove();
            // }
            // and I have to use nodeMap.removeNodes(Object[] keys) method 
            java.util.List nbNodes = new java.util.ArrayList();   
            for(java.util.Iterator iter = filtersNodeMap.keySet().iterator();
                iter.hasNext(); ) {
                org.openide.nodes.Node node = 
                    filtersNodeMap.getNode(iter.next()).getPeerNode();
                nbNodes.add(node);
            }
            filtersNodeMap.removeNodes(filtersNodeMap.keySet().toArray());

            for (int i = 0, j = nbNodes.size(); i < j ; i++) {
                try {
                    org.openide.nodes.Node node = (org.openide.nodes.Node) nbNodes.remove(0);
                    node.destroy();
                } catch (java.io.IOException ie) {
                    logger.error(ie, ie.fillInStackTrace());
                }
            }

        }
       
        /** This method returns all <code>List</code> with all defined filters
         * or empty list
         * @return <code>Collection</code> with <code>Filter</code> objects or empty list
         */
        public List getDefinedFilters() {
            java.util.Set set = filtersNodeMap.keySet();
            
            if ( set.size() == 0 )
                return EMPTY_LIST;
            
            //FilterItemBean [] nodes = new FilterItemBean[set.size()];
            List filters = new ArrayList();
            java.util.Iterator iter = set.iterator();
            //int counter = 0;
            while ( iter.hasNext() ) {
                //nodes[counter] = 
                filters.add(((FilterItemBean) filtersNodeMap.getNode(iter.next()).getBean()).getFilter());
                
                //counter++;
            }
            //return java.util.Arrays.asList(nodes);
            return filters;
            
        }
    } // end private CreatedFiltersNodeManager
    
}   // end CreatedFiltersExplorer

