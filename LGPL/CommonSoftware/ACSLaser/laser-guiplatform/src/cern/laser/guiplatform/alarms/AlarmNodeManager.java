/*
 * AlarmNodeManager.java
 *
 * Created on May 16, 2003, 2:21 PM
 */

package cern.laser.guiplatform.alarms;


import java.util.Collection;

import org.apache.log4j.Logger;
import org.openide.explorer.ExplorerManager;
import org.openide.nodes.Children;
import org.openide.nodes.Node;

import cern.laser.console.CommentedAlarmMap;
import cern.laser.guiplatform.explorer.ACChildrenMap;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This is manager for all alarm explorers
 * This manager is created by AlarmContainer.getAlarmManager(Constants.<which_list>);
 *
 * @author  pawlowsk
 */
public abstract class AlarmNodeManager implements AlarmListener, ExplorerManager.Provider {
    
    /** logger */
    protected static Logger logger =
    LogFactory.getLogger(AlarmNodeManager.class.getName());
    
    //protected NodeMap nodeMap;
    
    /** this indicates which list this nodeManager is interested in
     * "INHIBIT_LIST" for example
     */
    protected String listName = null;
    
    /**
     * list, which is used to initialize children manager
     */
    protected Collection initList = null;
    
    /** root node used with Explorer.setRootNode, in order to be able to
     * init children manager programmatically, without creating and displaying
     * window
     */
    protected Node root = null;
    
    protected ExplorerManager explorerManager = null;
    
    /** Creates a new instance of AlarmNodeManager
     *
     * @param list this list should be used to initiazlie node manager
     *
     * @param listName  this should be one of: Constants.ACTIVE_LISTENER_KEY,
     *        Constants.INHIBIT_LISTENER_KEY,
     */
    protected AlarmNodeManager(String listName, Collection list) {
        this.listName = listName;
        this.initList = list;
        
        try {
            root = NodeFactory.createRootNode();
            addAlarms(list);
            //^^^
            /*
            ACChildrenMap ch = new ACChildrenMap();
            Node [] nodes = new Node[5];
            for (int i=0; i<5; i++){
                nodes[i] = NodeFactory.createNode( new AlarmBean(new CommentedAlarm() ) );
            }
            //System.out.println("Nodes[] in AlarmNodeManager =" + nodes[1].toString() );
            ch.add( nodes );
            System.out.println("Map in AlarmNodeManager =" + ch.getNodesCount() );
            root =  NodeFactory.createNode( new AlarmBean(new CommentedAlarm() ), ch);
            
            System.out.println("Root in AlarmNodeManager =" + root.getChildren() );
            //vvv
            */
            getExplorerManager().setRootContext(root);
        } catch (java.beans.IntrospectionException ie) {
            logger.error(ie.getMessage(), ie);
        }
        
    }
    
    /**
     * This method removes all alarms from this nodeNamager
     */
    public abstract void removeAllAlarms();
    
    /**
     * This method initializes node manager This method can be used to
     * initialize actvie list node manager
     *
     * @param activeAlarms <code>Collection</code> with <code>Alarm</code>
     *        objects,
     */
    public abstract void initNodeManager(Collection activeAlarms);
    
    /**
     * This method initializes node manager depending on old inhibit, masked,
     * highlighted and klaxon or highlighted list. Method can be used to
     * initialize Inhibit, Mask, Highlighted and klaxon node managers
     *
     * @param activeAlarms <code>Collection</code> with active
     *        <code>Alarm</code> object (returned by
     *        AlarmSelectionHandler.select(selection, alarmSelectionListener))
     *
     * @param list one of the InhibitList, MaskedList, HighlightedList
     *        (returned by <code>Configuration.getInhibited()</code> or
     *        <code>Configuration.getMasked()</code>, etc.....
     *
     * @deprecated
     */
    public abstract void initNodeManager(Collection activeAlarms,
    CommentedAlarmMap list);
    
    
    public AlarmBean getAlarm(String alarmId) {
        AlarmBeanNode node = null;
        ACChildrenMap nodeMap = getMap();
        if ( nodeMap != null )
            if ( (node = (AlarmBeanNode) nodeMap.getNode(alarmId)) != null )
                return (AlarmBean) node.getBean();
        
        return null;
    }
    
    public Node getRootNode() {
        return root;
    }
    
    public void makeAlarmsNotNew() {
        ACChildrenMap nodeMap = (ACChildrenMap) getMap();
        java.util.Set set = nodeMap.keySet();
        for (java.util.Iterator iter = set.iterator(); iter.hasNext();) {
            AlarmBean ab = (AlarmBean) ( (AlarmBeanNode) nodeMap.getNode(iter.next()) ).getBean();
            if ( ab.isNew() )
                ab.setIsNew(false);
        }
        
    }

    public void fireFontChanged() {
        ACChildrenMap nodeMap = (ACChildrenMap) getMap();
        java.util.Set set = nodeMap.keySet();
        for (java.util.Iterator iter = set.iterator(); iter.hasNext();) {
            AlarmBean ab = (AlarmBean) ( (AlarmBeanNode) nodeMap.getNode(iter.next()) ).getBean();
            ab.fireFontChanged();
        }
        
    }
    
    public void programmagbleInitManager() {
        Children children = root.getChildren();
        children.getNodes();
        AlarmContainer.getDefault().attach(listName, this);
    }
    
    public ExplorerManager getExplorerManager() {
        if ( explorerManager == null ) {
            explorerManager = new ExplorerManager();
        }
        return explorerManager;
    }
    
    public ACChildrenMap getMap() {
        return (ACChildrenMap) root.getChildren();
    }       
}
