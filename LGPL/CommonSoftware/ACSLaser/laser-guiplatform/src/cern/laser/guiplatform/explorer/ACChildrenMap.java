/*
 * myChildrenMap.java
 *
 * Created on March 5, 2004, 3:40 PM
 */

package cern.laser.guiplatform.explorer;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.openide.nodes.Children;
import org.openide.nodes.Node;

import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.util.buffer.SynchroBuffer;
import cern.laser.util.buffer.SynchroBufferListener;


/**
 *
 * @author  woloszyn
 */
public class ACChildrenMap extends Children.SortedMap implements Comparator, SynchroBufferListener {
    private static Logger logger = LogFactory.getLogger(ACChildrenMap.class.getName());
    private SynchroBuffer buffer = null;
    
    /** Creates a new instance of ACChildrenMap */
    public ACChildrenMap() {
        super();
        setComparator(this);
        
        buffer = new SynchroBuffer(200, 2000, 100, SynchroBuffer.DUPLICATE_REPLACE);
        buffer.setSynchroBufferListener(this);
        buffer.enable();
    }
    
    public int compare(Object o1, Object o2) {
        try {
            AlarmBeanNode n1 = (AlarmBeanNode) o1;
            AlarmBeanNode n2 = (AlarmBeanNode) o2;
            
            return n1.compareTo(n2);
        }
        catch (ClassCastException cce) {
            logger.debug("ClassCastException, o1="+o1+" o2="+o2);
            return 0;
        }
    }
    
    public boolean add( Node[] arr ) {
        for (int i=0; i<arr.length; i++) {
            addNode(arr[i]);
        }
        return true;
        
    }
    /*
     * Adds the new node to the map using the name returned by the method AlarmBean.getAlarmId() as the key.
     */
    
    public boolean addNode( Node node ) {
        AlarmBeanNode abn = (AlarmBeanNode) node;
        AlarmBean abean = (AlarmBean) abn.getBean();
        addNode( abean.getAlarmId(), abn );
        return true;
    }
    /*
     * Adds the new node to the map using the given key
     */
    public void addNode(Object key, Node node) {
        NTI nti = new NTI(NTI.ADD, (String) key, node);
        buffer.push(nti);
    }
    /*
     * Adds all nodes in the given map to this node map.
     *
     */
    public synchronized void addNodes(java.util.Map nodesMap) {
        super.putAll(nodesMap);
    }
    /*
     * Returns the node associated to the given key or null
     *
     */
    public synchronized Node getNode(Object key) {
        return (Node) nodes.get( key );
    }
    /*
     * Returns a set view of the keys contained in this map.
     */
    public Set keySet() {
        synchronized ( nodes ) {
            return nodes.keySet();
        }
    }
    /*
     * Removes the node associated to the given key
     *
     */
    public Node removeNode(Object key) {
        Node node = getNode(key);
        NTI nti = new NTI(NTI.REMOVE, (String) key, key);
        buffer.push(nti);
        return node;
    }
    /*
     * Removes all nodes associated to the given keys
     *
     */
    public synchronized void removeNodes(Collection keys) {
        super.removeAll(keys);
    }
    
    /*
     * Removes all nodes from this collection
     *
     */
    public void clear() {
        nodes.clear();
        refresh();
    }
    
    public int getSize() {
        synchronized(nodes) {
            return nodes.size();
        }
    }
    public String toString() {
        synchronized(nodes) {
            return nodes.toString();
        }
    }
    /**
     * Returns map which will be used for storing objects
     */
    protected java.util.Map initMap() {
        return new HashMap(1009);
    }
    
    public void pull(cern.laser.util.buffer.PullEvent pullEvent) throws cern.laser.util.buffer.PullException {
        // we have to create 2 structures:
        // 1. Map of alarms to add (AlarmBeanNodes)
        // 2. Collection of alarm_ids to remove
        
        HashMap addMap = new HashMap(7);
        Vector keysToRemove = new Vector();
        
        AlarmBeanNode abn;
        NTI nti;
        
        Iterator iter = pullEvent.getPulled().iterator();
        while ( iter.hasNext() ) {
            nti = (NTI) iter.next();
            if ( nti.getType() == NTI.ADD ) {
                // ADD
                addMap.put(nti.getAlarmId(), nti.getObject());
            }
            else {
                // REMOVE                
                keysToRemove.add( nti.getAlarmId() );
            }
        }
        // Performing changes in structure
        //logger.debug("addMap="+addMap);
        //logger.debug("keysToRemove="+keysToRemove);
        addNodes(addMap);
        removeNodes(keysToRemove);
    }
    
    /**
     * Class NTI - Type, Node, Timestamp, Id
     * Helper class, represents change objects in synchro buffer with additional informations
     */
    protected class NTI implements Comparator {
        static public final int ADD = 1;
        static public final int REMOVE = 2;
        private int type;
        private String alarm_id;
        private Object object;
        private long timestamp; // for testing purposes
        
        public NTI() {
            
        }
        public NTI( int type, String alarm_id, Object object ) {
            this.type = type;
            this.alarm_id = alarm_id;
            this.object = object;
            this.timestamp = System.currentTimeMillis();
        }
        
        public int getType() {
            return type;
        }
        public String getAlarmId() {
            return alarm_id;
        }
        /**
         * Object can be AlarmBeanNode or alarm_id (depends of type)
         */
        public Object getObject() {
            return object;
        }
        public long getTimestamp() {
            return timestamp;
        }
        
        public int compare(Object o1, Object o2) {
            final int BEFORE = -1;
            final int EQUAL = 0;
            final int AFTER = 1;
            
            //this optimization is usually worthwhile, and can
            //always be added
            if ( o1 == o2 )
                return EQUAL;
            
            NTI nti1 = (NTI) o1;
            NTI nti2 = (NTI) o2;
            
            if ( nti1.getTimestamp() < nti2.getTimestamp() ) {
                return BEFORE;
            }
            else {
                if ( nti1.getTimestamp() > nti2.getTimestamp() ) {
                    return AFTER;
                }
                else {
                    return EQUAL;
                }
            }
        }
        
        public String toString() {
            return  "TYPE      = " + type       + "\n" +
            "ALARM_ID  = " + alarm_id   + "\n" +
            "OBJECT    = " + object     + "\n" +
            "TIMESTAMP = " + timestamp  + "\n" +
            "====================================";
        }
        public boolean equals(Object obj) {
            if ((obj == null) || (!(obj instanceof NTI))) {
                return false;
            }
            NTI nti = (NTI)obj;
            
            return getAlarmId().equals(nti.getAlarmId());
        }
        
        public int hashCode() {
            return getAlarmId().hashCode();
        }
    }
}
