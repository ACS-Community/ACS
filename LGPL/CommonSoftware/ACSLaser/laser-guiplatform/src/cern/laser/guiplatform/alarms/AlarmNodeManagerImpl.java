/*
 * AlarmNodeManagerImpl.java
 *
 * Created on May 16, 2003, 2:21 PM
 */

package cern.laser.guiplatform.alarms;

import java.beans.IntrospectionException;
import java.util.Collection;
import java.util.Iterator;

import org.apache.log4j.Logger;

import cern.laser.console.Comment;
import cern.laser.console.CommentedAlarm;
import cern.laser.console.CommentedAlarmMap;
import cern.laser.guiplatform.explorer.ACChildrenMap;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This is manager for all alarm explorers
 * This manager is created by AlarmContainer.getAlarmManager(Constants.<which_list>);
 * and internally by AlarmNodeManagerFactory.createNodeManager(Constants.<which_list>, 
 * CommnetedAlarmMap initlist);
 * 
 * Implementation is defferent for each node manager (different initNodeManager(..)
 * method) for each list (inhibit, mask, etc);
 * 
 * @author  pawlowsk
 */
public class AlarmNodeManagerImpl extends AlarmNodeManager {
  
    /** logger */ 
    private static Logger logger = LogFactory.getLogger(AlarmNodeManagerImpl.class.getName()); 

    /** Creates a new instance of AlarmNodeManager 
     *
     * @param list this list should be used to initiazlie node manager
     *
     * @param listName  this should be one of: Constants.ACTIVE_LISTENER_KEY,
     *        Constants.INHIBIT_LISTENER_KEY, 
     *
     */
    public AlarmNodeManagerImpl(String listName, Collection list) {
        super(listName, list);
    }
 
    /**    
     */
     public void initChildrenMap( ACChildrenMap nodeMap) {        

       try {
           root = NodeFactory.createNode(new Object(), nodeMap );

           for (Iterator iter = initList.iterator(); iter.hasNext(); ){
                AlarmBean alarmBean = (AlarmBean) iter.next();
                //customizeBean(alarmBean); 
                getMap().addNode(alarmBean.getAlarmId(), 
                                NodeFactory.createNode(alarmBean));
            }
        } catch ( IntrospectionException ie) {
            logger.error(ie, ie.fillInStackTrace());
        } //catch (CloneNotSupportedException ce) {

        // indicate that this manager wants to listen for AlarmContainer
        // this is moved to programmableInitManager method
        // AlarmContainer.getDefault().attach(listName, this);    
    }
   
    /**
     * This method set suitable AlarmBean parameters, like isAlarmInhibited,
     * depending on list on which this alarm is.
     * In this case could be used Factory mehthod but this is good solution as
     * well.
     * Used for different AlarmNodeManager initialization.
     *
     * @param alarmBean bean to be customized
     */
    private void customizeBean(AlarmBean alarmBean) {
        if ( listName.equals(Constants.INHIBIT_LISTENER_KEY) ) 
                alarmBean.setIsAlarmNodeInhibited(true);
        else if ( listName.equals(Constants.MASKED_LISTENER_KEY) ) 
                alarmBean.setIsAlarmNodeMasked(true);
        else if ( listName.equals(Constants.HIGHLITED_LISTENER_KEY) ) {
                alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                alarmBean.setIsOnHighlightedList(true);
        }
  
        // TODO: other list
       
    } 

    //
    // -- implemnts AlarmListener ----------------------------------------
    //
    /** add an alarm to the children list
     * @param al The alarm bean to added
     *
     */
    public void addAlarm(AlarmBean alarm) {
    	System.out.println("### AlarmNodeManagerImpl::addAlarm");
        try {
            // the NodeFactory creates the node corresponding to the Alarm ( = bean)
            // then, it is added to the nodeMap (the Integer privateIdentifier is used
            // as a key)
            //GPManager.getStdOut().println("adding alarm : " + dsplyChngEvtAdptr.getName());
            // No manager is associated with alarm because it won't have any child                        
            
            if ( getMap() != null ) {
                getMap().addNode(NodeFactory.createNode(alarm));
                //System.out.println("XXXX getMap()="+ getMap() );
            }        
        } catch (IntrospectionException e) {
            logger.error(e, e.fillInStackTrace());
            
            // maybe Notify window

            //@PENDING
        }
    }    
    
    /** add an alarm collection to the children list
     * @param als The alarm bean collection to added
     *
     */
    public void addAlarms(Collection als) {        
        Object [] alarms = als.toArray();        
        for( int i=0; i<alarms.length; i++ ) {
            logger.debug("addAlarms() - alarms[i]=" + alarms[i] );
            AlarmBean alarmBean;
            if ( alarms[i] instanceof AlarmBean ) { 
                alarmBean = (AlarmBean) alarms[i];
            }
            else {                
                Comment comment = new Comment("","" );
                CommentedAlarm commentedAlarm = new CommentedAlarm( (cern.laser.client.data.Alarm)alarms[i], comment );
                alarmBean = new AlarmBean( commentedAlarm );
            }    
            
            addAlarm( alarmBean );
            System.out.println( "AlarmNodeManagerImpl: addAlarms() " );
            System.out.println( "NodeCount = " + getMap().getNodesCount() );
        }
    }
    
    /** remove an alarm
     * @param al The alarm bean to be removed
     *
     */
    public void removeAlarm(AlarmBean alarm) {
        removeAlarm( alarm.getAlarmId() );
    }
   
    /** remove alarm using alarmId */ 
    public void removeAlarm(String alarmId) {        
        getMap().removeNode(alarmId);        
    }
    /** remove an alarm collection
     * @param al The alarm bean collection to be removed
     *
     */
    public void removeAlarms(Collection als) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    
    /**
     * This method removes all alarms from this nodeNamager
     */
    public void removeAllAlarms() {
        //throw new UnsupportedOperationException(
        //        "method should rewritten using Node.destroy() method");
        if ( getMap() != null ) 
            getMap().clear();
    }

    /** @deprecated */
    public void initNodeManager(Collection activeAlarms) {
     
        // TODO: probably remove this method
        throw new UnsupportedOperationException(
                "method should not be used, because is not needed");

    }    
       
    /** @deprecated */
    public void initNodeManager(Collection activeAlarms, CommentedAlarmMap list) {
        
        // TODO: probably remove this method
        throw new UnsupportedOperationException(
                "method should not be used, because is not needed");

    }
}





