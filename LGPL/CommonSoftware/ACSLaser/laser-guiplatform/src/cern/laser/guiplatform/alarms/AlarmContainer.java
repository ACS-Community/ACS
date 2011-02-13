/*
 * AlarmContainer.java
 *
 * Created on May 21, 2003, 3:59 PM
 */

package cern.laser.guiplatform.alarms;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import alma.acs.container.ContainerServicesBase;

import EDU.oswego.cs.dl.util.concurrent.Mutex;
import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.data.Category;
import cern.laser.client.services.browsing.AlarmBrowsingHandler;
import cern.laser.client.services.selection.AlarmSearchListener;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.LaserSearchException;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.console.Behaviour;
import cern.laser.console.Comment;
import cern.laser.console.CommentedAlarm;
import cern.laser.console.CommentedAlarmMap;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.guiplatform.logging.TraceLogger;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;

/**
 * This class store all alarm lists (active, inhibited, masked, etc
 * This is in fact alarm manager. This class is used to creating AlarmNodeManager
 * for each list
 *
 * @author  pawlowsk
 */ 
//public class AlarmContainer implements JMSAlarmListener {
public class AlarmContainer implements AlarmSelectionListener, AlarmSearchListener {
    
    /** logger */
    private static Logger logger =
    LogFactory.getLogger(AlarmContainer.class.getName());
    
    /** instance */
    private static AlarmContainer INSTANCE = null;
    
    /** observers (AlarmListener)*/
    private Map observers = null;
    
    /** AlarmNumberChangeListener observers */
    private Map alarmNumberChangeObservers = null;
    
    /** onExcetpion listeners (i.e. AlarmStatisticInfoPanel )
     * this map keps AlarmSelectionOnExceptionListener objects
     */
    private List onExceptionListeners = null;
    
    /** alarmNanagers AlarmNodeManagersImpl objects */
    private Map nodeManagers = null;
    
    
    /** mutex */
    private final Mutex lock = new Mutex();
    
    /** lists */
    private CommentedAlarmMap activeList                = null;
    private CommentedAlarmMap inhibitList               = null;
    private CommentedAlarmMap maskedList                = null;
    private CommentedAlarmMap highlightedAndKlaxonList  = null;
    private CommentedAlarmMap instantList               = null;
    private CommentedAlarmMap highlightedList           = null;
    private CommentedAlarmMap acknowledgedList          = null;
    private CommentedAlarmMap notNewAlarmsList          = null;
    
    /** Creates a new instance of AlarmContainer */
    private AlarmContainer() {
        
        observers = new HashMap();
        alarmNumberChangeObservers = new HashMap();
        onExceptionListeners = new ArrayList();
        /*
        activeList = Collections.synchronizedMap(new HashMap());
        inhibitList = Collections.synchronizedMap(new HashMap());
        maskedList = Collections.synchronizedMap(new HashMap());
        highlightedAndKlaxonList = Collections.synchronizedMap(new HashMap());
        instantList = Collections.synchronizedMap(new HashMap());
         */
        
        activeList = new CommentedAlarmMap();
        inhibitList = new CommentedAlarmMap();
        maskedList = new CommentedAlarmMap();
        highlightedAndKlaxonList = new CommentedAlarmMap();
        instantList = new CommentedAlarmMap();
        highlightedList = new CommentedAlarmMap();
        acknowledgedList = new CommentedAlarmMap();
        notNewAlarmsList = new CommentedAlarmMap();
        nodeManagers = new HashMap();
        
        /* this is done by initContainer method
        nodeManagers.put(Constants.ACTIVE_LISTENER_KEY,
                         new AlarmNodeManagerImpl(Constants.ACTIVE_LISTENER_KEY));
        nodeManagers.put(Constants.INHIBIT_LISTENER_KEY,
                        new AlarmNodeManagerImpl(Constants.INHIBIT_LISTENER_KEY));
        nodeManagers.put(Constants.MASKED_LISTENER_KEY,
                        new AlarmNodeManagerImpl(Constants.MASKED_LISTENER_KEY));
        nodeManagers.put(Constants.HIGHLITED_LISTENER_KEY,
                        new AlarmNodeManagerImpl(Constants.HIGHLITED_LISTENER_KEY));
        nodeManagers.put(Constants.INSTANT_LISTENER_KEY,
                        new AlarmNodeManagerImpl(Constants.INSTANT_LISTENER_KEY));
         */
        
        
    }
    
    /** This method gives default (and only one) AlarmContainer
     * @return alarmConstainer
     */
    public /*synchronized*/ static AlarmContainer getDefault() {
        if ( INSTANCE == null )
            INSTANCE = new AlarmContainer();
        return INSTANCE;
    }
    
    /* This method clears alarm container, should be used after user logout
     */
    public void clearContainer() {
        observers.clear();
        observers = null;
        alarmNumberChangeObservers.clear();
        alarmNumberChangeObservers = null;
        onExceptionListeners.clear();
        onExceptionListeners = null;
        
        // clean each node namagers
        for (Iterator managerIter = nodeManagers.values().iterator();
        managerIter.hasNext();) {
            AlarmNodeManager manager = (AlarmNodeManager) managerIter.next();
            manager.removeAllAlarms();
        }
        
        activeList.clear();
        activeList = null;
        inhibitList.clear();
        inhibitList = null;
        maskedList.clear();
        maskedList = null;
        highlightedAndKlaxonList.clear();
        highlightedAndKlaxonList = null;
        instantList.clear();
        instantList = null;
        highlightedList.clear();
        highlightedList = null;
        acknowledgedList.clear();
        acknowledgedList = null;
        notNewAlarmsList.clear();
        notNewAlarmsList = null;
        nodeManagers.clear();
        nodeManagers = null;
        
        // mayby lock.release()
        //lock.release();
        
        INSTANCE = null;
    }
    
    //
    // -- methods for adding end deleting listeners --------------------
    //
    /** This method is used for adding listeners (Alarm Listeners)
     * @param key list name "ACTIVE_LISTENER_KEY"
     * @param listener listener
     */
    public void attach(String key, AlarmListener listener) {
        
        // check if there is appropriate key
        
        observers.put(key, listener);
        
        // fire existing alarms
        // since all node managers are expanded programmatically
        // this is not needed
        //Iterator iter = getList(key).values().iterator();
        //while (iter.hasNext()) {
        //    CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
        //    listener.addAlarm(new AlarmBean(commentedAlarm));
        //}
        
        
    }
    
    /**
     * This method is used for removing listener
     * @param key list name "ACTIVE_LISTENER_KEY"
     */
    public void detach(String key) {
        observers.remove(key);
    }
    
    /**
     * This method is used to add AlarmNubmerChangeListener
     * (i. e. for adding AlarmStaticInfoPanel
     *
     * @param key temporarily not used
     */
    public void attach(String key, AlarmsNumberChangeListener listener) {
        alarmNumberChangeObservers.put(key, listener);
        
        // update alarmNumberChangeListeners
        listener.update(AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER,
        activeList.values().size());
        listener.update(AlarmsNumberChangeListener.INHIBITED_ALARM_COUNTER,
        inhibitList.values().size());
        listener.update(AlarmsNumberChangeListener.MASKED_ALARM_COUNTER,
        maskedList.values().size());
        
        // other lists
        
    }
    /**
     * Method used for removing AlarmNumberChangeListener
     * @param key
     */
    public void detachAlarmNumberChangeListener(String key) {
        alarmNumberChangeObservers.remove(key);
    }
    /** This method returns apropriate list
     * @param key this should be one of: Constants.ACTIVE_LISTENER_KEY,
     *        Constants.INHIBIT_LISTENER_KEY,
     *
     * @return list
     */
    //private Map getList(String key) {
    private CommentedAlarmMap getList(String key) {
        
        if ( key.equals(Constants.INHIBIT_LISTENER_KEY) )
            return inhibitList;
        else if ( key.equals(Constants.MASKED_LISTENER_KEY) )
            return maskedList;
        else if ( key.equals(Constants.HIGHLITED_LISTENER_KEY) )
            return highlightedAndKlaxonList;
        else if ( key.equals(Constants.INSTANT_LISTENER_KEY) )
            return instantList;
        
        return activeList;
    }
    
    /**
     * @param (i. e. AlarmStaticInfoPanel)
     */
    public void addAlarmSelectionOnExceptionListener(AlarmSelectionOnExceptionListener listener) {
        onExceptionListeners.add(listener);
    }
    
    public void removeAlarmSelectionOnExceptionListener(AlarmSelectionOnExceptionListener listener) {
        onExceptionListeners.remove(listener);
    }
    /** AlarmNodeManager factory
     *
     * @param listName this should be one of: Constants.ACTIVE_LISTENER_KEY,
     *        Constants.INHIBIT_LISTENER_KEY,
     *
     * @throws IllegalArgumentException if list name is not one of
     *          Constants.ACTIVE_LISTENER_KEY, Constants.INHIBIT_LISTENER_KEY, etc
     *
     */
    public AlarmNodeManager getAlarmNodeManager(String listName) {
        if ( !(listName.equals(Constants.ACTIVE_LISTENER_KEY) ||
        listName.equals(Constants.INHIBIT_LISTENER_KEY) ||
        listName.equals(Constants.HIGHLITED_LISTENER_KEY) ||
        listName.equals(Constants.MASKED_LISTENER_KEY) ||
        listName.equals(Constants.INSTANT_LISTENER_KEY) ||
        listName.equals(Constants.SEARCH_LISTENER_KEY)) )   // and other lists
            throw new IllegalArgumentException("Invalid alarm list name");
        
        return (AlarmNodeManager) nodeManagers.get(listName);
    }
    /**
     * @param key
     * @param manager
     */
    /*
    public void setAlarmNodeManager(String key, AlarmNodeManager manager) {
        // TODO: check if key is OK (Constants.AlarmNodeManager, etc);
        nodeManagers.put(key, manager);
    }
     */
    
    //
    // -- init lists mehtods -----------------------------
    //
    /**
     * This method initializes alarm container, what means: initialize active
     * list, (all list in general) as well as change configuration according
     * to active list
     *
     * @param activeList <code>Map</code> with Alarm object
     * @param configuration <code>Configuration</code>, which should be used
     *          to initialize alarm container
     */
    public void initContainer(Map _activeList, Configuration configuration)
    throws LaserConsoleException {
        
        logger.debug("initializing alarm container !!! with initList=" + _activeList);
        
        inhibitList = configuration.getInhibited();
        maskedList = configuration.getMasked();
        highlightedAndKlaxonList = configuration.getAutoHighlighted();
        highlightedList = configuration.getHighlighted();
        acknowledgedList = configuration.getAcknowledged();
        notNewAlarmsList = configuration.getNewIndicator();
        
        logger.debug("inhibitList="+inhibitList);
        logger.debug("maskedList="+maskedList);
        logger.debug("highlightedAndKlaxon="+highlightedAndKlaxonList);
        logger.debug("highlightedList="+highlightedList);
        logger.debug("acknowledgedList="+acknowledgedList);
        logger.debug("not new list="+notNewAlarmsList);
        
        logger.debug("highlightedList size: " + highlightedList.size());
        
        // update active list font size from config
        
        if( configuration.getActiveListFont()!=null && configuration.getActiveListFont().equals(Boolean.TRUE) ) { // true = BIG
        	AppRegister.getInstance().setFontBig(true);
        }
        else {
        	AppRegister.getInstance().setFontBig(false);
        }

        //process lists
        
        processList(_activeList, inhibitList);
        
        processList(_activeList, maskedList);
        
        // rest alarms to active list
        for (Iterator iter = _activeList.values().iterator(); iter.hasNext(); ) {
            Alarm alarm = (Alarm) iter.next();
            CommentedAlarm commentedAlarm = new CommentedAlarm(alarm, null); // without comment
            this.activeList.put(commentedAlarm);
        }
        
        // node managers initializtions
        Collection higlightedKlaxonTempList = new ArrayList();
        
        Collection activeTempList = new ArrayList();
        for (Iterator iter = activeList.values().iterator(); iter.hasNext(); ){
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            AlarmBean alarmBean = new AlarmBean(commentedAlarm);
            if ( highlightedAndKlaxonList.containsKey(commentedAlarm.getAlarm().getAlarmId()) ) {
                alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                // WARNING I and not able to do this
                //alarmBean.setHighlightedOrKlaxon(???????????????);
                CommentedAlarm _commAlarm = highlightedAndKlaxonList.get(commentedAlarm.getAlarm().getAlarmId());
                AlarmBean _alarmBean = new AlarmBean(_commAlarm);
                _alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                _alarmBean.setIsOnHighlightedList(true);
                _alarmBean.registerPropertyChangeListener(alarmBean);
                alarmBean.registerPropertyChangeListener(_alarmBean);
                higlightedKlaxonTempList.add(_alarmBean);
            }
            //customizeBean(alarmBean);
            
            if ( highlightedList.containsKey(commentedAlarm.getAlarm().getAlarmId()) )
                alarmBean.setIsAlarmNodeHighlighted(true);
            
            
            activeTempList.add(alarmBean);
        }
        
        
        // set acknowledged
        logger.debug("acknowledgedList=" + acknowledgedList);
        for (Iterator iter = activeTempList.iterator(); iter.hasNext(); ) {
            AlarmBean ab = (AlarmBean) iter.next();
            logger.debug("set acknowledge, ab="+ab);
            if ( acknowledgedList.containsKey(ab.getAlarmId()) ){
                ab.setIsAlarmNodeAcknowledged(true);
                logger.debug("--> Alarm acknowledged " +ab.getAlarmId() + "state="+ab.isAlarmNodeAcknowledged() );
            }
        }
        
        // set not new
        for (Iterator iter = activeTempList.iterator(); iter.hasNext(); ) {
            AlarmBean ab = (AlarmBean) iter.next();
            logger.debug("set not new, ab="+ab);
            if ( notNewAlarmsList.containsKey(ab.getAlarmId()) ){
                ab.setIsNew(false);
                logger.debug("--> Alarm set as not new " +ab.getAlarmId() + "state="+ab.isNew() );
            }
        }
        
        AlarmNodeManager activeListManager = AlarmNodeManagerFactory.createNodeManager(
        Constants.ACTIVE_LISTENER_KEY,
        activeTempList);
        activeListManager.programmagbleInitManager();
        
        Collection inhibitTempList = new ArrayList();
        for (Iterator iter = inhibitList.values().iterator(); iter.hasNext(); ){
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            AlarmBean alarmBean = new AlarmBean(commentedAlarm);
            if ( highlightedAndKlaxonList.containsKey(commentedAlarm.getAlarm().getAlarmId()) ) {
                alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                // WARNING I and not able to do this
                //alarmBean.setHighlightedOrKlaxon(???????????????);
                CommentedAlarm _commAlarm = highlightedAndKlaxonList.get(commentedAlarm.getAlarm().getAlarmId());
                AlarmBean _alarmBean = new AlarmBean(_commAlarm);
                _alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                _alarmBean.setIsOnHighlightedList(true);
                _alarmBean.registerPropertyChangeListener(alarmBean);
                alarmBean.registerPropertyChangeListener(_alarmBean);
                higlightedKlaxonTempList.add(_alarmBean);
            }
            alarmBean.setIsAlarmNodeInhibited(true);
            logger.debug("inhibit, alarmBean is " + alarmBean);
            logger.debug("inhibit, active="+alarmBean.isAlarmNodeActive() );
            logger.debug("inhibit, on highlighted list="+alarmBean.isOnHighlightedList() );
            //customizeBean(alarmBean);
            inhibitTempList.add(alarmBean);
        }
        
        AlarmNodeManager inhibitListManager = AlarmNodeManagerFactory.createNodeManager(
        Constants.INHIBIT_LISTENER_KEY,
        inhibitTempList);
        inhibitListManager.programmagbleInitManager();
        
        // add inhibited alarms to Inhibit Alarm Node Manager
        /*
        for (Iterator iter = inhibitList.values().iterator(); iter.hasNext(); ) {
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            AlarmBean alarmBean = new AlarmBean(commentedAlarm);
            inhibitListManager.addAlarm(alarmBean);
        }
         */
        Collection maskedTempList = new ArrayList();
        for (Iterator iter = maskedList.values().iterator(); iter.hasNext(); ){
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            AlarmBean alarmBean = new AlarmBean(commentedAlarm);
            if ( highlightedAndKlaxonList.containsKey(commentedAlarm.getAlarm().getAlarmId()) ) {
                alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                // WARNING I and not able to do this
                //alarmBean.setHighlightedOrKlaxon(???????????????);
                CommentedAlarm _commAlarm = highlightedAndKlaxonList.get(commentedAlarm.getAlarm().getAlarmId());
                AlarmBean _alarmBean = new AlarmBean(_commAlarm);
                _alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                _alarmBean.setIsOnHighlightedList(true);
                _alarmBean.registerPropertyChangeListener(alarmBean);
                alarmBean.registerPropertyChangeListener(_alarmBean);
                higlightedKlaxonTempList.add(_alarmBean);
            }
            
            alarmBean.setIsAlarmNodeMasked(true);
            // if alarm belongs to selected categories add it to mask list
            CategorySelection selectedCategories = configuration.getSelection().getCategorySelection();
            if ( belongsToSelectedCategories( alarmBean, selectedCategories) ) {
                maskedTempList.add(alarmBean);
                logger.debug("reload container: add alarm to mask list because it is in selection");
            }
            else {
                iter.remove();
            }
        }
        
        AlarmNodeManager maskedListManager = AlarmNodeManagerFactory.createNodeManager(
        Constants.MASKED_LISTENER_KEY,
        maskedTempList);
        maskedListManager.programmagbleInitManager();
        
        for (Iterator iter = highlightedAndKlaxonList.values().iterator(); iter.hasNext(); ) {
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            AlarmBean __alarmBean = new AlarmBean(commentedAlarm);
            if ( !higlightedKlaxonTempList.contains(__alarmBean) ) {
                __alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                __alarmBean.setIsOnHighlightedList(true);
                logger.debug(__alarmBean.getName() + " must be added to higlightedKlaxonTempList");
                logger.debug("IsAlarmNodeHighlightedAndKlaxon: " + __alarmBean.isAlarmNodeHighlightedAndKlaxon());
                logger.debug("IsOnHighlightedList: " + __alarmBean.isOnHighlightedList());
                higlightedKlaxonTempList.add(__alarmBean);
            }
        }
        
        AlarmNodeManager highlightedAndKlaxonedListManager = AlarmNodeManagerFactory.createNodeManager(
        Constants.HIGHLITED_LISTENER_KEY,
        higlightedKlaxonTempList);
        highlightedAndKlaxonedListManager.programmagbleInitManager();
        
        
        AlarmNodeManager instantListManager = AlarmNodeManagerFactory.createNodeManager(
        Constants.INSTANT_LISTENER_KEY,
        new ArrayList());
        instantListManager.programmagbleInitManager();
        
        // Search Node Manager
        
        AlarmNodeManager searchListManager = AlarmNodeManagerFactory.createNodeManager(
        Constants.SEARCH_LISTENER_KEY,  new ArrayList());
        
        // other managers
        
        
        nodeManagers.put(Constants.ACTIVE_LISTENER_KEY, activeListManager);
        nodeManagers.put(Constants.INHIBIT_LISTENER_KEY, inhibitListManager);
        nodeManagers.put(Constants.MASKED_LISTENER_KEY, maskedListManager);
        nodeManagers.put(Constants.HIGHLITED_LISTENER_KEY, highlightedAndKlaxonedListManager);
        nodeManagers.put(Constants.INSTANT_LISTENER_KEY, instantListManager);
        nodeManagers.put(Constants.SEARCH_LISTENER_KEY, searchListManager);
        
        // update alarmNumberChangeListeners
        AlarmsNumberChangeListener numChngListener = null;
        for (Iterator iter = alarmNumberChangeObservers.keySet().iterator(); iter.hasNext(); ) {
            numChngListener = (AlarmsNumberChangeListener) alarmNumberChangeObservers.get(iter.next());
            numChngListener.update(
            AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER,
            activeList.values().size());
            numChngListener.update(
            AlarmsNumberChangeListener.INHIBITED_ALARM_COUNTER,
            inhibitList.values().size());
            numChngListener.update(
            AlarmsNumberChangeListener.MASKED_ALARM_COUNTER,
            maskedTempList.size() );
            
        }
        
    }
    
    /** this method clears all node mangers and all lists  */
    public void clearNodeManagers() {
        
        // clean each node namagers
        for (Iterator managerIter = nodeManagers.values().iterator(); managerIter.hasNext();) {
            AlarmNodeManager manager = (AlarmNodeManager) managerIter.next();
            manager.removeAllAlarms();
        }
        
        // reset alarm counters
        for (Iterator iter = alarmNumberChangeObservers.values().iterator();
        iter.hasNext();) {
            AlarmsNumberChangeListener  listener =
            (AlarmsNumberChangeListener) iter.next();
            listener.update(AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER, 0);
            listener.update(AlarmsNumberChangeListener.INHIBITED_ALARM_COUNTER, 0);
            listener.update(AlarmsNumberChangeListener.MASKED_ALARM_COUNTER, 0);
        }
        activeList.clear();
        inhibitList.clear();
        maskedList.clear();
        highlightedAndKlaxonList.clear();
        instantList.clear();
        highlightedList.clear();
        acknowledgedList.clear();
        notNewAlarmsList.clear();
        
    }
    
    public void reloadContainer(Map _activeList, Configuration configuration)
    throws LaserConsoleException {
        
        logger.debug("reloading  alarm container, new active list size: " + _activeList.size());
        
        inhibitList = configuration.getInhibited();
        maskedList = configuration.getMasked();
        highlightedAndKlaxonList = configuration.getAutoHighlighted();
        highlightedList = configuration.getHighlighted();
        acknowledgedList = configuration.getAcknowledged();
        notNewAlarmsList = configuration.getNewIndicator();
        
        // update active list font size from config
        
        if( configuration.getActiveListFont()!=null && configuration.getActiveListFont().equals(Boolean.TRUE) ) { // true = BIG
        	AppRegister.getInstance().setFontBig(true);
        }
        else {
        	AppRegister.getInstance().setFontBig(false);
        }
        
        // process inhibit list
        processList(_activeList, inhibitList);
        
        // process masked list
        processList(_activeList, maskedList);
        
        // rest alarms to active list
        for (Iterator iter = _activeList.values().iterator(); iter.hasNext(); ) {
            Alarm alarm = (Alarm) iter.next();
            CommentedAlarm commentedAlarm = new CommentedAlarm(alarm, null); // without comment
            this.activeList.put(commentedAlarm);
        }
        
        // process highlighted and klaxon list
        //for (Iterator iter = highlightedAndKlaxonList.values.iterator();
        //     iter.hasNext(); ) {
        //    ;
        //}
        
        // node managers initializtions
        Collection higlightedKlaxonTempList = new ArrayList();
        
        Collection activeTempList = new ArrayList();
        for (Iterator iter = activeList.values().iterator(); iter.hasNext(); ){
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            AlarmBean alarmBean = new AlarmBean(commentedAlarm);
            if ( highlightedAndKlaxonList.containsKey(commentedAlarm.getAlarm().getAlarmId()) ) {
                alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                // WARNING I and not able to do this
                //alarmBean.setHighlightedOrKlaxon(???????????????);
                CommentedAlarm _commAlarm = highlightedAndKlaxonList.get(commentedAlarm.getAlarm().getAlarmId());
                AlarmBean _alarmBean = new AlarmBean(_commAlarm);
                _alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                _alarmBean.setIsOnHighlightedList(true);
                _alarmBean.registerPropertyChangeListener(alarmBean);
                alarmBean.registerPropertyChangeListener(_alarmBean);
                higlightedKlaxonTempList.add(_alarmBean);
            }
            //customizeBean(alarmBean);
            if ( highlightedList.containsKey(commentedAlarm.getAlarm().getAlarmId()) )
                alarmBean.setIsAlarmNodeHighlighted(true);
            
            activeTempList.add(alarmBean);
        }
        
        // set acknowledged
        logger.debug("acknowledgedList=" + acknowledgedList);
        for (Iterator iter = activeTempList.iterator(); iter.hasNext(); ) {
            AlarmBean ab = (AlarmBean) iter.next();
            logger.debug("set acknowledge, ab="+ab);
            if ( acknowledgedList.containsKey(ab.getAlarmId()) ){
                ab.setIsAlarmNodeAcknowledged(true);
                logger.debug("--> Alarm acknowledged " +ab.getAlarmId() + "state="+ab.isAlarmNodeAcknowledged() );
            }
        }

        // set not new
        for (Iterator iter = activeTempList.iterator(); iter.hasNext(); ) {
            AlarmBean ab = (AlarmBean) iter.next();
            logger.debug("set not new, ab="+ab);
            if ( notNewAlarmsList.containsKey(ab.getAlarmId()) ){
                ab.setIsNew(false);
                logger.debug("--> Alarm set as not new " +ab.getAlarmId() + "state="+ab.isNew() );
            }
        }
        
        AlarmNodeManager activeListManager =  getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
        for (Iterator iter = activeTempList.iterator(); iter.hasNext(); )
            activeListManager.addAlarm((AlarmBean) iter.next());
        
        Collection inhibitTempList = new ArrayList();
        for (Iterator iter = inhibitList.values().iterator(); iter.hasNext(); ){
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            AlarmBean alarmBean = new AlarmBean(commentedAlarm);
            if ( highlightedAndKlaxonList.containsKey(commentedAlarm.getAlarm().getAlarmId()) ) {
                alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                // WARNING I and not able to do this
                //alarmBean.setHighlightedOrKlaxon(???????????????);
                CommentedAlarm _commAlarm = highlightedAndKlaxonList.get(commentedAlarm.getAlarm().getAlarmId());
                AlarmBean _alarmBean = new AlarmBean(_commAlarm);
                _alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                _alarmBean.setIsOnHighlightedList(true);
                //_alarmBean.addPropertyChangeListener(alarmBean);
                _alarmBean.registerPropertyChangeListener(alarmBean);
                alarmBean.registerPropertyChangeListener(_alarmBean);
                higlightedKlaxonTempList.add(_alarmBean);
            }
            alarmBean.setIsAlarmNodeInhibited(true);
            //customizeBean(alarmBean);
            inhibitTempList.add(alarmBean);
        }
        
        AlarmNodeManager inhibitListManager =  getAlarmNodeManager(Constants.INHIBIT_LISTENER_KEY);
        for (Iterator iter = inhibitTempList.iterator(); iter.hasNext(); )
            inhibitListManager.addAlarm((AlarmBean) iter.next());
        
        Collection maskedTempList = new ArrayList();
        for (Iterator iter = maskedList.values().iterator(); iter.hasNext(); ){
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            AlarmBean alarmBean = new AlarmBean(commentedAlarm);
            if ( highlightedAndKlaxonList.containsKey(commentedAlarm.getAlarm().getAlarmId()) ) {
                alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                // WARNING I and not able to do this
                //alarmBean.setHighlightedOrKlaxon(???????????????);
                CommentedAlarm _commAlarm = highlightedAndKlaxonList.get(commentedAlarm.getAlarm().getAlarmId());
                AlarmBean _alarmBean = new AlarmBean(_commAlarm);
                _alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                _alarmBean.setIsOnHighlightedList(true);
                //_alarmBean.addPropertyChangeListener(alarmBean);
                _alarmBean.registerPropertyChangeListener(alarmBean);
                alarmBean.registerPropertyChangeListener(_alarmBean);
                higlightedKlaxonTempList.add(_alarmBean);
            }
            
            alarmBean.setIsAlarmNodeMasked(true);
            // if alarm belongs to selected categories add it to mask list
            CategorySelection selectedCategories = configuration.getSelection().getCategorySelection();
            if ( belongsToSelectedCategories( alarmBean, selectedCategories) ) {
                maskedTempList.add(alarmBean);
                logger.debug("reload container: add alarm to mask list because it is in selection");
            }
        }
        
        AlarmNodeManager maskedListManager = getAlarmNodeManager(Constants.MASKED_LISTENER_KEY);
        for (Iterator iter = maskedTempList.iterator(); iter.hasNext(); )
            maskedListManager.addAlarm((AlarmBean) iter.next());
        
        for (Iterator iter = highlightedAndKlaxonList.values().iterator(); iter.hasNext(); ) {
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            AlarmBean __alarmBean = new AlarmBean(commentedAlarm);
            if ( !higlightedKlaxonTempList.contains(__alarmBean) ) {
                __alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                __alarmBean.setIsOnHighlightedList(true);
                //logger.debug(__alarmBean.getName() + " must be added to higlightedKlaxonTempList");
                //logger.debug("IsAlarmNodeHighlightedAndKlaxon: " + __alarmBean.isAlarmNodeHighlightedAndKlaxon());
                //logger.debug("IsOnHighlightedList: " + __alarmBean.isOnHighlightedList());
                higlightedKlaxonTempList.add(__alarmBean);
            }
        }
        
        AlarmNodeManager highlightedAndKlaxonedListManager = getAlarmNodeManager(Constants.HIGHLITED_LISTENER_KEY);
        for (Iterator iter = higlightedKlaxonTempList.iterator(); iter.hasNext(); )
            highlightedAndKlaxonedListManager.addAlarm((AlarmBean) iter.next());
        
        // do nothing with instant manager
        AlarmNodeManager instantListManager = null;
        
        // update alarmNumberChangeListeners
        AlarmsNumberChangeListener numChngListener = null;
        for (Iterator iter = alarmNumberChangeObservers.keySet().iterator(); iter.hasNext(); ) {
            numChngListener = (AlarmsNumberChangeListener) alarmNumberChangeObservers.get(iter.next());
            numChngListener.update(
            AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER,
            activeList.values().size());
            numChngListener.update(
            AlarmsNumberChangeListener.INHIBITED_ALARM_COUNTER,
            inhibitList.values().size());
            numChngListener.update(
            AlarmsNumberChangeListener.MASKED_ALARM_COUNTER,
            maskedTempList.size() );
            
        }
        
        // end: node mangers updateing
    }
    
    /**
     * this method romoves alarms from activeList depending on list (inhibit, mask)
     *
     * @param activeList - <code>Map</code> which contains <code>Alarm</code> object, create as a result of
     *                      AlarmSelectionHandler.select(Selection, AlarmSelectionListener)
     *
     * @param list - one of the inhibit, or mask list
     */
    private void processList(Map activeList, CommentedAlarmMap list) {
        // process list
        for (Iterator iter = list.values().iterator(); iter.hasNext(); ) {
            // remove these alarms from active list
            Alarm alarm = ((CommentedAlarm) iter.next()).getAlarm();
            logger.debug(alarm.getAlarmId());
            if ( activeList.containsKey(alarm.getAlarmId()) ) {
                // update alarm on inhibit list
                Alarm _alarm = (Alarm) activeList.remove(alarm.getAlarmId());
            }
        }
    }
    
    
    //
    // -- ihibit, uninhibit, mask, unmask, higlight and klaxon, uhhigliht and klaxon methods -------- //
    //
    
    /** this method inhibits set of alarms, when user selects set of alarm and
     * then chooses InhibitAction
     *
     * Remove that alarms from active list and node managers and add that alarms
     * to inhibit list and node managers (nothing else)
     *
     * @param inhibitedAlarm alarms to be inhibited
     * @param comment commnet which will be applied to all alarms from
     *              inhibitedAlarm
     */
    public void inhibit(AlarmBean [] inhibitedAlarm, Comment comment) {
        boolean isLockReleased = false;
        try {
            lock.acquire();
            try {
                //action
                
                // this is done because when exception ocurrs all this alarms should
                // be deleted from inhibit list
                // this should be done other way, because in case of exception
                // that comments are set.
                // alarm.setComment() should be done after conf.setInhibited
                // same situation with other lists
                CommentedAlarm [] commAlarms = new CommentedAlarm[inhibitedAlarm.length];
                for (int i = 0; i < inhibitedAlarm.length; i++) {
                    commAlarms[i] = inhibitedAlarm[i].getCommentedAlarm();
                    commAlarms[i].setComment(comment);
                }
                
                CommentedAlarmMap tempInhibited = new CommentedAlarmMap();
                tempInhibited.putAll(inhibitList);
                
                // add all alarm to inhibit list and do configuration.setInhibited
                for (int i = 0; i < commAlarms.length; i++)
                    tempInhibited.put(commAlarms[i]);
                
                try {
                    
                    Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
                    if ( AppRegister.getInstance().isGuestMode() == false ) {
                        conf.setInhibited(tempInhibited);   // here Exception can be thrown
                    }
                    for (int i = 0; i < commAlarms.length; i++)
                        inhibitList.put(commAlarms[i]);
                    
                    // remove from active list
                    for (int i = 0; i < commAlarms.length; i++)
                        activeList.remove(commAlarms[i].getAlarm().getAlarmId());
                    
                    // remove from activeListNodeManager
                    AlarmNodeManager actvienManager = getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
                    AlarmNodeManager inhibitnManager = getAlarmNodeManager(Constants.INHIBIT_LISTENER_KEY);
                    
                    for (int i = 0; i < inhibitedAlarm.length; i++) {
                        actvienManager.removeAlarm(inhibitedAlarm[i]);
                        
                        // add to inhibit list
                        //AlarmBean newAlarm = new AlarmBean(commAlarms[i]);
                        AlarmBean newAlarm = inhibitedAlarm[i];
                        newAlarm.setIsAlarmNodeInhibited(true);
                        
                        // add to ihnibit list node manager
                        inhibitnManager.addAlarm(newAlarm);
                        
                        updateAlarmNumberChangeListeners(
                        AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER,
                        AlarmsNumberChangeListener.INHIBITED_ALARM_COUNTER);
                        
                    }
                } catch (LaserConsoleException lce) {
                    
                    
                    // here comments shoule be reseted
                    
                    
                    lock.release();
                    isLockReleased = true;
                    logger.error(lce, lce.fillInStackTrace());
                    logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                    
                    TraceLogger.log(lce.fillInStackTrace());
                    AcWindowManager.notifyError("Alarm can't be inhibited.\n" +
                    "Internal error.\n" +
                    "See log files or contact console developers.");
                }
                
            } finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
        
    }
    
    /** this method uninhibits set of alarms, when user selects set of alarm and
     * then chooses InhibitAction
     *
     * @param inhibitedAlarm alarms to be inhibited
     */
    public void uninhibit(AlarmBean [] uninhibitedAlarms, ContainerServicesBase contSvcs) {
        boolean isLockReleased = false;
        
        Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
        CategorySelection catSel =
        AppRegister.getInstance().getSelection().getCategorySelection();
        boolean autoterminated = AppRegister.getInstance().getBehaviour().isAlarmAutoTerminated();
        
        List toActiveAB = new ArrayList();     // AlarmBean's which should be added to activeListManager
        List toActiveCA = new ArrayList();     // CommentedAlarm's which should be added to activeLsit
        
        AlarmBrowsingHandler alarmBrowser = null;
        try {
            lock.acquire();
            try {
                // action
                CommentedAlarmMap tempInhibited = new CommentedAlarmMap();
                tempInhibited.putAll(inhibitList);
                
                try {
                    alarmBrowser = AlarmBrowsingHandlerFactory.getHandler(contSvcs); // laser exception
                    
                    for (int i = 0; i < uninhibitedAlarms.length; i++) {
                        String alarmId = uninhibitedAlarms[i].getAlarmId();
                        CommentedAlarm ca = tempInhibited.remove(alarmId);
                        
                        // get current status from BL
                        Alarm currentStateAlarm = alarmBrowser.getAlarmById(alarmId);
                        //logger.debug("currentStateAlarm.isActive: " + currentStateAlarm.getStatus().isActive());
                        boolean alarmBelongsToCategories= false;
                        Collection categories = currentStateAlarm.getCategories();
                        for (Iterator iter = categories.iterator(); iter.hasNext(); ) {
                            Category cat = (Category) iter.next();
                            if ( catSel.contains(cat) ) {
                                alarmBelongsToCategories = true;
                                break;
                            }
                        }
                        
                        // check if this alarm should be added to active
                        if ( alarmBelongsToCategories && (currentStateAlarm.getStatus().isActive() ||
                        (!currentStateAlarm.getStatus().isActive() && !autoterminated)) ) {
                            
                            ca.setAlarm(currentStateAlarm);
                            toActiveCA.add(ca);
                            toActiveAB.add(uninhibitedAlarms[i]);
                            
                        }
                    }
                    
                    // configuration set new inhibited
                    if ( AppRegister.getInstance().isGuestMode() == false ) {
                        conf.setInhibited(tempInhibited);   // here LaserConsoleException can be thrown
                    }
                    // update inhibitList
                    for (int i = 0; i < uninhibitedAlarms.length; i++)
                        inhibitList.remove(uninhibitedAlarms[i].getAlarmId());
                    
                    // update inhibitListManger
                    AlarmNodeManager inhibitnManager = getAlarmNodeManager(Constants.INHIBIT_LISTENER_KEY);
                    for (int i = 0; i < uninhibitedAlarms.length; i++) {
                        inhibitnManager.removeAlarm(uninhibitedAlarms[i]);
                        // update info panel
                        for (Iterator iter = alarmNumberChangeObservers.values().iterator();
                        iter.hasNext();) {
                            AlarmsNumberChangeListener listener =
                            (AlarmsNumberChangeListener) iter.next();
                            listener.decreaseCounter(
                            AlarmsNumberChangeListener.INHIBITED_ALARM_COUNTER);
                        }
                    }
                    
                    AlarmNodeManager actvienManager = getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
                    for (Iterator abIter = toActiveAB.iterator(), caIter = toActiveCA.iterator();
                    abIter.hasNext() && caIter.hasNext(); ) {
                        
                        // add to active
                        AlarmBean alarmNew = (AlarmBean) abIter.next();
                        alarmNew.setIsAlarmNodeActive();
                        activeList.put((CommentedAlarm) caIter.next());
                        
                        // add to activeNodeManager
                        actvienManager.addAlarm(alarmNew);
                        
                        // update info panel
                        for (Iterator iter = alarmNumberChangeObservers.values().iterator();
                        iter.hasNext();) {
                            AlarmsNumberChangeListener listener =
                            (AlarmsNumberChangeListener) iter.next();
                            listener.increaseCounter(
                            AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER);
                        }
                    }
                    
                } catch (LaserConsoleException lce) {
                    lock.release();
                    isLockReleased = true;
                    
                    logger.error(lce, lce.fillInStackTrace());
                    logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                    TraceLogger.log(lce.fillInStackTrace());
                    AcWindowManager.notifyError("Alarms can't be uninhibited.\n Request can't be served");
                } catch (LaserException le) {
                    lock.release();
                    isLockReleased = true;
                    
                    logger.error(le, le.fillInStackTrace());
                    logger.error(le.getRootCause(), le.getRootCause().fillInStackTrace());
                    AcWindowManager.notifyError("AlrmBrowsinHandler can't be found.\n" +
                    "Can't connect to database.");
                }
                
                // end action
            } finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
        
    }
    
    /** this method masks set of alarms, when user selects set of alarm and
     * then chooses MaskAction
     *
     * @param maskedAlarm alarms to be masked
     * @param comment commnet which will be applied to all alarms from
     *              maskedAlarm
     */
    public void mask(AlarmBean [] maskedAlarm, Comment comment) {
        boolean isLockReleased = false;
        try {
            lock.acquire();
            try {
                // action
                // this is done because when exception ocurrs all this alarms should
                // be deleted from inhibit list
                CommentedAlarm [] commAlarms = new CommentedAlarm[maskedAlarm.length];
                for (int i = 0; i < maskedAlarm.length; i++) {
                    commAlarms[i] = maskedAlarm[i].getCommentedAlarm();
                    commAlarms[i].setComment(comment);
                }
                
                CommentedAlarmMap tempMasked = new CommentedAlarmMap();
                tempMasked.putAll(maskedList);
                
                // add all alarm to inhibit list and do configuration.setMasked
                for (int i = 0; i < commAlarms.length; i++)
                    tempMasked.put(commAlarms[i]);
                
                try {
                    
                    Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
                    if ( AppRegister.getInstance().isGuestMode() == false ) {
                        conf.setMasked(tempMasked);   // here Exception can be thrown
                    }
                    CommentedAlarmMap list = getList(Constants.MASKED_LISTENER_KEY);
                    for (int i = 0; i < commAlarms.length; i++)
                        list.put(commAlarms[i]);
                    
                    
                    // remove from active list
                    for (int i = 0; i < commAlarms.length; i++)
                        activeList.remove(commAlarms[i].getAlarm().getAlarmId());
                    
                    // remove from activeListNodeManager
                    AlarmNodeManager actvienManager = getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
                    AlarmNodeManager masknManager = getAlarmNodeManager(Constants.MASKED_LISTENER_KEY);
                    
                    for (int i = 0; i < maskedAlarm.length; i++) {
                        actvienManager.removeAlarm(maskedAlarm[i]);
                        
                        // add to inhibit list
                        //AlarmBean newAlarm = new AlarmBean(commAlarms[i]);
                        AlarmBean newAlarm = maskedAlarm[i];
                        newAlarm.setIsAlarmNodeMasked(true);
                        
                        
                        // add to ihnibit list node manager
                        masknManager.addAlarm(newAlarm);
                        
                        updateAlarmNumberChangeListeners(
                        AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER,
                        AlarmsNumberChangeListener.MASKED_ALARM_COUNTER);
                        
                    }
                    
                } catch (LaserConsoleException lce) {
                    
                    //TODO: clear comments should be done here
                    
                    
                    lock.release();
                    isLockReleased = true;
                    logger.error(lce, lce.fillInStackTrace());
                    logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                    TraceLogger.log(lce.fillInStackTrace());
                    AcWindowManager.notifyError("Alarm can't be masked.\n" +
                    "Internal error.\n" +
                    "See log files or contact console developers.");
                }
                
                // end action
            } finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
        
    }
    
    /** this method unmasks set of alarms, when user selects set of alarm and
     * then chooses UnmaskAction
     *
     * @param unmaskedAlarms alarms to be inhibited
     */
    public void unmask(AlarmBean [] unmaskedAlarms, ContainerServicesBase contSvcs) {
        boolean isLockReleased = false;
        
        Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
        CategorySelection catSel =
        AppRegister.getInstance().getSelection().getCategorySelection();
        
        List toActiveAB = new ArrayList();     // AlarmBean's which should be added to activeListManager
        List toActiveCA = new ArrayList();     // CommentedAlarm's which should be added to activeLsit
        
        AlarmBrowsingHandler alarmBrowser = null;
        
        try {
            lock.acquire();
            try {
                // action
                CommentedAlarmMap tempMasked = new CommentedAlarmMap();
                tempMasked.putAll(maskedList);
                
                try {
                    alarmBrowser = AlarmBrowsingHandlerFactory.getHandler(contSvcs); // laser exception
                    
                    for (int i = 0; i < unmaskedAlarms.length; i++) {
                        String alarmId = unmaskedAlarms[i].getAlarmId();
                        CommentedAlarm ca = tempMasked.remove(alarmId);
                        
                        // get current status from BL
                        Alarm currentStateAlarm = alarmBrowser.getAlarmById(alarmId);
                        //logger.debug("currentStateAlarm.isActive: " + currentStateAlarm.getStatus().isActive());
                        boolean alarmBelongsToCategories= false;
                        Collection categories = currentStateAlarm.getCategories();
                        for (Iterator iter = categories.iterator(); iter.hasNext(); ) {
                            Category cat = (Category) iter.next();
                            if ( catSel.contains(cat) ) {
                                alarmBelongsToCategories = true;
                                break;
                            }
                        }
                        
                        // check if this alarm should be added to active
                        if ( alarmBelongsToCategories ) {
                            
                            ca.setAlarm(currentStateAlarm);
                            toActiveCA.add(ca);
                            toActiveAB.add(unmaskedAlarms[i]);
                            
                        }
                    }
                    
                    // configuration set new masked
                    if ( AppRegister.getInstance().isGuestMode() == false ) {
                        conf.setInhibited(tempMasked);   // here LaserConsoleException can be thrown
                    }
                    // update maskedList
                    for (int i = 0; i < unmaskedAlarms.length; i++)
                        maskedList.remove(unmaskedAlarms[i].getAlarmId());
                    
                    // update maskListManger
                    AlarmNodeManager masknManager = getAlarmNodeManager(Constants.MASKED_LISTENER_KEY);
                    for (int i = 0; i < unmaskedAlarms.length; i++) {
                        masknManager.removeAlarm(unmaskedAlarms[i]);
                        // update info panel
                        for (Iterator iter = alarmNumberChangeObservers.values().iterator();
                        iter.hasNext();) {
                            AlarmsNumberChangeListener listener =
                            (AlarmsNumberChangeListener) iter.next();
                            listener.decreaseCounter(
                            AlarmsNumberChangeListener.MASKED_ALARM_COUNTER);
                        }
                    }
                    
                    AlarmNodeManager actvienManager = getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
                    for (Iterator abIter = toActiveAB.iterator(), caIter = toActiveCA.iterator();
                    abIter.hasNext() && caIter.hasNext(); ) {
                        
                        // add to active
                        AlarmBean alarmNew = (AlarmBean) abIter.next();
                        alarmNew.setIsAlarmNodeActive();
                        activeList.put((CommentedAlarm) caIter.next());
                        
                        // add to activeNodeManager
                        actvienManager.addAlarm(alarmNew);
                        
                        // update info panel
                        for (Iterator iter = alarmNumberChangeObservers.values().iterator();
                        iter.hasNext();) {
                            AlarmsNumberChangeListener listener =
                            (AlarmsNumberChangeListener) iter.next();
                            listener.increaseCounter(
                            AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER);
                        }
                    }
                    
                    
                    
                } catch (LaserConsoleException lce) {
                    lock.release();
                    isLockReleased = true;
                    
                    logger.error(lce, lce.fillInStackTrace());
                    logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                    TraceLogger.log(lce.fillInStackTrace());
                    AcWindowManager.notifyError("Alarms can't be unmasked.\n Request can't be served");
                } catch (LaserException le) {
                    lock.release();
                    isLockReleased = true;
                    
                    logger.error(le.getMessage(), le);
                    logger.error(le.getRootCause().getMessage(), le.getRootCause());
                    AcWindowManager.notifyError("AlrmBrowsinHandler can't be found.\n" +
                    "Can't connect to database.");
                }
                
                // end action
            } finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
        
    }
    
    /** this method masks set of alarms, when user selects set of alarm and
     * then chooses MaskAction
     *
     * @param maskedAlarm alarms to be masked
     * @param comment commnet which will be applied to all alarms from
     *              maskedAlarm
     * @param highlightOption one of AlarmConstants.HIGHLIGHTED_ONLY,
     *              AlarmConstants.KLAXON_ONLY,
     *              AlarmConstants.HIGHLIGHTED_AND_KLAXON
     */
    public void highlightAndKlaxon(AlarmBean [] highlightedAndKlaxonedAlarms,
    Comment comment, int highlightOption) {
        
        
        boolean isLockReleased = false;
        try {
            lock.acquire();
            try {
                // action
                // this is done because when exception ocurrs all this alarms should
                // be deleted from hisghlightedAndKlaxoned list
                CommentedAlarm [] commAlarms = new CommentedAlarm[highlightedAndKlaxonedAlarms.length];
                for (int i = 0; i < highlightedAndKlaxonedAlarms.length; i++) {
                    commAlarms[i] = highlightedAndKlaxonedAlarms[i].getCommentedAlarm();
                    commAlarms[i].setComment(comment);
                }
                
                CommentedAlarmMap tempHighlightedAndKlaxon = new CommentedAlarmMap();
                tempHighlightedAndKlaxon.putAll(highlightedAndKlaxonList);
                for (int i = 0; i < commAlarms.length; i++)
                    tempHighlightedAndKlaxon.put(commAlarms[i]);
                
                
                try {
                    
                    Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
                    if ( AppRegister.getInstance().isGuestMode()==false ) {
                        conf.setAutoHighlighted(tempHighlightedAndKlaxon);   // here Exception can be thrown
                    }
                    //conf.setHighlightedKlaxoned(tempHighlightedAndKlaxon);   // here Exception can be thrown
                    
                    // add all alarm to highlightedAndKlaxoned list and do configuration.setMasked
                    CommentedAlarmMap list = getList(Constants.HIGHLITED_LISTENER_KEY);
                    for (int i = 0; i < commAlarms.length; i++)
                        list.put(commAlarms[i]);
                    
                    AlarmNodeManager highlightedAndKlaxonednManager = getAlarmNodeManager(Constants.HIGHLITED_LISTENER_KEY);
                    
                    for (int i = 0; i < highlightedAndKlaxonedAlarms.length; i++) {
                        //actvienManager.removeAlarm(inhibitedAlarm[i]);
                        highlightedAndKlaxonedAlarms[i].setIsAlarmNodeHighlightedAndKlaxon(true);
                        highlightedAndKlaxonedAlarms[i].setHighlightedOrKlaxon(highlightOption);
                        
                        // add to highlighted and klaxon list
                        try {
                            AlarmBean newAlarm = (AlarmBean) highlightedAndKlaxonedAlarms[i].clone();
                            newAlarm.setIsOnHighlightedList(true);
                            
                            
                            // connect nodes using PropertyChangeListener
                            newAlarm.registerPropertyChangeListener(
                            highlightedAndKlaxonedAlarms[i]);
                            highlightedAndKlaxonedAlarms[i].registerPropertyChangeListener(
                            newAlarm);
                            
                            // add to ihnibit list node manager
                            highlightedAndKlaxonednManager.addAlarm(newAlarm);
                        } catch (CloneNotSupportedException cnse) {
                            // should never happen, but .........
                            highlightedAndKlaxonedAlarms[i].setIsAlarmNodeHighlightedAndKlaxon(false);
                            list.remove(highlightedAndKlaxonedAlarms[i].getAlarmId());
                            logger.error(cnse, cnse.fillInStackTrace());
                        }
                        
                        //updateAlarmNumberChangeListeners(
                        //        AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER,
                        //        AlarmsNumberChangeListener.xxxxxx_highlighted_not_imlemented_not_needed_ALARM_COUNTER);
                        
                    }
                    
                } catch (LaserConsoleException lce) {
                    
                    //TODO: clear commnet should be done here
                    
                    lock.release();
                    isLockReleased = true;
                    
                    logger.error(lce, lce.fillInStackTrace());
                    logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                    TraceLogger.log(lce.fillInStackTrace());
                    AcWindowManager.notifyError("Alarm can't be highlighted and klaxoned.\n" +
                    "Internal error.\n" +
                    "See log files or contact console developers.");
                }
                // end action
            } finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
        
        
        
    }
    
    /** this method removes node from autohighlihted list only*/
    public void unhighlightAndKlaxon(AlarmBean [] unhighlightedAndKlaxonAlarms) {
        boolean isLockReleased = false;
        try {
            lock.acquire();
            try {
                // action
                // this is done because when exception ocurrs all this alarms should
                // be deleted from inhibit list
                CommentedAlarm [] commAlarms = new CommentedAlarm[unhighlightedAndKlaxonAlarms.length];
                for (int i = 0; i < unhighlightedAndKlaxonAlarms.length; i++) {
                    commAlarms[i] = unhighlightedAndKlaxonAlarms[i].getCommentedAlarm();
                }
                
                CommentedAlarmMap tempHighlightedAndKlaxon = new CommentedAlarmMap();
                tempHighlightedAndKlaxon.putAll(highlightedAndKlaxonList);
                
                // remove all uninhibitedAndKlaxon from temp list
                for (int i = 0; i < commAlarms.length; i++)
                    tempHighlightedAndKlaxon.remove(commAlarms[i].getAlarm().getAlarmId());
                
                
                try {
                    Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
                    if ( AppRegister.getInstance().isGuestMode() == false ) {
                        conf.setAutoHighlighted(tempHighlightedAndKlaxon);   // here Exception can be thrown
                    }
                    //conf.setHighlightedKlaxoned(tempHighlightedAndKlaxon);   // here Exception can be thrown
                    
                    for (int i = 0; i < commAlarms.length; i++)
                        highlightedAndKlaxonList.remove(commAlarms[i].getAlarm().getAlarmId());
                    
                    
                    AlarmNodeManager highlightedAndKlaxonnManager =
                    getAlarmNodeManager(Constants.HIGHLITED_LISTENER_KEY);
                    
                    AlarmBean alarmBeanTemp = null;
                    for (int i = 0; i < unhighlightedAndKlaxonAlarms.length; i++) {
                        alarmBeanTemp = unhighlightedAndKlaxonAlarms[i];
                        
                        if ( alarmBeanTemp.isOnHighlightedList() ) {    // this alarm is on highlightedlist (highlighted explorer)
                            alarmBeanTemp.setIsAlarmNodeHighlightedAndKlaxon(false);
                        } else {
                            alarmBeanTemp.setIsAlarmNodeHighlightedAndKlaxon(false);
                        }
                        highlightedAndKlaxonnManager.removeAlarm(alarmBeanTemp);
                    }
                } catch (LaserConsoleException lce) {
                    lock.release();
                    isLockReleased = true;
                    
                    logger.error(lce, lce.fillInStackTrace());
                    logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                    TraceLogger.log(lce.fillInStackTrace());
                    AcWindowManager.notifyError("Alarm can't be unhighlighted.\n" +
                    "Internal error.\n" +
                    "See log files or contact console developers.");
                }
                
                // end action
            } finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
    }
    
    /** this methods highlights alarm (not auto highlight)
     *
     * @param highlighted
     * @param highlight indicates whether alarms should be highlighted or not
     *
     */
    public void highlight(AlarmBean [] highlighted, boolean highlight) {
        
        // send new highlighted list to business layer
        CommentedAlarmMap tempMap = new CommentedAlarmMap();
        tempMap.putAll(highlightedList);
        
        if ( highlight )
            for (int i = 0; i < highlighted.length; i++)
                tempMap.put(highlighted[i].getCommentedAlarm());
        else
            for (int i = 0; i < highlighted.length; i++)
                tempMap.remove(highlighted[i].getAlarmId());
        
        Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
        
        try {
            if ( AppRegister.getInstance().isGuestMode() == false) {
                conf.setHighlighted(tempMap);   // here exception might be thrown
            }
            if ( highlight )
                // set is alarm node highlithed
                for (int i = 0; i < highlighted.length; i++) {
                    highlighted[i].setIsAlarmNodeHighlighted(highlight);
                    highlightedList.put(highlighted[i].getCommentedAlarm());
                }
            else
                for (int i = 0; i < highlighted.length; i++) {
                    highlighted[i].setIsAlarmNodeHighlighted(highlight);
                    highlightedList.remove(highlighted[i].getAlarmId());
                }
        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
            TraceLogger.log(lce.fillInStackTrace());
            AcWindowManager.notifyError("Alarms can't be highlighted");
        }
        
    }
    
    /**
     * @param acknowledged
     * @param acknowledge indicates whether alarms should be acknowledged or not
     * @param comment if alarm should be acknowledged, this is comment
     *                if acknowledge is false this can be null
     */
    public void acknowledge(AlarmBean [] acknowledged, boolean acknowledge,
    Comment comment) {
        
        if ( acknowledge )
            acknowledge(acknowledged, comment);
        else
            unacknowledge(acknowledged);
    }
    
    private void acknowledge(AlarmBean [] acknowledge, Comment comment) {
        boolean isLockReleased = false;
        // if alarm is termianated delete is
        List removedFromActive = new java.util.ArrayList(); // list with AlarmBean objects
        List addedToAcknowleded = new java.util.ArrayList();
        
        try {
            lock.acquire();
            try {
                // action
                for (int i = 0; i < acknowledge.length; i++) {
                    if ( acknowledge[i].isActive() ) {
                        acknowledge[i].getCommentedAlarm().setComment(comment);
                        addedToAcknowleded.add(acknowledge[i]);
                    } else {
                        removedFromActive.add(acknowledge[i]);
                    }
                }
                
                CommentedAlarmMap tempAckn = new CommentedAlarmMap();
                tempAckn.putAll(acknowledgedList);
                
                for (Iterator iter = addedToAcknowleded.iterator(); iter.hasNext(); ) {
                    tempAckn.put(((AlarmBean) iter.next()).getCommentedAlarm());
                }
                try {
                    
                    Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
                    if ( AppRegister.getInstance().isGuestMode() == false ) {
                        conf.setAcknowledged(tempAckn);   // here Exception can be thrown
                    }
                    AlarmNodeManager actvienManager = getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
                    
                    // remove from active
                    for (int i = 0; i < removedFromActive.size(); i++) {
                        AlarmBean ab = (AlarmBean) removedFromActive.get(i);
                        activeList.remove(ab.getAlarmId());
                        actvienManager.removeAlarm(ab);
                        
                        // update alarm no changeListeners
                        updateAlarmNoChangeListeners(
                        AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER);
                    }
                    
                    // update that which should be acknowledged
                    for (int i = 0; i < addedToAcknowleded.size(); i++) {
                        AlarmBean ab = (AlarmBean) addedToAcknowleded.get(i);
                        acknowledgedList.put(ab.getCommentedAlarm());
                        ab.setIsAlarmNodeAcknowledged(true);
                    }
                    
                    
                } catch (LaserConsoleException lce) {
                    lock.release();
                    isLockReleased = true;
                    logger.error(lce, lce.fillInStackTrace());
                    logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                    
                    TraceLogger.log(lce.fillInStackTrace());
                    AcWindowManager.notifyError("Alarms can't be acknowledged.\n" +
                    "Internal error.\n" +
                    "See log files or contact console developers.");
                }
                // end action
            } finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
        
    }
    
    private void unacknowledge(AlarmBean [] unacknowledged) {
        boolean isLockReleased = false;
        try {
            lock.acquire();
            try {
                //action
                CommentedAlarmMap tempUcknowledged = new CommentedAlarmMap();
                tempUcknowledged.putAll(acknowledgedList);
                
                
                for (int i = 0; i < unacknowledged.length; i++) {
                    tempUcknowledged.remove(unacknowledged[i].getAlarmId());
                }
                
                try {
                    Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
                    if ( AppRegister.getInstance().isGuestMode() == false ) {
                        conf.setAcknowledged(tempUcknowledged);   // here Exception can be thrown
                    }
                    for (int i = 0; i < unacknowledged.length; i++) {
                        acknowledgedList.remove(unacknowledged[i].getAlarmId());
                    }
                    
                    for (int i = 0; i < unacknowledged.length; i++) {
                        unacknowledged[i].setIsAlarmNodeAcknowledged(false);
                    }
                    
                } catch  (LaserConsoleException lce) {
                    lock.release();
                    isLockReleased = true;
                    
                    logger.error(lce, lce.fillInStackTrace());
                    logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                    TraceLogger.log(lce.fillInStackTrace());
                    AcWindowManager.notifyError("Alarm can't be unhighlighted.\n" +
                    "Internal error.\n" +
                    "See log files or contact console developers.");
                }
                
                
                
                //end: action
            } finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
    }
    
    /**
     * Removes all new alarms from active list
     * (Remove all new indicators, or clear all terminated)
     *
     * @param which alarm should be deleted ("NEW", "TERMINATE")
     */
    public void removeNewOrTerminatedAlarms(String whichAlarm
                                   /* maybe from which list, now only from
                                   active list */) {
                                       
                                       boolean doSomethnig = false;
                                       final int NEW = 1, TERMINATE = 2;
                                       int which = (whichAlarm.equals("NEW") ? NEW : TERMINATE);
                                       
                                       String _whichList = Constants.ACTIVE_LISTENER_KEY;
                                       
                                       AlarmNodeManager nManager = getAlarmNodeManager(_whichList);
                                       
                                       //Set set = activeList.keySet();
                                       Collection values = activeList.values();
                                       
                                       synchronized(activeList) {
                                           logger.debug("remove all new indicators inside synchronized block iter.remove()");
                                           // remove active alarms from active list
                                           for (Iterator listIter = values.iterator(); listIter.hasNext();) {
                                               CommentedAlarm commAlarm = (CommentedAlarm) listIter.next();
                                               AlarmBean alarmBean = nManager.getAlarm(commAlarm.getAlarm().getAlarmId());
                                               switch (which) {
                                                   case NEW:
                                                       doSomethnig = alarmBean.isNew();
                                                       break;
                                                   case TERMINATE:
                                                       doSomethnig = !alarmBean.isActive();
                                                       break;
                                               }
                                               if ( doSomethnig ) {
                                                   
                                                   listIter.remove();
                                                   // remove alarm from activeNodeManager
                                                   nManager.removeAlarm(alarmBean);
                                                   
                                                   // update status panel
                                                   for (Iterator iter = alarmNumberChangeObservers.values().iterator();
                                                   iter.hasNext();) {
                                                       
                                                       AlarmsNumberChangeListener listener =
                                                       (AlarmsNumberChangeListener) iter.next();
                                                       listener.decreaseCounter(
                                                       AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER);
                                                   }
                                               }
                                               doSomethnig = false;
                                           }
                                       }
    }
    
    /**
     * This method check all new alarm as not new
     */
    public void makeAlarmNotNew() {
        AlarmNodeManager activenManager =
        getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
        activenManager.makeAlarmsNotNew();
    }
    public void fireFontChanged() {
        Iterator iter = nodeManagers.values().iterator();
        while ( iter.hasNext() ) {
            AlarmNodeManager nodeManager = (AlarmNodeManager) iter.next();
            if( nodeManager != null) {
                nodeManager.fireFontChanged();
            }
        }
    }
    /**
     * This method removes all alarms from inhibit list and adds to active list
     * and updates all listeners
     *
     */
    public void clearInhibitList() {
        
        boolean isAlarmAutoTerminated =
        AppRegister.getInstance().getBehaviour().isAlarmAutoTerminated();
        
        Configuration configuartion = AppRegister.getInstance().getLoadedConfiguration();
        CategorySelection catSel =
        AppRegister.getInstance().getSelection().getCategorySelection();
        
        String _inhibitList = Constants.INHIBIT_LISTENER_KEY;
        String _activeList = Constants.ACTIVE_LISTENER_KEY;
        AlarmNodeManager nManagerActiveList = getAlarmNodeManager(_activeList);
        AlarmNodeManager nManagerIhibitList = getAlarmNodeManager(_inhibitList);
        
/*
 
 */
        try {
            lock.acquire();
            // action
            try {
                //action
                // check which nodes from inhibit list should be add to active
                for (Iterator list_iter = inhibitList.values().iterator(); list_iter.hasNext();) {
                    CommentedAlarm commAlarm = (CommentedAlarm) list_iter.next();
                    logger.debug("commAlarm="+commAlarm);
                    Alarm alarm = commAlarm.getAlarm();
                    logger.debug("alarm="+alarm);
                    list_iter.remove();
                    
                    AlarmBean alarmBean = nManagerIhibitList.getAlarm(
                    commAlarm.getAlarm().getAlarmId());
                    logger.debug("alarmBean="+alarmBean);
                    
                    nManagerIhibitList.removeAlarm(alarmBean);
                    logger.debug("after manager.removeAlarm");
                    alarmBean.setIsAlarmNodeActive();
                    logger.debug("alarmBean="+alarmBean);
                    
                    // check whether alarm should be added to active
                    if ( alarm.getStatus().isActive() ||
                    (!alarm.getStatus().isActive() && !isAlarmAutoTerminated) ) {
                        
                        boolean shouldBeAddedToActive = false;
                        
                        // check if conf.getSelection.getCategorySelection().get
                        Collection categories = commAlarm.getAlarm().getCategories();
                        //logger.debug("alarm categories: " + categories);
                        for (Iterator iter = categories.iterator(); iter.hasNext(); ) {
                            Category cat = (Category) iter.next();
                            if ( catSel.contains(cat) ) {
                                shouldBeAddedToActive = true;
                                break;
                            }
                        }
                        if ( shouldBeAddedToActive ) {
                            activeList.put(commAlarm);
                            nManagerActiveList.addAlarm(alarmBean);
                            updateAlarmNumberChangeListeners(
                            AlarmsNumberChangeListener.INHIBITED_ALARM_COUNTER,
                            AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER);
                        }
                    } else {
                        for (Iterator iter = alarmNumberChangeObservers.values().iterator();
                        iter.hasNext();) {
                            
                            AlarmsNumberChangeListener listener =
                            (AlarmsNumberChangeListener) iter.next();
                            listener.decreaseCounter(
                            AlarmsNumberChangeListener.INHIBITED_ALARM_COUNTER);
                        }
                        
                    }
                    
                }
            } finally {
                lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
        
        // send empty inhibit list to BL
        try {
            configuartion.setInhibited(inhibitList);
        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
            TraceLogger.log(lce.fillInStackTrace());
            AcWindowManager.notifyError("New inhibit list can't be persisted.");
        }
        
        
    }
    
    public void clearSearchList() {
        AlarmNodeManager searchListNodeManager = getAlarmNodeManager(Constants.SEARCH_LISTENER_KEY);
        searchListNodeManager.removeAllAlarms();
    }
    
    /**
     * This method removes all alarms from mask list and adds to active list
     * and updates all listeners
     *
     */
    public void clearMaskList() {
        
        boolean isAlarmAutoTerminated =
        AppRegister.getInstance().getBehaviour().isAlarmAutoTerminated();
        
        Configuration configuartion = AppRegister.getInstance().getLoadedConfiguration();
        CategorySelection catSel =
        AppRegister.getInstance().getSelection().getCategorySelection();
        
        
        String _maskList = Constants.MASKED_LISTENER_KEY;
        String _activeList = Constants.ACTIVE_LISTENER_KEY;
        AlarmNodeManager nManagerActiveList = getAlarmNodeManager(_activeList);
        AlarmNodeManager nManageriMaskList = getAlarmNodeManager(_maskList);
        
        try {
            lock.acquire();
            try {
                // action
                for (Iterator listIter = maskedList.values().iterator(); listIter.hasNext();) {
                    CommentedAlarm commAlarm = (CommentedAlarm) listIter.next();
                    Alarm alarm = commAlarm.getAlarm();
                    listIter.remove();
                    AlarmBean alarmBean = nManageriMaskList.getAlarm(
                    commAlarm.getAlarm().getAlarmId());
                    nManageriMaskList.removeAlarm(alarmBean);
                    alarmBean.setIsAlarmNodeActive();
                    
                    // check whether alarm should be added to active
                    if ( alarm.getStatus().isActive() ) {
                        
                        boolean shouldBeAddedToActive = false;
                        
                        // check if conf.getSelection.getCategorySelection().get
                        Collection categories = commAlarm.getAlarm().getCategories();
                        //logger.debug("alarm categories: " + categories);
                        for (Iterator iter = categories.iterator(); iter.hasNext(); ) {
                            Category cat = (Category) iter.next();
                            if ( catSel.contains(cat) ) {
                                shouldBeAddedToActive = true;
                                break;
                            }
                        }
                        if ( shouldBeAddedToActive ) {
                            activeList.put(commAlarm);
                            nManagerActiveList.addAlarm(alarmBean);
                            updateAlarmNumberChangeListeners(
                            AlarmsNumberChangeListener.MASKED_ALARM_COUNTER,
                            AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER);
                        }
                    } else {
                        for (Iterator iter = alarmNumberChangeObservers.values().iterator();
                        iter.hasNext();) {
                            
                            AlarmsNumberChangeListener listener =
                            (AlarmsNumberChangeListener) iter.next();
                            listener.decreaseCounter(
                            AlarmsNumberChangeListener.MASKED_ALARM_COUNTER);
                        }
                        
                    }
                    
                }
            } finally {
                lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
        
        // send empty masked list to BL
        try {
            configuartion.setMasked(maskedList);
        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
            TraceLogger.log(lce.fillInStackTrace());
            AcWindowManager.notifyError("New mask list can't be persisted.\n" +
            "Try do it later.");
        }
        
    }
    
    /**
     * This method removes all alarm from "autohighilighted" list
     * In order to remove alarms from autoKlaxon list use clearAutoKlaxonList
     * (implement this method)
     */
    public void clearAutoHighlightedList() {
        
        Configuration configuartion = AppRegister.getInstance().getLoadedConfiguration();
        
        String _highlightedAndKlaxonList = Constants.HIGHLITED_LISTENER_KEY;
        String _activeList = Constants.ACTIVE_LISTENER_KEY;
        AlarmNodeManager nManagerActiveList = getAlarmNodeManager(_activeList);
        AlarmNodeManager nManageriHighlitghtedList =
        getAlarmNodeManager(_highlightedAndKlaxonList);
        
        synchronized(highlightedAndKlaxonList) {
            for (Iterator listIter = highlightedAndKlaxonList.values().iterator();
            listIter.hasNext();) {
                CommentedAlarm  commAlarm = (CommentedAlarm) listIter.next();
                listIter.remove();
                
                // remove alarm from activeNodeManager
                AlarmBean alarmBean = nManageriHighlitghtedList.getAlarm(
                commAlarm.getAlarm().getAlarmId());
                alarmBean.setIsAlarmNodeHighlightedAndKlaxon(false);
                nManageriHighlitghtedList.removeAlarm(alarmBean);
                
            }
        }
        
        // send empty masked list to BL
        try {
            configuartion.setAutoHighlighted(highlightedAndKlaxonList);
        } catch (LaserConsoleException lce) {
            logger.error(lce, lce.fillInStackTrace());
            logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
            TraceLogger.log(lce.fillInStackTrace());
            AcWindowManager.notifyError("New mask list can't be persisted.");
        }
        
        
    }
    
    /**
     * Move alarm numbres from one label to other.  This method substracts
     * 1 from fromCounterType and adds 1 to toCounterType
     *
     * @param fromCounterType
     * @param toCounterType
     *
     * // synchronized here should be tested
     */
    private void updateAlarmNumberChangeListeners(final int fromCounterType,
    final int toCounterType) {
        
        // update static panel
        // update all AlarmNumberChangeListeners
        Collection alarmNoChanLis = alarmNumberChangeObservers.values();
        Iterator iter = alarmNoChanLis.iterator();
        while (iter.hasNext()) {
            AlarmsNumberChangeListener listener =
            (AlarmsNumberChangeListener) iter.next();
            listener.moveAlarmNumber(fromCounterType, toCounterType);
        }
        
    }
    
    //
    // -- implements AlarmSelectionListener ----------------------------------
    //
    /** this method will be invoked by business layer when on alarm arrives
     * @param alarm alarm from business layer
     *
     */
    public void onAlarm(Alarm _alarm) {
        
        String alarmId = _alarm.getAlarmId();
        String ff = _alarm.getTriplet().getFaultFamily();
        String fm = _alarm.getTriplet().getFaultMember();
        Integer fc = _alarm.getTriplet().getFaultCode();
        
        //logger.debug("on alarm method, alarm Id: " + alarmId + " " + ff + " " + fm + " " + fc);
        try {
            lock.acquire();
            try {
                //action
                if ( inhibitList.containsKey(alarmId) ) {
                    updateAlarmOnInhibitList(_alarm);
                } else if ( maskedList.containsKey(alarmId) ) {
                    updateAlarmMaskedOnList(_alarm);
                } else if ( _alarm.isInstant() ) {
                    updateAlarmOnInstantList(_alarm);
                } else if ( activeList.containsKey(alarmId) ) {
                    updateAlarmOnActiveList(_alarm);
                } else {
                    addAlarmToActiveList(_alarm);
                }
                // end: action
            } finally {
                lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }
        
    }
    
    /** callback method */
    public void onException(LaserSelectionException laserSelectionException) {
        //throw new UnsupportedOperationException(" not implemented yet ");
        for(Iterator iter = onExceptionListeners.iterator(); iter.hasNext(); )
            ((AlarmSelectionOnExceptionListener)
            iter.next()).onException(laserSelectionException.getCode());
        
        
        // where exception code is CONNECTION_ESTABLISHED
        // container should be raloaded
        
        
        
    }
    
    private void updateAlarmOnInstantList(Alarm newInstant) {
        CommentedAlarm oldAlarm = null;
        
        oldAlarm = instantList.get(newInstant.getAlarmId());
        if ( oldAlarm == null ) {
            oldAlarm = new CommentedAlarm(newInstant, null);
            instantList.put(oldAlarm);
            AlarmNodeManager nManager =
            getAlarmNodeManager(Constants.INSTANT_LISTENER_KEY);
            nManager.addAlarm(new AlarmBean(oldAlarm));
        } else {
            oldAlarm.setAlarm(newInstant);
            // 2. find on node manager replace it and fireNamePropertyChange()
            AlarmNodeManager nManager = getAlarmNodeManager(Constants.INSTANT_LISTENER_KEY);
            AlarmBean alarmBean = nManager.getAlarm(newInstant.getAlarmId());
            alarmBean.setCommentedAlarm(oldAlarm);
        }
    }
    
    private void updateAlarmOnActiveList(Alarm newAlarm) {
        String ff = newAlarm.getTriplet().getFaultFamily();
        String fm = newAlarm.getTriplet().getFaultMember();
        Integer fc = newAlarm.getTriplet().getFaultCode();
        logger.debug("updateAlarmOnActiveList(" + ff + " " + fm +
        " " + fc + ")");
        
        Behaviour behaviour = AppRegister.getInstance().getBehaviour();
        
        if ( newAlarm.getStatus().isActive() ||
        (!newAlarm.getStatus().isActive() &&
        !behaviour.isAlarmAutoTerminated() &&
        !acknowledgedList.containsKey(newAlarm.getAlarmId())) ) {
            // in that case always update on activelist
            
            // 1. find this alarm on active
            CommentedAlarm oldAlarm = activeList.get(newAlarm.getAlarmId());
            oldAlarm.setAlarm(newAlarm);
            
            // 2. find on node manager replace
            AlarmNodeManager nManager = getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
            AlarmBean alarmBean = nManager.getAlarm(newAlarm.getAlarmId());
            alarmBean.setCommentedAlarm(oldAlarm);
        } else {
            
            if ( behaviour.isAlarmAutoTerminated() ) {
                activeList.remove(newAlarm.getAlarmId());
                AlarmNodeManager nManager = getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
                nManager.removeAlarm(newAlarm.getAlarmId());
                updateAlarmNoChangeListeners(AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER);
            } else if ( acknowledgedList.containsKey(newAlarm.getAlarmId()) ) {
                activeList.remove(newAlarm.getAlarmId());
                AlarmNodeManager nManager = getAlarmNodeManager(Constants.ACTIVE_LISTENER_KEY);
                nManager.removeAlarm(newAlarm.getAlarmId());
                updateAlarmNoChangeListeners(AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER);
                acknowledgedList.remove(newAlarm.getAlarmId());
            }
        }
        
    }
    
    private void updateAlarmOnInhibitList(Alarm newAlarm) {
        /** after last discusion do nothing */
        
            /*
            // 1. find this inhibit on active
            CommentedAlarm oldAlarm = inhibitList.get(newAlarm.getAlarmId());
            logger.debug(oldAlarm.getAlarm().getTriplet().toString() + " before: " + oldAlarm.getAlarm().getStatus().isActive());
            oldAlarm.setAlarm(newAlarm);
            logger.debug(oldAlarm.getAlarm().getTriplet().toString() + " after: " + oldAlarm.getAlarm().getStatus().isActive());
             
            // 2. find on node manager replace it and fireNamePropertyChange()
            //AlarmListener inhibitListListener =
            //    (AlarmListener) observers.get(Constants.INHIBIT_LISTENER_KEY);
            AlarmNodeManager nManager = getAlarmNodeManager(Constants.INHIBIT_LISTENER_KEY);
            AlarmBean alarmBean = nManager.getAlarm(newAlarm.getAlarmId());
            logger.debug("found alarm: " + alarmBean.getName());
            alarmBean.setCommentedAlarm(oldAlarm);
             */
    }
    
    private void updateAlarmMaskedOnList(Alarm newAlarm) {
        // if alarm is termianted remove that alarm from
        // internal list and noge manager, do not send it to BL
        if ( !newAlarm.getStatus().isActive() ) {
            try {
                
                CommentedAlarm oldAlarm = maskedList.remove(newAlarm.getAlarmId());
                
                //oldAlarm.setAlarm(newAlarm);
                
                AlarmNodeManager nManager = getAlarmNodeManager(Constants.MASKED_LISTENER_KEY);
                nManager.removeAlarm(newAlarm.getAlarmId());
                
                // update masked list in BL
                
                CommentedAlarmMap oldMasked = AppRegister.getInstance().getLoadedConfiguration().getMasked();
                oldMasked.remove( oldAlarm.getAlarm().getAlarmId());
                AppRegister.getInstance().getLoadedConfiguration().setMasked(oldMasked);
                
                // decrease masked counter
                for (Iterator iter = alarmNumberChangeObservers.values().iterator();
                iter.hasNext();) {
                    
                    AlarmsNumberChangeListener listener =
                    (AlarmsNumberChangeListener) iter.next();
                    listener.decreaseCounter(AlarmsNumberChangeListener.MASKED_ALARM_COUNTER);
                }
            }
            catch (LaserConsoleException lce) {
                logger.error(lce, lce.fillInStackTrace());
                logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                TraceLogger.log(lce.fillInStackTrace());
                AcWindowManager.notifyError("New mask list can't be persisted.");
            }
        }
        
    }
    //
    // -- helper methods -------------------------------------------------
    //
    /**
     * This method adds alarm to actvie list and updates all observers
     * @param alarm
     */
    private void addAlarmToActiveList(Alarm alarm) {
        String ff = alarm.getTriplet().getFaultFamily();
        String fm = alarm.getTriplet().getFaultMember();
        Integer fc = alarm.getTriplet().getFaultCode();
        
        logger.debug("addAlarmToActiveList(" + ff + " " + fm +
        " " + fc + ")");
        
        Behaviour behTemp = AppRegister.getInstance().getBehaviour();
        // do not add alarm when is active and auto termiante is true
        if ( !alarm.getStatus().isActive() && behTemp.isAlarmAutoTerminated() )
            return;
        
        CommentedAlarm commAlarm = new CommentedAlarm(alarm, null);
        activeList.put(commAlarm);
        
        AlarmListener activeListListener =
        (AlarmListener) observers.get(Constants.ACTIVE_LISTENER_KEY);
        
        if ( activeListListener != null )  {
            AlarmBean alarmBean = new AlarmBean(commAlarm);
            
            // check if alarm is on autoHighlighred list
            CommentedAlarm alarmTemp = null;
            if ( (alarmTemp = highlightedAndKlaxonList.get(alarm.getAlarmId())) != null ) {
                alarmBean.setIsAlarmNodeHighlightedAndKlaxon(true);
                AlarmBean beanTemp = getAlarmNodeManager(
                Constants.HIGHLITED_LISTENER_KEY).getAlarm(alarm.getAlarmId());
                if ( beanTemp != null ) {
                    beanTemp.registerPropertyChangeListener(alarmBean);
                }
            }
            
            // check if alarm is on autoKlaxon list (future)
            activeListListener.addAlarm(alarmBean);
            //logger.debug(" activeListListener added alarm " + alarmBean.getName() );
        }
        
        // update all AlarmNumberChangeListeners
        Collection alarmNoChanLis = alarmNumberChangeObservers.values();
        Iterator iter = alarmNoChanLis.iterator();
        while (iter.hasNext()) {
            AlarmsNumberChangeListener listener =
            (AlarmsNumberChangeListener) iter.next();
            listener.increaseCounter(
            AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER);
        }
        
        
    }
    
    /**
     * @param whichListener one of:
     *  AlarmsNumberChangeListener.INHIBITED_ALARM_COUNTER
     *  AlarmsNumberChangeListener.MASKED_ALARM_COUNTER
     *  AlarmsNumberChangeListener.ACTIVE_ALARM_COUNTER, etc
     */
    private void updateAlarmNoChangeListeners(int whichListener) {
        for (Iterator iter = alarmNumberChangeObservers.values().iterator();
        iter.hasNext();) {
            
            AlarmsNumberChangeListener listener =
            (AlarmsNumberChangeListener) iter.next();
            listener.decreaseCounter(whichListener);
        }
        
        
    }
    
    private boolean belongsToSelectedCategories( AlarmBean alarmBean, CategorySelection selectedCategories) {
        Collection categories = alarmBean.getCategories();
        for (Iterator catIter = categories.iterator(); catIter.hasNext(); ) {
            Category cat = (Category) catIter.next();
            logger.debug("reload container: category="+cat);
            if ( selectedCategories.contains(cat) ) {
                logger.debug("reload container: alarm on current selection (alarm cat="+cat+"/selected cat="+selectedCategories+")");
                return true;
            }
        }
        return false;
    }

    
    public void setNewIndicator(AlarmBean [] notNew) {
        boolean isLockReleased = false;
        try {
            lock.acquire();
            try {
                CommentedAlarmMap notNewMap = new CommentedAlarmMap();
                for (int i = 0; i < notNew.length; i++) {
                    if ( notNew[i] != null ) {
                    	notNewMap.put( notNew[i].getCommentedAlarm());
                    }
                }
                try {
                    Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
                    if ( AppRegister.getInstance().isGuestMode() == false ) {
                    	logger.debug("status not new persisted, alarms="+notNewMap);
                    	conf.setNewIndicator(notNewMap);   // here Exception can be thrown
                    }
                } catch (LaserConsoleException lce) {
                    lock.release();
                    isLockReleased = true;
                    logger.error(lce, lce.fillInStackTrace());
                    logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                                                                                                                                                             
                    TraceLogger.log(lce.fillInStackTrace());
                    AcWindowManager.notifyError("New indicator cannot be persisted.\n" +
                    "Internal error.\n" +
                    "See log files or contact console developers.");
                }
                // end action
            } finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }                                                                                                                                                            
    }    
    
    public void setActiveListFont(boolean isBig) {
        boolean isLockReleased = false;
        try {
            lock.acquire();
            
            try {
                Configuration conf = AppRegister.getInstance().getLoadedConfiguration();
                if ( AppRegister.getInstance().isGuestMode() == false ) {
                	logger.debug("font size persisted, isBig="+isBig);
                    conf.setActiveListFont( new Boolean(isBig) );   // here Exception can be thrown
                    logger.debug("font size stored as:"+conf.getActiveListFont());
                }
             } catch (LaserConsoleException lce) {
                lock.release();
                isLockReleased = true;
                logger.error(lce, lce.fillInStackTrace());
                logger.error(lce.getRootCause(), lce.getRootCause().fillInStackTrace());
                                                                                                                                                             
                TraceLogger.log(lce.fillInStackTrace());
                AcWindowManager.notifyError("Font Size cannot be persisted.\n" +
                "Internal error.\n" +
                "See log files or contact console developers.");
             }
             finally {
                if ( !isLockReleased )
                    lock.release();
            }
        } catch (InterruptedException ie) {
            logger.error(ie, ie.fillInStackTrace());
        }                                                                                                                                                            
    }

    public boolean isSearchCancelled() {
        // not used in this context
        return false;
    }
    
    public void onSearchAlarm(cern.laser.client.data.Alarm alarm) {
        AlarmNodeManager searchListNodeManager = getAlarmNodeManager(Constants.SEARCH_LISTENER_KEY);
        CommentedAlarm commentedAlarm = new CommentedAlarm(alarm, null);
        AlarmBean alarmBean = new AlarmBean(commentedAlarm);
        searchListNodeManager.addAlarm(alarmBean);
    }
    
    public void onSearchException(LaserSearchException laserSearchException){
        
    }
    
    public void searchFinished() {
        // open dialog with confirmation that all alarms are received
    }
    
}
