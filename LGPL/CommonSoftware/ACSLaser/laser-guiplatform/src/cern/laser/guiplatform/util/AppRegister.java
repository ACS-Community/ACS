/*
 * AppRegister.java
 *
 * Created on March 21, 2003, 3:29 PM
 */

package cern.laser.guiplatform.util;

import java.util.HashMap;

import javax.swing.JButton;

import org.apache.log4j.Logger;

import cern.laser.client.data.Category;
import cern.laser.client.services.selection.Selection;
import cern.laser.console.Behaviour;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.guiplatform.windows.ActiveListExplorerPanel;
import cern.laser.guiplatform.windows.search.SearchWindowSettings;

/**
 * This class stores global informations, i. e. GlobalConfiguration objects, etc
 * This class is also used as global register which stores references to different
 * objects. This objects are used inside *Action classes.
 *
 * @author  pawlowsk
 */
public class AppRegister {
    
    private static Logger logger = LogFactory.getLogger(AppRegister.class.getName());
    
    /** key for actual configuration */
    public final String configurationKey = "actualConfiguraton";
    /** key for logged user */
    private final String loggedUserKey = "loggedUser";
    
    /** key for active list explorer panel */
    private final String activeListExplorerPanelKey = "activeListExplorerPanel";
    
    /** register instance */
    private static AppRegister instance = null;
    
    /** register */
    private HashMap register = null;
    
    /** if network exception ( onException method) occurs
     * this is global value and is used to disable all actions
     */
    //private boolean businessLayerExcpetion = false;
    
    /** this is configuration which is kept localy, currently loaded configuraion */
    private Configuration loadedConfiguration = null;
    
    /** this is configuration used by CategorySelectorWindow */
    private Category [] searchCategories = null;
    
    /** Behaviour
     * this is used due to fact, that Behaviour should be kept locally
     */
    private Behaviour behaviour = null;
    
    /** Selection
     * this is used due to fact, that Selection should be kept locally
     */
    private Selection selection = null;
    
    /** save button */
    private JButton saveButton = null;
    
    /** fonts for explorers */
    private final String FONT_NAME = "Dialog";
    private final int FONT_SIZE_BIG = 22;
    private final int FONT_SIZE_SMALL = 18;
    private int fontSize;
    
    /** Stored Settings of Search Window */
    private SearchWindowSettings searchWindowSettings = null;
    
    /** Creates a new instance of AppRegister */
    private AppRegister() {
        register = new HashMap();
        fontSize = FONT_SIZE_SMALL;        
    }
    
    /*
    public void updateConf(String conf){
    }
     */
    
    public boolean isFontBig() {
        return fontSize==FONT_SIZE_BIG;
    }
    /* Check if logged user is Guest or not.
     * @return true if logged user is guest
     */
    public boolean isGuestMode() {
    	String userName = getRegisteredUserName();
        if ( userName!=null && userName.compareToIgnoreCase( Constants.GUEST_USER_NAME ) == 0 ) {
            return true;
        }
        else {
            return false;
        }
    }
    public void setFontBig( boolean flag ) {
        if ( flag==true ) {
            fontSize = FONT_SIZE_BIG;
        }
        else {
            fontSize = FONT_SIZE_SMALL;
        }
    }
    public String getFontName() {
        return FONT_NAME;
    }
    public int getFontSize() {
        return fontSize;
    }
    public static synchronized AppRegister getInstance() {
        if ( instance == null )
            instance = new AppRegister();
        
        return instance;
    }
    
    public synchronized Configuration getLoadedConfiguration() {
        return loadedConfiguration;
    }
    public synchronized void registerLoadedConfiguration(Configuration configuration)
    throws LaserConsoleException {
        
        behaviour = configuration.getBehaviour();   //this throws LaserConsoleException
        selection= configuration.getSelection();   //this throws LaserConsoleException
        
        if ( loadedConfiguration != null ) loadedConfiguration = null;
        
        loadedConfiguration = configuration;
    }
    
    public synchronized Behaviour getBehaviour() {
        return behaviour;
    }
    
    public synchronized Selection getSelection() {
        return selection;
    }
    /**
     * This method register object in register
     * @param key key
     * @param obj object which should be registered
     */
    public void registerObject(Object key, Object obj) {
        register.put(key, obj);
    }
    /**
     * This method returns previous registered object
     * @param key key
     * @return object or null
     */
    public Object getRegisterdObject(Object key) {
        return register.get(key);
    }
    /**
     * This method removes Objects from register
     * @param key key
     */
    public void removeRegisteredObject(Object key) {
        register.remove(key);
    }
    
    /**
     * This method register logged user
     *
     * @param user logged user <code>User</code> object
     */
    public void registerUser(User user) {
        //logger.debug(" user " +  user.getName() + " was registered in AppRegister");
        register.put(loggedUserKey, user);
    }
    /**
     * This method unregisters user
     */
    public void unregisterUser() {
        register.remove(loggedUserKey);
    }
    /**
     * Returns registered user <code>User</code> or null
     * @return registered user or null
     */
    public User getRegisteredUser() {
        return (User) register.get(loggedUserKey);
        
    }
    /**
     * @return registered user name
     *      if there is registered user or in case of LaserConsoleException
     */
    public String getRegisteredUserName() {
        String username = null;
        User user = (User) register.get(loggedUserKey);
        try {
            if ( user != null )
                username = user.getName();
        } catch (LaserConsoleException lce) {
            logger.debug(lce, lce.fillInStackTrace());
            username = null;
        }
        
        return username;
    }
    /**
     * This method registers explorer
     */
    
    public void registerActiveListExplorerPanel(ActiveListExplorerPanel explorer) {
        register.put(activeListExplorerPanelKey, explorer);
    }
    /**
     * This method unregisters explorer
     */
    public void unregisterActiveListExplorerPanel() {
        register.remove(activeListExplorerPanelKey);
    }
    /**
     * Returns registered explorer or null
     * @return registered explorer or null
     */
    public ActiveListExplorerPanel getRegisteredActiveListExplorerPanel() {
        return (ActiveListExplorerPanel) register.get(activeListExplorerPanelKey);
        
    }
    
    /** this method is used to indicate, that business layer exception occured */
    //public void exceptionOccured() {
    //    businessLayerExcpetion = true;
    //}
    
    /** this method is used to indicate, that business layer exception is solved */
    //public void exceptionSolved() {
    //    businessLayerExcpetion = false;
    //}
    
    /** this method indicates whether is or not business layer exception */
    //public boolean exceptionStatus() {
    //    return businessLayerExcpetion;
    //}
    
    
    /** this method is used to clear register */
    public void cleanRegister() {
        
        register.clear();
        register = null;
        instance = null;
        loadedConfiguration = null;
        behaviour = null;
        logger.debug("AppRegister was cleared");
    }
    
    /**
     * This method is used to change Save button status and
     * configurationChanged variable in ConfigurationPanel
     */
    public void notifyConfigurationChange() {
        if ( saveButton != null )
            if ( isGuestMode() == false )
                saveButton.setEnabled(true);
    }
    
    public void registerConfigurationChangeListeners(JButton button) {
        saveButton = button;
    }
    
    public void clearConfigurationChangeListeners() {
        saveButton = null;
    }
    /**
     * getter for last Search Window's Setting
     * @param windowMode one of Constants.SEARCH_WINDOW_MODE_* constants
     */
    public SearchWindowSettings getSearchWindowSettings( int windowMode ) {
        //synchronized(searchWindowSettings){
            if (searchWindowSettings==null) {
                searchWindowSettings = new SearchWindowSettings(windowMode);
            }
            searchWindowSettings.setWindowMode(windowMode);
            return searchWindowSettings;
        //}
    }
    /**
     * getter for last Search Window's Setting
     * @param windowMode one of Constants.SEARCH_WINDOW_MODE_* constants
     */
    public Category [] getSearchCategories() {
        //synchronized(searchWindowSettings){
            if (searchCategories==null) {
                searchCategories = new Category[0];
            } 
            return searchCategories;
        //}
    }    
    public void setSearchCategories( Category [] categories ) {
        searchCategories = categories;
    }
}


//AppRegister.getInstance().updateConf();
