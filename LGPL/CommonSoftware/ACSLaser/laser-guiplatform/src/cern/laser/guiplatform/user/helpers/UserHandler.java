/*
 * UserHandler.java
 *
 * Created on March 6, 2003, 2:01 PM
 */

package cern.laser.guiplatform.user.helpers;

import java.util.Collection;
import java.util.List;

import org.apache.log4j.Logger;

import alma.acs.container.ContainerServicesBase;

import cern.laser.console.LaserConsoleException;

/**
 *
 * This class is only for simulating bussines layer. In the future
 * is should be changed.
 *
 *
 * @author  pawlowsk
 */
public class UserHandler extends cern.laser.console.UserHandler {
    
    /** logger for that class */
    private static final Logger logger = 
        Logger.getLogger(UserHandler.class.getName());
    
    // temp variables
    private static final int userCounter = 6;
    private static final int confCounter = 5;
    
    private static final String userName = "bartek";
    private static final String confName = "config";
  
    private static List userList = null;

    private static cern.laser.guiplatform.user.helpers.UserHandler INSTANCE = null;
    
    /** Creates a new instance of UserHandler */
    private UserHandler() {
    }
    
    public cern.laser.console.User createUser(String name, String password) throws LaserConsoleException {
        throw new UnsupportedOperationException("this method is not implemented");
    }
    
    public cern.laser.console.User getUser(String name, ContainerServicesBase contSvcs) throws LaserConsoleException {
        throw new UnsupportedOperationException("this method is not implemented");        
    }
    
    public Collection getUsers() throws LaserConsoleException {
        throw new UnsupportedOperationException("this method is not implemented");
    }
    
    public void removeUser(String name) throws LaserConsoleException {
        throw new UnsupportedOperationException("this method is not implemented");       
    }
  
     //
    // -- sigleton methods -------------
    //
    public static cern.laser.console.UserHandler get() {
        if ( INSTANCE == null ) 
            INSTANCE = new cern.laser.guiplatform.user.helpers.UserHandler();
        
        return INSTANCE;
    }

    
///////////////////////////////////////////////////////////////////////////////
//
    /** 
     * This method returns list with all users and their configurations
     * 
     * @return <code>List</code> with <code>UserBean</code> objects
     */
    /*
    public static List getUsers() {
        userList = new Vector();
        for (int i = 0; i < userCounter; i++) {
            UserBean userBean = new UserBean(userName + i);
            Vector confs = new Vector();
            for (int j = 0; j < confCounter; j++) {
                ConfigurationBean conf = 
                            new ConfigurationBean(confName + i + j);
                confs.add(conf);
            }
            userBean.setConfigurations(confs);
            userList.add(userBean);
        }
        return userList;

    }
    */
    /**
     * This method returns categories choosen by given user in the frame of
     * given configuration (configuration name)
     *
     * @param userName user name
     * @param confName configuration name
     * 
     * @deprecated this method probalby is not used
     */
    /*
    public static List getUserCategories(String userName, String confName) {
        System.out.println(" ble ble ble getUserCategories  "+ userName + " conf name " +
                confName);
        Vector list = new Vector();
        /*
        for (int i = 0; i < userCategoryCounter; i++) {
            CategoryTest category = new CategoryTest("CERN", 100, "CERN DESC");
            list.addElement(category);
        }
         */
    /*
        return list;
    }
    */
    /**
     * This method login user 
     *
     * @param userName user name
     * @param password password 
     * @return logged user <code>User</code> object or null if user does
     * not exist
     */    
    /*
    public static User login(String userName, String password) {
        User user = null;
        if ( userName.equals("bartek") ) {
            user = new User("bartek", "Bartek", "Pawlowski");
        }
        return user;
    }
    */
    
    
}
