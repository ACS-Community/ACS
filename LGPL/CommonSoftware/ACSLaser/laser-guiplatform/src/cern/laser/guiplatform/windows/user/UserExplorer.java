/*
 * UserExplorer.java
 *
 * Created on March 6, 2003, 3:37 PM
 */

package cern.laser.guiplatform.windows.user;


import java.beans.IntrospectionException;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;
import org.openide.nodes.Children;
import org.openide.nodes.Node;

import cern.gp.beans.BeanSupport;
import cern.gp.nodes.GPNode;
import cern.gp.nodes.NodeFactory;
import cern.gp.nodes.children.ChildrenListManager;
import cern.gp.nodes.children.ChildrenMapManager;
import cern.gp.nodes.children.NodeCollection;
import cern.gp.nodes.children.NodeList;
import cern.gp.nodes.children.NodeMap;
import cern.laser.client.LaserException;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.guiplatform.configuration.ConfigurationBean;
import cern.laser.guiplatform.user.UserBean;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;

/**
 *
 * @author  pawlowsk
 */
public class UserExplorer extends cern.gp.explorer.TreeExplorer {
    
    private static final Logger logger =
    LogFactory.getLogger(UserExplorer.class.getName());
    
    /** user list */
    private Collection userList = null;
    
    /** user who is logged */
    private String loggedUserName = null;
    
    /** user node manager */
    private UserNodeManager userNodeManager = null;
    
    /** Creates a new instance of UserExplorer
     * @param userList list with User objects
     * @param loggedUserName
     */
    public UserExplorer(Collection userList, String loggedUserName) {
        super();
        this.userList = userList;
        this.loggedUserName = loggedUserName;
        initializeExplorer();
    }
    
    
    /** initialize explorer */
    private void initializeExplorer() {
        // remove Guest user from user collection
        userList = removeGuestUser(userList);
        
        userNodeManager = new UserNodeManager(userList, loggedUserName);
        GPNode root = null;
        try {
            root = NodeFactory.createNode(new Object(), userNodeManager);
            setRootNode(root);
            getTreeAccess().getTreeView().setRootVisible(false);
        } catch (IntrospectionException e) {
            logger.error(e, e.fillInStackTrace());
        }
        
        expandUserNode(root.getPeerNode(), loggedUserName);
        
    }
    /**
     * Removes guest user from Collection used to initialize UserExplorer
     */
    private Collection removeGuestUser(Collection userList) {
        logger.debug("Users before remove Guest:" + userList);
        for ( Iterator iter = userList.iterator(); iter.hasNext(); ) {
            try {
                if ( ((User) iter.next()).getName().compareToIgnoreCase(Constants.GUEST_USER_NAME ) ==0 ){
                    iter.remove();
                    logger.debug("GUEST user removed");
                }
            }
            catch ( LaserConsoleException lce) {
                logger.debug(lce.getMessage());
            }
        }
        return userList;
    }
    
    private void expandUserNode(Node node, String nodeName) {
        Children children = node.getChildren();
        Node nodeToSelect = children.findChild(nodeName);
        if (nodeToSelect != null) {
            getTreeAccess().getTreeView().expandNode(nodeToSelect);
        }
    }
    
    /**
     * This method adds cofigurations for given user
     *
     * @param configuration configuration which should be saved
     *      this should be <code>ConfigurationBean</code> object
     */
    public void addConfiguration(BeanSupport configuration)
    throws IntrospectionException {
        //logger.debug(getClass().getName() + " addConfiguration() for user" + loggedUserName);
        userNodeManager.addConfiguration(configuration);
    }
    
    public void removeConfiguration(GPNode node) {
        
        ConfigurationNodeManger manager =
        (ConfigurationNodeManger) node.getParent().getNodeCollection().getChildrenManager();
        manager.removeConfiguration(node);
    }
    
    public List getDefinedConfigurationNames(User user) throws LaserConsoleException {
        return userNodeManager.getConfigurationNames(user.getName());
    }
    
    /**************************************************************************
     *  UserNodeManager
     *************************************************************************/
    private class UserNodeManager implements ChildrenMapManager {
        private final Logger logger =
        LogFactory.getLogger(UserNodeManager.class.getName());
        
        /** configuration for given user */
        private NodeMap confMap;
        
        /** user list */
        private Collection userList = null;
        /** logged user name */
        private String loggedUserName = null;
        
        /** logged user node */
        private GPNode loggedUserNode = null;
        
        /**
         * @param userList list with <code>User</code> objects
         * @param loggedUserNode
         */
        public UserNodeManager(Collection userList, String loggedUserName) {
            this.userList = userList;
            this.loggedUserName = loggedUserName;
            
        }
        public void initChildrenMap(NodeMap nodeMap) {
            confMap = nodeMap;
            try {
                Iterator iter = userList.iterator();
                while ( iter.hasNext() ) {
                    User user = (User) iter.next();
                    //BeanSupport bean = (BeanSupport) iter.next();
                    UserBean bean = new UserBean(user);
                    User loggedUser = AppRegister.getInstance().getRegisteredUser();
                    if ( user.getName().equals(loggedUser.getName()) )
                        bean.setIsLogged(true);
                    //logger.debug(bean.getName());
                    GPNode node = NodeFactory.createNode(bean,
                    new ConfigurationNodeManger(bean.getConfigurations()));
                    
                    nodeMap.addNode(node.getName(), node);
                }
            } catch (IntrospectionException ex) {
                logger.error(ex, ex.fillInStackTrace());
            } catch (LaserException le) {
                logger.error(le, le.fillInStackTrace());
                AcWindowManager.notifyError("Internal error.\n Check log file");
            }
            
        }
        
        public java.util.Comparator getComparator() {
            return null;
        }
        
        
        /**
         * This method add configuraton for given user
         */
        public void addConfiguration(BeanSupport conf) throws IntrospectionException {
            GPNode node = confMap.getNode(loggedUserName);
            // node should be always not null
            NodeCollection coll = node.getNodeCollection();
            // collection should always be not null
            ConfigurationNodeManger confManager =
            (ConfigurationNodeManger)coll.getChildrenManager();
            
            
            if ( ((ConfigurationBean) conf).getIsDefault() ) {
                // adding new default configuration
                
                // find old default conf
                ConfigurationBean oldDefaultConf = findDefaultConfiguration();
                if ( oldDefaultConf != null )
                    oldDefaultConf.setIsDefault(false);
                
                confManager.addConfiguration(conf);
                
            } else {
                // adding not default configuration
                confManager.addConfiguration(conf);
            }
            
            
        }
        
        /**
         * @return default configuration of NULL if does default configuration
         *          does not exists
         */
        private ConfigurationBean findDefaultConfiguration() {
            GPNode node = confMap.getNode(loggedUserName);
            NodeCollection children = node.getNodeCollection();
            
            ConfigurationBean foundConf = null;
            for(Iterator iter = ((NodeList) children).iterator(); iter.hasNext(); ) {
                ConfigurationBean confBean =
                (ConfigurationBean) ((GPNode) iter.next()).getBean();
                
                if ( confBean.getIsDefault() ) {
                    foundConf = confBean;
                    break;
                }
                
            }
            
            return foundConf;
            
        }
        
        public List getConfigurationNames(String userName) {
            List configurationNames = new java.util.ArrayList();
            GPNode node = confMap.getNode(userName);
            NodeCollection coll = node.getNodeCollection();
            
            for(Iterator iter = ((NodeList) coll).iterator(); iter.hasNext(); ) {
                ConfigurationBean confBean =
                (ConfigurationBean) ((GPNode) iter.next()).getBean();
                
                configurationNames.add(confBean.getName());
            }
            return configurationNames;
        }
        
    }
    
    /**************************************************************************
     *   ConfigurationsNodeManager
     **************************************************************************/
    private class ConfigurationNodeManger implements ChildrenListManager {
        
        private final Logger logger =
        LogFactory.getLogger(ConfigurationNodeManger.class.getName());
        
        /** user list */
        private Collection confList = null;
        
        private NodeList nodeList = null;
        
        /**
         * @param confList <code>List</code> with <code>Configuration</code>
         *  Objects
         */
        public ConfigurationNodeManger(Collection confList) {
            this.confList = confList;
        }
        public void initChildrenList(NodeList nodeList) {
            this.nodeList = nodeList;
            logger.debug("  " + getClass().getName() + " initChildrenList()");
            try {
                Iterator iter = confList.iterator();
                while ( iter.hasNext() ) {
                    Configuration conf = (Configuration) iter.next();
                    
                    ConfigurationBean bean = new ConfigurationBean(conf);
                    logger.debug(bean.getName());
                    GPNode node = NodeFactory.createNode(bean);
                    
                    // do not display default configuration
                    if ( !conf.getName().equals(Constants.DEFAULT_CONFIGURATION_NAME)  &&
                    !conf.getName().equals(Constants.RECENTLY_APPLIED_CONFIGURATION) )
                        nodeList.addNode(node);
                }
            } catch (IntrospectionException ex) {
                logger.error(ex.getMessage(), ex);//.printStackTrace();
            } catch (LaserException le) {
                logger.error(le.getMessage(), le);
            }
            logger.debug("  " + getClass().getName() + " after initChildrenList()");
        }
        
        public java.util.Comparator getComparator() {
            return null;
        }
        
        public void addConfiguration(BeanSupport configuration)
        throws IntrospectionException {
            
            // remove existing conf in case when new conf has the same name
            // this is because of "SAVE" button
            for (Iterator iter = nodeList.iterator(); iter.hasNext(); ) {
                GPNode node = (GPNode) iter.next();
                if ( node.getName().equals(configuration.getName()) ) {
                    org.openide.nodes.Node nbNode = node.getPeerNode();
                    nodeList.removeNode(node);
                    try {
                        nbNode.destroy();
                    } catch (java.io.IOException ie) {
                        logger.error(ie.getMessage(), ie);
                    }
                    break;
                }
            }
            
            nodeList.addNode(NodeFactory.createNode(configuration));
        }
        
        public void removeConfiguration(GPNode node) {
            org.openide.nodes.Node nbNode = node.getPeerNode();
            nodeList.removeNode(node);
            try {
                nbNode.destroy();
            } catch (java.io.IOException ie) {
                logger.error(ie.getMessage(), ie);
            }
        }
    }
    
}
