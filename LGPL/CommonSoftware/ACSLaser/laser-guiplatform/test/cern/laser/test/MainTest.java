/*
 * MainTest.java
 *
 * Created on May 5, 2003, 3:11 PM
 */

package cern.laser.test;

//import cern.gp.actions.support.ActionUtils;

/**
 *
 * @author  pawlowsk
 */
public class MainTest {
    
//    public static Logger logger = LogFactory.getLogger(MainTest.class.getName());
//    
//    static final int NUMBER_OF_BEANS = 6;
//    /** Creates a new instance of MainTest */
//    public MainTest() {
//    }
//
//    // this is temporary solution, I do not know yet where I should put
//    // all this *.properties files
//    // maybe, when everthing will be fine, when this application will work as 
//    // NetBeans module    
//    static {
//        /*
//        // cmw-mom-config.properties 
//        System.setProperty("cmw.mom.config", "http://slwww/~pca/cmw/cmw.cfg");
//
//
//        // cmw-mom.properties")
//        System.setProperty("cmw.mom.retry", "40");
//        System.setProperty("cmw.mom.maxretry", "10");
//        System.setProperty("cmw.mom.ping", "10");
//        System.setProperty("cmw.mom.username", "laser_usr");
//        System.setProperty("cmw.mom.password", "laser_pwd");
//        System.setProperty("cmw.mom.brokerlist", "abjas1:2507,abjas2:2507");
//        System.setProperty("cmw.mom.loadbalancing", "false");
//        System.setProperty("cmw.mom.persistance", "false");
//        System.setProperty("cmw.mom.timetolive", "0");
//        System.setProperty("cmw.mom.priority", "4");
//        System.setProperty("cmw.mom.keepalive", "0");
//                 
//        */
//        // jndi.properties
//        System.setProperty("java.naming.factory.initial", "com.evermind.server.ApplicationClientInitialContextFactory");
//        //System.setProperty("java.naming.factory.initial", "com.evermind.server.rmi.RMIInitialContextFactory");
//        System.setProperty("java.naming.provider.url", "ormi://abjas1/laser-core");
//        //System.setProperty("java.naming.provider.url", "ormi://hpjvm/laser-core");
//        System.setProperty("java.naming.security.principal", "admin");
//        System.setProperty("java.naming.security.credentials", "password");
//        
//    }
//
//    /**
//     * @param args the command line arguments
//     */
//    public static void main(String[] args) throws Exception, Error {
//        
//        
//        testSystemProperties();
//        //testListExplorer();
//        //testActvieListExplorer();
//        //testAlarmDetailsPanel();
//        //System.getProperties();
//        
//        //try {
//        //    testCategoryExplorers();   // not finished
//        //} catch (LaserException le) {System.out.println(le.getRootCause());}
//        
//        //testUserExplorer();
//                
//    }
//   
//    /**
//     */
//    public static void testUserExplorer() throws LaserException {
//        UserHandler handler = UserHandlerFactory.getHandler();
//        // like login user
//        UserHandler userHandler = UserHandlerFactory.getHandler();
//        User user = userHandler.getUser("laser");
//        AppRegister.getInstance().registerUser(user);
//
//        String loggerUserName = "laser";
//
//        UserExplorer expl = new UserExplorer(handler.getUsers(), loggerUserName);
//        
//        JButton loadActionButton = new JButton(
//            ActionUtils.createActionForComp(expl,
//                SystemAction.get(cern.laser.guiplatform.actions.configuration.ConfigurationLoadAction.class)));
//        
//        JButton deleteActionButton = new JButton(
//            ActionUtils.createActionForComp(expl, 
//                SystemAction.get(cern.laser.guiplatform.actions.configuration.ConfigurationDeleteAction.class)));
//        
//        JButton deleteAllActionButton = new JButton(
//            ActionUtils.createActionForComp(expl, 
//                SystemAction.get(cern.laser.guiplatform.actions.configuration.ConfigurationSetAsDefaultAction.class)));
//        
//        
//        JPanel buttonPanel = new JPanel();
//        buttonPanel.setLayout(new java.awt.GridLayout(3, 0));
//        buttonPanel.add(loadActionButton);
//        buttonPanel.add(deleteActionButton);
//        buttonPanel.add(deleteAllActionButton);
//        
//        
//        expl.add(buttonPanel, BorderLayout.SOUTH);
//
//        expl.open(); 
//    } 
//    
//    
//    
//    /**
//     * This method test category explorers, both CategoryTree 
//     * and choosencategory
//     *
//     *  TODO not finished, becuase of GP problem and NodeActions problem
//     *  (context aware node actions)
//     *
//     *
//     */
//    public static void testCategoryExplorers() throws Exception {
//        TopComponent tc = new TopComponent();
//        tc.setLayout(new BorderLayout());
//        
//        
//        
//        CategoryTreeExplorer expl = 
//            new CategoryTreeExplorer(
//                CategoryBrowsingHandlerFactory.getHandler().getCategoryTreeRoot()
//            );
//        
//        JButton button = cern.gp.actions.support.ActionUtils.createJButton(CopyNodesActionAction.class);
//        //expl.add(button, BorderLayout.NORTH);
//        //expl.open();
//        
//        ChoosenCategoryExplorer expl1 = new ChoosenCategoryExplorer(new ArrayList());
//        //expl1.open();
//        
//        
//        tc.add(expl, BorderLayout.WEST);
//        tc.add(expl1, BorderLayout.EAST);
//        tc.setName("test top");
//        
//        tc.add(button, BorderLayout.SOUTH);
//        tc.open();
//        
//    }
//    
//    public static void testActvieListExplorer() throws Exception {
//        
//        logger.debug(" testActvieListExplorer start" );
//        
//        // like login user
//        UserHandler userHandler = UserHandlerFactory.getHandler();
//        User user = cern.laser.console.UserHandler.get().getUser("laser");
//        //User user = userHandler.getUser("laser");
//        AppRegister.getInstance().registerUser(user);
//    
//        logger.debug("user " + user.getName() + " is found");
//        try {
//            if ( user.getConfiguration(Constants.DEFAULT_CONFIGURATION_NAME) != null )
//                user.removeConfiguration(Constants.DEFAULT_CONFIGURATION_NAME);
//        } catch (LaserConfigurationNotFoundException lcnf) {
//            //logger.error(lcnf, lcnf.fillInStackTrace());
//            logger.error("user do not have default conf");
//        }
//        Configuration loadedConfiguration = null;
//        Configuration confTemp = null;
//        if ( (confTemp = user.getDefaultConfiguration()) == null ) {
//            logger.debug("user: " + user.getName() + " does not have default configuration");
//            loadedConfiguration = user.createConfiguration(
//                                            Constants.DEFAULT_CONFIGURATION_NAME);
//        } else {
//            loadedConfiguration = confTemp;
//        }
//        
//        AppRegister.getInstance().registerLoadedConfiguration(loadedConfiguration);
//       
//        // initialize active list inhibit list, mask list etc 
//        logger.debug("selection: ");
//        logger.debug(loadedConfiguration.getSelection()); 
//         
//                
//        
//        
//        
//        AlarmSelectionHandler jms_selectionHandler = 
//            AlarmSelectionHandlerFactory.getHandler();
//        try {
//           
//            logger.debug("selecting ........... ");
//
//            ProxyBuffer.getDefault().registerAlarmSelectionListener(AlarmContainer.getDefault());
//
//            java.util.Map activeAlarms = 
//                jms_selectionHandler.select(loadedConfiguration.getSelection(), 
//                                            ProxyBuffer.getDefault());
//            logger.debug("selected " + activeAlarms.size() + " alarms "); 
//            logger.debug("active alarms: ");
//            for (Iterator iter = activeAlarms.values().iterator(); iter.hasNext(); ) {
//                logger.debug(((Alarm) iter.next()).getTriplet().toString());
//            }
//
//            //for (Iterator iter = activeAlarms.iterator(); iter.hasNext(); ) {
//            //    logger.debug(((Alarm) iter.next()).getAlarmId());
//            //}
//
//            AlarmContainer.getDefault().initContainer(activeAlarms,
//                    loadedConfiguration);
//            
//            ProxyBuffer.getDefault().enable();
//
//            AlarmNodeManager activeListManager =
//                AlarmContainer.getDefault().getAlarmNodeManager(
//                                                Constants.ACTIVE_LISTENER_KEY);
//
//
//
//            // active list            
//            ActiveListExplorerPanel expl = new ActiveListExplorerPanel(
//                                    activeListManager, 
//                                    NbBundle.getMessage(ActiveListExplorerPanel.class, 
//                                       "LBL_Active_list_component_name"), Constants.getPropertyNames());
//            
//            
//            AlarmStatisticInfoPanel statisticPanel = new AlarmStatisticInfoPanel("", null, false);
//            AlarmContainer.getDefault().attach("this_key_has_to_be_changed", statisticPanel);
//            expl.add(statisticPanel, java.awt.BorderLayout.SOUTH);
//            
//            //WindowUtils.openInMode(expl, expl.getName());
//            Workspace wksp = WindowManager.getDefault().findWorkspace("test workspace");
//            
//            
//            WindowUtils.openInMode(wksp, expl, "Active mode", WindowUtils.DESKTOP_FRAME);
//          
//            //expl.open(wksp);
//            //expl.requestFocus();
//            
//            Thread.sleep(10000);
//            
//            // inhibit list 
//            AlarmNodeManager inhibitListManager =
//                AlarmContainer.getDefault().getAlarmNodeManager(Constants.INHIBIT_LISTENER_KEY);
//
//
//            ActiveListExplorerPanel inhibitExpl = new ActiveListExplorerPanel(
//                                                inhibitListManager, 
//                                                NbBundle.getMessage(ActiveListExplorerPanel.class, 
//                                   "LBL_Inhibit_list_component_name"), Constants.getPropertyNames()
//                                                );
//            
//            WindowUtils.openInMode(wksp, inhibitExpl, "Inhibit mode", WindowUtils.DESKTOP_FRAME);
//           
//            //inhibitExpl.open(wksp);
//            
//            // masked list
//
//            AlarmNodeManager maskedListManager = 
//                AlarmContainer.getDefault().getAlarmNodeManager(
//                                            Constants.MASKED_LISTENER_KEY);
//            ActiveListExplorerPanel maskedExpl = new ActiveListExplorerPanel(
//                                                maskedListManager, 
//                                            NbBundle.getMessage(ActiveListExplorerPanel.class,
//                                          "LBL_Mask_list_component_name"), Constants.getPropertyNames()
//                                                );
//            
//            WindowUtils.openInMode(wksp, maskedExpl, "Masked mode", WindowUtils.DESKTOP_FRAME);
//           
//            //masked.open();
//
//            AlarmNodeManager highlightedAndKlaxonListManager = 
//                AlarmContainer.getDefault().getAlarmNodeManager(
//                                            Constants.HIGHLITED_LISTENER_KEY);
//            ActiveListExplorerPanel highlightedExpl = new ActiveListExplorerPanel(
//                                                highlightedAndKlaxonListManager, 
//                                           NbBundle.getMessage(ActiveListExplorerPanel.class,
//                                          "LBL_HighligtedAndKlaxon_list_component_name")
//                                          , Constants.getPropertyNames()
//                                                );
//            WindowUtils.openInMode(wksp, highlightedExpl, "Higlighted and klaxoned mode", WindowUtils.DESKTOP_FRAME);
//            //highlighted.open();
//     
//            
//            } catch (cern.laser.console.LaserConsoleException lce) {
//            logger.error(lce, lce.fillInStackTrace());
//            logger.error(lce.getRootCause().toString());
//            
//            NotifyDescriptor selectErrorDescriptor =
//                new NotifyDescriptor.Message(
//                    "Alarm container can't be initialized!! See log file.", 
//                    NotifyDescriptor.ERROR_MESSAGE);
//            DialogDisplayer.getDefault().notify(selectErrorDescriptor);
//
//        } catch (cern.laser.client.LaserException le) {
//            logger.error(le, le.fillInStackTrace());
//            logger.error(le.getRootCause().toString());
//            
//            NotifyDescriptor selectErrorDescriptor =
//                new NotifyDescriptor.Message(
//                    NbBundle.getMessage(AlarmConsoleLoginWindow.class, 
//                                    "LASER_EXCEPTION_SELECT_message"), 
//                    NotifyDescriptor.ERROR_MESSAGE);
//            DialogDisplayer.getDefault().notify(selectErrorDescriptor);
//            
//        } 
//       
//    }
//    
//    
//    public static void testAlarmDetailsPanel() {
//        String _FF = "HADRON_BEAN_CHANNEL";
//        String _FM = "DISABLED_BEAN_STOP";
//        int _FC = 83, alarmCounter = 1, priority = 3;
//        boolean active = true;
//        
//        AlarmImpl alarm = new AlarmImpl(alarmCounter, active, _FF, _FM, _FC);
//        alarm.setPriority(priority);
//        CommentedAlarm commentedAlarm = new CommentedAlarm(alarm, null);
//        AlarmBean alarmBean = new AlarmBean(commentedAlarm);
//        AlarmDetailsPanel detailsWin = new AlarmDetailsPanel(alarmBean);
//        detailsWin.open();
//
//    }
//    
//    /** this class test SimpleDemoBean with list explorer */
//    public static void testListExplorer() throws Exception {
//         // create the beans you want to display
//        SimpleDemoBean[] beans = new SimpleDemoBean[NUMBER_OF_BEANS];
//        for (int ix=0; ix<NUMBER_OF_BEANS; ix++) {
//          beans[ix] = new SimpleDemoBean("bean_" + (NUMBER_OF_BEANS-ix-1));
//        }
//
//        // create nodes associated with the beans
//        GPNode[] nodes = NodeFactory.createNode(beans);
//
//        // create the explorer and set the columns it shall display
//        ListTableExplorer expl = new ListTableExplorer(nodes);
//        expl.setTableColumns(new String[] { "name", "value" });
//
//        //  open the explorer in a NetBeans "Mode"
//        WindowUtils.openInMode(expl, "TestListTableExplorer");
//        expl.requestFocus();
//        
//        System.out.println("activatedNodesLength " + expl.getActivatedNodes().length);
//        System.out.println("selectedNodesLength " + 
//            expl.getExplorerManager().getSelectedNodes().length);
//        
//        
//    }
//    
//    public static void testSystemProperties() {
//        
//      for (java.util.Iterator iter = System.getProperties().keySet().iterator();
//            iter.hasNext(); ) 
//      {
//          String key = (String) iter.next();
//          System.out.println(key + " = " + System.getProperty(key));
//      }
// 
//      /*
//      System.out.println("cmw-mom-config.properties: " + System.getProperty("cmw-mom-config.properties"));
//      System.out.println("cmw-mom.properties: " + System.getProperty("cmw-mom.properties"));
//      
//      try {
//        java.io.FileReader file = new java.io.FileReader(System.getProperty("cmw-mom.properties"));
//        file.close();
//      } catch (Exception e) {
//          e.printStackTrace();
//      }
//      
//      */
//      
//    }
}
