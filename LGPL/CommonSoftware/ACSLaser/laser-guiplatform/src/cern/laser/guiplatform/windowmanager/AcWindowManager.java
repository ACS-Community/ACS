/*
 * AcWindowManager.java
 *
 * Created on May 8, 2003, 3:38 PM
 */

package cern.laser.guiplatform.windowmanager;

import java.awt.Rectangle;
import java.net.URL;
import java.util.Iterator;

import org.apache.log4j.Logger;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.explorer.ExplorerPanel;
import org.openide.nodes.Node;
import org.openide.util.NbBundle;
import org.openide.windows.Mode;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import org.openide.windows.Workspace;

import cern.gp.windows.WindowUtils;
import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.explorer.ACExplorer;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windows.ActiveListExplorerPanel;
import cern.laser.guiplatform.windows.WindowConstants;
import cern.laser.guiplatform.windows.alarms.AlarmDetailsPanel;
import cern.laser.guiplatform.windows.alarms.AlarmTimestampsPanel;

/**
 * This is Alarm Console window manager.
 * This manager helps with managing windows within Alarm Console.
 *
 * @author  pawlowsk
 */
public class AcWindowManager {
    
    /** logger */
    private static Logger logger =
    LogFactory.getLogger(AcWindowManager.class.getName());
    
    /** default workspace name
     * all operation will be done in this workspace
     */
    private static String defaultWorkSpaceName = Constants.ALARM_WORKSPACE_NAME;
    
    private static Workspace defautlWorkspace = findWorkspace();
    //    private static Workspace defautlWorkspace = findWorkspace();
    
    private static final Node [] EMPTY_NODE_ARRAY = new Node[0];
    
    
    /** Creates a new instance of AcWindowManager
     * This shold be singleton
     */
    private AcWindowManager() {
    }
    
    /**
     * This method opens TopComponents in previosly (XML based) defined mode
     * TopComponent will be opended in default workspace
     *
     * @param modeName mode name
     * @param top TopComponent
     */
    public static void openInMode(String modeName, TopComponent component) {
        logger.debug("component bounds: " + component.getName() + " "  + component.getBounds().toString());
        Workspace workspace = findWorkspace();
        Mode mode = workspace.findMode(modeName);
        if (mode == null) {
            logger.debug("Mode " + modeName + " not found, crated new one");
            mode = workspace.createMode(modeName, modeName, null);
        }
        logger.debug("mode bounds: " + mode.getName() + " "  + mode.getBounds().toString());
        // set topcomponent bounds the same as mode bounds
        //component.setBounds(mode.getBounds());
        mode.dockInto(component);
        component.open(workspace);
        component.requestFocus();
    }
    
    /**
     * This method opens given TopComponent in default workspace
     *
     * @param componentName component Name
     */
    public static void openComponent(String componentName) {
        TopComponent top = findTopComponent(findWorkspace(),
        componentName);
        logger.debug("openComponent: TopComponent " + componentName + " was found : " +
        (top != null ? "YES" : "NO"));
        if ( top != null) top.open();
    }
    
    /**
     * This method closes TopComponent in default workspace
     *
     * @param componentName component name
     */
    public static void closeComponent(String componentName) {
        TopComponent top = findTopComponent(findWorkspace(),
        componentName);
        logger.debug("closeComponent: TopComponent " + componentName + " was found : " +
        (top != null ? "YES" : "NO"));
        if ( top != null) top.close();
    }
    
    /**
     * This method closes all TopComponents
     *
     * @param componentsNames TopComponents names
     * @depracated not implemented yet, and probably not used
     */
    public static void closeComponents(String [] componentsNames) {
        Workspace spc = findWorkspace();
        for (int i = 0; i < componentsNames.length; i++) {
            //TopComponent top = WindowUtils.findTopComponent(findWorkspace(), "ALARM_ACTIVE_LIST");
            TopComponent top = findTopComponent(spc, componentsNames[i]);
            if ( top != null ) {
                top.setCloseOperation(TopComponent.CLOSE_EACH);
                //if ( top.canClose(
                top.close();
            } else {
                logger.debug("Top Component " + componentsNames[i] + " not found ");
            }
        }
    }
    
    /**
     * This method closes all apropriate TopComponents after user logout
     * All components should be closed
     */
    public static void closeAllTopComponents() {
        Workspace spc = findWorkspace();
        java.util.Set set = spc.getModes();
        java.util.Iterator iter = set.iterator();
        
        while (iter.hasNext()) {
            Mode mode = (Mode) iter.next();
            logger.debug("mode name --> " + mode.getName());
            TopComponent [] tops = mode.getTopComponents();
            for (int i = 0; i < tops.length; i++) {
                if ( tops[i] instanceof ActiveListExplorerPanel ) {
                    ((ActiveListExplorerPanel) tops[i]).setCanBeClose(true);
                    tops[i].close(spc);
                } else {
                    tops[i].close(spc);
                }
            }
        }
    }
    /** This method checks if topComponent is opened in default workspace
     *
     * @param componentName component Name
     */
    public static boolean isOpened(String componentName) {
        
        TopComponent top = findTopComponent(findWorkspace(),
        componentName);
        if ( top != null )
            return top.isOpened();
        
        return false;
    }
    
    /**
     * @return TopComponent if can be found
     *          null otherwise
     */
    public static TopComponent findTopComponent(String componentName) {
        return findTopComponent(findWorkspace(), componentName);
    }
    
    /**
     * This class opens topComponent inside default workspace
     *
     * @param comp topComponent which should be opened
     * @param modeName mode name
     * @param index index (i.e. CENTER, RIGHT, LEFT)
     * @param workspaceUsage ALL - (mode, topComponent) should fill up
     *                              the whole workspace
     *                       COMPONENT_SIZE - create mode with comp size
     * @param modeIcon mode icon
     *
     */
    public static void openTopComponent(TopComponent comp, String modeName,
    int index, int workspaceUsage,
    URL modeIcon) {
        //throw new java.lang.UnsupportedOperationException(" this method is not implemented yet !!!\n" +
        //    "Try use other methods from cern.laser.guiplatform.windowmanager.AcWindowManager class.");
        
        Workspace alarmWsp = findWorkspace();
        
        int modeX = -1, modeY = -1;
        if ( index == WindowConstants.CENTER &&
        workspaceUsage == WindowConstants.COMPONENT_SIZE) {
            modeX = alarmWsp.getBounds().x +
            (alarmWsp.getBounds().width/2 - comp.getPreferredSize().width/2);
            modeY = alarmWsp.getBounds().y +
            (alarmWsp.getBounds().height/2 - comp.getPreferredSize().height/2);
        }
        Mode mode = alarmWsp.findMode(modeName);
        if ( mode == null ) {
            logger.debug(modeName + "mode not found ");
            mode = alarmWsp.createMode(modeName, modeName, modeIcon);
        }
        
        //alarmWsp.createMode("LOGIN_mode_name", "LOGIN_mode_display_name", null);
        
        if ( workspaceUsage == WindowConstants.COMPONENT_SIZE )
            mode.setBounds(
            new Rectangle(modeX, modeY,
            comp.getPreferredSize().width,
            comp.getPreferredSize().height)
            //Constants.LOGIN_WINDOW_WIDTH,
            //Constants.LOGIN_WINDOW_HEIGHT)
            );
        else if ( workspaceUsage == WindowConstants.ALL )
            mode.setBounds(
            new Rectangle(alarmWsp.getBounds().x, alarmWsp.getBounds().y,
            alarmWsp.getBounds().width,
            alarmWsp.getBounds().height)
            );
        
        openInMode(mode.getName(), comp);
        // !!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!
        // frame type is probably wrong and it probably does not work
        // but I do not now how can I fix it.
        // this should be done by GP
        //WindowUtils.openInMode(alarmWsp, comp, mode.getName(), WindowUtils.DESKTOP_FRAME);
        //WindowUtils.openInMode(alarmWsp, comp, mode.getName(), WindowUtils.TOP_FRAME);
    }
    
    
    /**
     * This class opens topComponent inside default workspace
     *
     * @param comp topComponent which should be opened
     * @param modeName mode name
     * @param index index (i.e. CENTER, RIGHT, LEFT)
     * @param workspaceUsage ALL - (mode, topComponent) should fill up
     *                              the whole workspace
     *                       COMPONENT_SIZE - create mode with comp size
     * @param modeIcon mode icon
     *
     */
    public static void openTopComponent(TopComponent comp, String modeName,
    int index, int workspaceUsage) {
        openTopComponent(comp, modeName, index, workspaceUsage, null);
    }
    
    /**
     * This methods opens topComponent inside previously defined mode
     */
    public static void openTopComponent(TopComponent comp, String modeName) {
        // !!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!
        // frame type is probably wrong and it probably does not work
        // but I do not now how can I fix it.
        // this should be done by GP
        
        //WindowUtils.openInMode(defautlWorkspace, comp, modeName);
        WindowUtils.openInMode(comp, modeName);
        //WindowUtils.openInMode(defautlWorkspace, comp, modeName,WindowUtils.TOP_FRAME);
    }
    
    /**
     * This method activates default workspace
     */
    public static void activateDefaultWorkspace() {
        findWorkspace().activate();
    }
    
    /**
     * This method finds default workspace
     * @throws RuntimeException if workspace can not be found
     */
    public static Workspace findWorkspace() {
        Workspace alarmWsp =
        WindowManager.getDefault().findWorkspace(defaultWorkSpaceName);
        
        /** uncomment this is final version
         * if ( alarmWsp == null )
         * throw new RuntimeException(
         * Constants.ALARM_WORKSPACE_NAME + " not found\n" +
         * " Probably alarm console module is not properly installed");
         */
        return alarmWsp;
    }
    
    /** This method sets the columns of the ListTableExplorer
     * @param componentName component name
     * @param newColumns new columns property names
     * @throws ClassCastException if component with given component name is
     *          not ListTableExplorer
     */
    public static void setTableColumns(String componentName, String [] newColumns) {
        TopComponent activeList = findTopComponent(WindowManager.getDefault().getCurrentWorkspace(), componentName);
        
        if ( activeList == null ) 
            logger.debug(componentName + " is null");
        else 
            logger.debug(componentName + " is not null");
        
        if ( activeList != null ) {
            logger.debug("newColumns.length " + newColumns.length);
            logger.debug("activeList is " + activeList.getClass().getName() );
            ((ACExplorer) activeList).setTableColumns(newColumns);
        } else {
            logger.debug(componentName + " top component is null");
        }
    }
    
    /**
     * This method displays "Detail" window for given alarm
     * @param alarm
     */
    public static void showDetails(AlarmBean alarm) {
        
        // check if Details TopComponent is opened
        String componentName = NbBundle.getMessage(AlarmDetailsPanel.class,
        "LBL_ALARM_DETAILS_PANEL_component_name");
        TopComponent detailsWin = findTopComponent(defautlWorkspace,
        componentName);
        if ( detailsWin != null ) {      // component is opened
            ((AlarmDetailsPanel) detailsWin).updatePanel(alarm);
            logger.debug(componentName + " found");
        } else {
            detailsWin = new AlarmDetailsPanel(alarm);
            logger.debug(componentName + " found");
        }
        
        //openInMode(Constants.ALARM_DETAILS_MODE_NAME, detailsWin);
        detailsWin.open();
    }
    
    public static void showTimestamps(AlarmBean ab) {
        
        String fsName = ab.getFaultFamily() + " " + ab.getFaultMember() +
        " " + ab.getFaultCode();
        
        // check if Details TopComponent is opened
        String componentName = NbBundle.getMessage(
        AlarmTimestampsPanel.class,
        "LBL_ALARM_TIMESTAMP_PANEL_component_name");
        
        logger.debug("defautlWorkspace=" + defautlWorkspace + " componentName=" + componentName);
        Iterator modesIterator = defautlWorkspace.getModes().iterator();
        while (modesIterator.hasNext()) {
            Mode mode = (Mode) modesIterator.next();
            logger.debug("mode=" + mode);
            TopComponent[] tcs = mode.getTopComponents();
            for (int i = 0; i < tcs.length; i++) {
                TopComponent element = tcs[i];
                logger.debug("element=" + element);
                //if (element.getName().equals(componentName)) {
                //    timeWin=element;
                //}
            }
        }
        
        
        TopComponent timeWin = findTopComponent(defautlWorkspace, componentName);
        if ( timeWin != null ) {      // component is opened
            ((AlarmTimestampsPanel) timeWin).updatePanel(ab.getTimestamps(), fsName);
            // TODO: focus on window
            logger.debug("timestamps window found");
        } else {
            logger.debug("new timestamps window must be opened");
            new AlarmTimestampsPanel(ab.getTimestamps(), fsName).open();
            //timeWin = new AlarmTimestampsPanel(ab.getTimestamps(), fsName).open();
        }
        
        //openInMode(Constants.ALARM_DETAILS_MODE_NAME, detailsWin);
    }
    
    // -----------------------------------------------------------------------
    //
    // this is set of helper method
    // in final version these method should be deleted
    //
    // -----------------------------------------------------------------------
    
    /** this method prints all mode name in default workspace
     */
    public static void printAllModeNames() {
        
        Workspace workSpc = findWorkspace();
        
        java.util.Set set = workSpc.getModes();
        java.util.Iterator iter = set.iterator();
        
        while (iter.hasNext()) {
            Mode mode = (Mode) iter.next();
            //mode.
            logger.debug("mode name --> " + mode.getName());
        }
    }
    
    /** This method create mode inside default workspace */
    public static void createMode(String modeName) {
        Workspace spc = findWorkspace();
        spc.createMode(modeName, modeName, null);
    }
    
    
    /** utility method
     * @param errorMsg error message
     */
    public static void notifyError(String errorMsg) {
        NotifyDescriptor.Message desc = new NotifyDescriptor.Message(
        errorMsg, NotifyDescriptor.ERROR_MESSAGE);
        DialogDisplayer.getDefault().notify(desc);
        
    }
    
    public static void notifyWarning(String warningMsg) {
        NotifyDescriptor.Message desc = new NotifyDescriptor.Message(
        warningMsg, NotifyDescriptor.WARNING_MESSAGE);
        DialogDisplayer.getDefault().notify(desc);
    }
    
    public static void setStatusText(String text) {
        org.openide.awt.StatusDisplayer.getDefault().setStatusText(text);
    }
    
    /**
     * this method sets 0 as selected nodes for given explorer
     * Used in actions in order not to have context menu in empty explorer.
     *
     * @param topComponentName name used to find explorer (TopComponent name)
     */
    public static void setZeroSelectedNodes(String topComponentName) {
        
        ExplorerPanel expl = (ExplorerPanel)
        findTopComponent(topComponentName);
        if ( expl != null )  {
            //logger.debug("inhibit list panel found");
            try {
                expl.getExplorerManager().setSelectedNodes(EMPTY_NODE_ARRAY);
            } catch (java.beans.PropertyVetoException pve) {
                logger.error(pve, pve.fillInStackTrace());
            }
        }
        
    }
    
    
    /** This method prints all components (modes, topcomponents) from default
     * workspace
     */
    public static void printAllComponents() {
        Workspace workSpc = findWorkspace();
        
        java.util.Set set = workSpc.getModes();
        java.util.Iterator iter = set.iterator();
        
        while (iter.hasNext()) {
            Mode mode = (Mode) iter.next();
            
            TopComponent [] tops = mode.getTopComponents();
            logger.debug("mode name --> " + mode.getName() +
            " x = " + mode.getBounds().x +
            " y = " + mode.getBounds().y +
            " width = " + mode.getBounds().width +
            " height = " + mode.getBounds().height);
            for (int i = 0; i < tops.length; i++)
                logger.debug("    top comp name ----> " + tops[i].getName() +
                (tops[i].getCloseOperation() == TopComponent.CLOSE_EACH ? " CLOSE_EACH" : " CLOSE_LAST"));
        }
    }
  /**
   * Utility method: Returns the TopComponent with the given programmatic name in the given workspace,
   * or <code>null</code> if there is no such TopComponent.
   * @param workspace the workspace in which to look for the TopComponent
   * @param componentName the programmatic name of the TopComponent
   * @return Returns the TopComponent with the given programmatic name,
   * or <code>null</code> if there is no such TopComponent
   * Method taken from gp.windows.WindowUtil, bug with empty element name fixed
   */
    
    public static TopComponent findTopComponent(Workspace workspace, String componentName) {
        Iterator modesIterator = workspace.getModes().iterator();
        while (modesIterator.hasNext()) {
            Mode mode = (Mode) modesIterator.next();
            TopComponent[] tcs = mode.getTopComponents();
            for (int i = 0; i < tcs.length; i++) {
                TopComponent element = tcs[i];
                if ( element.getName()!=null && element.getName().equals(componentName)) {
                    return element;
                }
            }
        }        
        return null;
    }
    
}
