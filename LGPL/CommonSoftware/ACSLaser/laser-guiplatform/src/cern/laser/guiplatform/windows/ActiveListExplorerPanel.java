package cern.laser.guiplatform.windows;

/**
 *
 * @author  woloszyn
 */
import java.awt.Dimension;

import org.apache.log4j.Logger;
import org.openide.windows.WindowManager;
import org.openide.windows.Workspace;

import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.alarms.AlarmNodeManager;
import cern.laser.guiplatform.explorer.ACExplorer;
import cern.laser.guiplatform.util.LogFactory;

// As needed:
/*
import java.io.*;
import java.net.*;
import org.openide.ErrorManager;
import org.openide.TopManager;
import org.openide.actions.*;
import org.openide.awt.UndoRedo;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.Utilities;
import org.openide.util.actions.*;
import org.openide.util.datatransfer.PasteType;
import org.openide.util.io.*;
 */

/** An openable window available to the IDE's window manager.
 * This class is used to visualize active, inhibited, highlighted, etc lists.
 *
 * @author pawlowsk
 */
//public class ActiveListExplorerPanel extends TopComponent /* or CloneableTopComponent */ {
public class ActiveListExplorerPanel extends ACExplorer {    
    // REMEMBER: You should have a public default constructor!
    // This is for externalization. If you have a nondefault
    // constructor for normal creation of the component, leave
    // in a default constructor that will put the component into
    // a consistent but unconfigured state, and make sure readExternal
    // initializes it properly. Or, be creative with writeReplace().
    /* Constructor
     *
     * @param nodeManager node Manager
     * @param componentName component name
     * @param canBeClosed indicates whether this component can be closed by user
     *          default true
     * @param serialize indicates if this component should be serialized by
     *      Netbeans, default true
     *
     * @param columnToDisplay which columns should be dispaley (ActvieListExplorer)
     */
    public ActiveListExplorerPanel(AlarmNodeManager nodeManager,
    String componentName, boolean canBeClosed,
    boolean serialize, String [] columnsToDisplay) {
        super( nodeManager, columnsToDisplay );
        initComponents();
        setCloseOperation(CLOSE_LAST); // or CLOSE_EACH
        this.componentName = componentName;
        // Display name of this window (not needed if you use the DataObject constructor):
        setName(componentName);
        
        this.canBeClosed = canBeClosed;
        
        if ( !serialize )
            // do not serialize
            putClientProperty("PersistenceType", "Never");
        
        putClientProperty("TabPolicy", "HideWhenAlone");
        
        
        
        // test with selection listener, make "New Alarm" "Not New"
        this.getExplorerManager().addPropertyChangeListener( new ListSelectionListener());
        
        // You may set the icon, but often it is better to set the icon for an associated mode instead:
        // setIcon(Utilities.loadImage("cern/laser/guiplatform/windows/ActiveListExplorerPanelIcon.gif"));
        // Use the Component Inspector to set tool-tip text. This will be saved
        // automatically. Other JComponent properties you may need to save yuorself.
        // At any time you can affect the node selection:
        // setActivatedNodes(new Node[] {...});
    }
    
    /** Constructor */
    public ActiveListExplorerPanel(AlarmNodeManager nodeManager,
    String componentName, String [] columnsToDisplay) {
        
        this(nodeManager, componentName, true, true, columnsToDisplay);
    }
    
    /** Constructor */
    public ActiveListExplorerPanel(AlarmNodeManager nodeManager,
    String componentName, boolean canBeClosed,
    String [] columnsToDisplay) {
        this(nodeManager, componentName, canBeClosed, true, columnsToDisplay);
    }
    
    /*
    public HelpCtx getHelpCtx() {
        return new HelpCtx(ActiveListExplorerPanel.class);
    }
     */
    
    /*
    // If you are using CloneableTopComponent, probably you should override:
    protected CloneableTopComponent createClonedObject() {
        return new ActiveListExplorerPanel();
    }
    protected boolean closeLast() {
        // You might want to prompt the user first and maybe return false:
        return true;
    }
     */
    /** This method indicates whether this component can be closed
     * @return true is can be close, otherwise false
     */
    public boolean canClose(Workspace workspace, boolean flag) {
        return canBeClosed;
    }
    
    /** This is helpers method, which is used to indicates whether this component
     * can be closed or not
     * @param flag true if component can be closed
     *              flase if can not be closed
     */
    public void setCanBeClose(boolean flag) {
        canBeClosed = flag;
    }
    
    public String getName() {
        return componentName;
    }
    
    /**
     * @param
     */
    private boolean [] getColumnsToSort( String [] columnsToDisplay ) {
        boolean [] columnsToSort = new boolean[columnsToDisplay.length];
        for( int i=0; i<columnsToSort.length; i++ ){
            columnsToSort[i] = false;
        }
        return columnsToSort;
    }
    
    // APPEARANCE
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the FormEditor.
     */
    private void initComponents() {
        
    }
    
    
    // Variables declaration - do not modify
    // End of variables declaration
    
    /** logger */
    private static Logger logger =
    LogFactory.getLogger(ActiveListExplorerPanel.class.getName());
    
    /** this flag indicates that this component can be closed only programmatically */
    private boolean canBeClosed = true;//false;
    
    /** component name */
    private String componentName = null;
    
    /** default dimension, used by InhibitList, etc */
    private Dimension defaultDimension = new Dimension(
    (int) (WindowManager.getDefault().getCurrentWorkspace().getBounds().width /1.5) ,
    WindowManager.getDefault().getCurrentWorkspace().getBounds().height / 3);
    
    
    
    
    // MODES AND WORKSPACES
    
    /*
    // If you want it to open in a specific mode:
    public static final String ActiveListExplorerPanel_MODE = "ActiveListExplorerPanel";
    public void open(Workspace ws) {
        super.open(ws);
        if (ws == null) ws = TopManager.getDefault().getWindowManager().getCurrentWorkspace();
        Mode m = ws.findMode(ActiveListExplorerPanel_MODE);
        if (m == null) {
            try {
                m = ws.createMode(ActiveListExplorerPanel_MODE, // code name
                                   NbBundle.getMessage(ActiveListExplorerPanel.class, "LBL_mode_name"), // display name
                                   new URL("nbresloc:/cern/laser/guiplatform/windows/ActiveListExplorerPanelIcon.gif"));
            } catch (MalformedURLException mfue) {
                TopManager.getDefault().notifyException(mfue);
                return;
            }
            // If you want it in a specific position:
            // m.setBounds(...ws.getBounds()...);
        }
        m.dockInto(this);
    }
     */
    
    /**/
    // If you are not specifying a mode you may wish to use:
    public Dimension getPreferredSize() {
        return defaultDimension;
    }
     /*
      *
      */
    
    /*
    // If you want it to open on a specific workspace:
    public static final String ActiveListExplorerPanel_WORKSPACE = NbBundle.getMessage(ActiveListExplorerPanel.class, "LBL_workspace_name");
    public void open() {
        WindowManager wm = TopManager.getDefault().getWindowManager();
        Workspace ws = wm.findWorkspace(ActiveListExplorerPanel_WORKSPACE);
        if (ws == null)
            ws = wm.createWorkspace(ActiveListExplorerPanel_WORKSPACE);
        open(ws);
        ws.activate();
    }
     */
    
    
    /** This method cleans AlarmContainer, in final version is should be moved
     * to logout action and to windowListener
     */
    /*
    protected void componentClosed() {
     
        try {
            AlarmSelectionHandler jms_selectionHandler =
            AlarmSelectionHandlerFactory.getHandler();
     
            jms_selectionHandler.resetSelection();
        } catch (LaserException le) {
            logger.error(le, le.fillInStackTrace());
            logger.error(le.getRootCause(), le.getRootCause().fillInStackTrace());
        }
     
        cern.laser.guiplatform.util.ProxyBuffer.getDefault().disable();
        cern.laser.guiplatform.util.ProxyBuffer.getDefault().close();
     
        AlarmContainer.getDefault().clearContainer();
        logger.debug("AlarmContainer was cleared!!");
    }
     */
    // PERSISTENCE
    
    // private static final long serialVersionUID = ...;
    
    /*
    // If you wish to keep any state between IDE restarts, put it here:
    public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
        super.readExternal(in);
        setSomeState((SomeType)in.readObject());
    }
    public void writeExternal(ObjectOutput out) throws IOException {
        super.writeExternal(out);
        out.writeObject(getSomeState());
    }
     */
    
    /*
    // The above assumes that the SomeType is safely serializable, e.g. String or Date.
    // If it is some class of your own that might change incompatibly, use e.g.:
    public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
        super.readExternal(in);
        NbMarshalledObject read = (NbMarshalledObject)in.readObject();
        if (read != null) {
            try {
                setSomeState((SomeType)read.get());
            } catch (Exception e) {
                ErrorManager.getDefault().notify(e);
                // If the problem would make this component inconsistent, use:
                // throw new SafeException(e);
            }
        }
    }
    public void writeExternal(ObjectOutput out) throws IOException {
        super.writeExternal(out);
        Object toWrite;
        try {
            toWrite = new NbMarshalledObject(getSomeState());
        } catch (Exception e) {
            ErrorManager.getDefault().notify(e);
            toWrite = null;
            // Again you may prefer to use:
            // throw new SafeException(e);
        }
        out.writeObject(toWrite);
    }
     */
    
    /*
    // Use this to discard the component after restarts (make it nonpersistent):
    private Object readResolve() throws ObjectStreamException {
        return null;
        // If you wish to conditionally discard it, make readExternal set
        // or clear some flag acc. to the condition, then use:
        // return discardFlag ? null : this;
        // Singleton component using private static ActiveListExplorerPanel theInstance:
        // if (theInstance == null) theInstance = this;
        // return theInstance;
    }
     */
    
    // ACTIONS
    
    /*
    // If you wish to have extra actions appear in the window's
    // popup menu, they can go here:
    public SystemAction[] getSystemActions() {
        SystemAction[] supe = super.getSystemActions();
        SystemAction[] mine = new SystemAction[supe.length + 1];
        System.arraycopy(supe, 0, mine, 0, supe.length);
        mine[supe.length] = SystemAction.get(SomeActionOfMine.class);
        return mine;
    }
     */
    
    /*
    // Associate implementations with copying, searching, etc.:
    protected void componentActivated() {
        ((CallbackSystemAction)SystemAction.get(FindAction.class)).setActionPerformer(new ActionPerformer() {
                public void performAction(SystemAction action) {
                    // search this component somehow
                }
            });
        // similar for CopyAction, CutAction, DeleteAction, GotoAction, ReplaceAction, etc.
        // for PasteAction, use:
        // ((PasteAction)SystemAction.get(PasteAction.class)).setPasteTypes(new PasteType[] {...});
    }
    protected void componentDeactivated() {
        // FindAction will be turned off by itself
        // ((PasteAction)SystemAction.get(PasteAction.class)).setPasteTypes(null);
    }
     */
    
    /*
    // If you want UndoAction and RedoAction to be enabled on this component:
    public UndoRedo getUndoRedo() {
        return new MyUndoRedo(this);
    }
     */
    
    // Printing, saving, compiling, etc.: use cookies on some appropriate node and
    // use this node as the node selection.
    
    
    /** Method for test execution */
    //public static void main(String args[]) throws Exception {
    // XXX: this method contains errors, pleas do not use it
    // use testActiveListExplorer from MainTest class
    
    // user login
    // register global confugiration
    // intialize alarm container
    // open active list
    
    // alarm lists test
    //}
    
    //
    // -- inner class ListSelectionListener -------------------
    //
    /**
     */
    private class ListSelectionListener implements java.beans.PropertyChangeListener {
        
        /*
        private TopComponent top = null;
         
        public ListSelectionListener(TopComponent top) {
            this.top = top;
        }
         */
        // logger
        private Logger logger =
        LogFactory.getLogger(ListSelectionListener.class.getName());
        
        public void propertyChange(java.beans.PropertyChangeEvent propertyChangeEvent) {
            /*
            logger.debug("progertyChagne(propertyChangeEvent) propName " +
                    propertyChangeEvent.getPropertyName() + " " +
                    org.openide.explorer.ExplorerManager.PROP_SELECTED_NODES
            );
             */
            org.openide.nodes.Node node [] =
            ActiveListExplorerPanel.this.getExplorerManager().getSelectedNodes();
            //org.openide.nodes.node node [] = this.top.getactivatednodes();
            //logger.debug("activated node count " + node.length);
            
            // change status isNew for each selected node
            AlarmBeanNode abn=null;
            AlarmBean ab=null;
            AlarmBean [] abeans = new AlarmBean[node.length]; // array of alarms to persist status (not) NEW
            boolean wasNewAlarm = false; // indicates that in entry set was at least one new alarm - persist it!
            for (int i = 0; i < node.length; i++) {
                try {
                    abn = (AlarmBeanNode) node[i];
                    try {
                        ab = (AlarmBean) abn.getBean();
                    }
                    catch (ClassCastException cce) {
                        logger.debug("ClassCastException not AlarmBean, it is:"+node[i]);
                    }
                } 
                catch (ClassCastException cce) {
                    logger.debug("ClassCastException not AlarmBeanNode, it is:"+node[i]);
                }
                
                if( ab != null ) {
                	if ( ab.isNew() == true ) {
                		wasNewAlarm = true;
                	}
                    ab.setIsNew(false);
                    try {
                    	abeans[i] = (AlarmBean) ab.clone();
                    }
                    catch( CloneNotSupportedException cnse) {
                    	logger.debug("CloneNotSupportedException "+cnse.getMessage() );
                    }
                }
            }
            // persisting of status data
            if ( wasNewAlarm == true ) {
            	AlarmContainer.getDefault().setNewIndicator(abeans);
            }
        }
    } // end inner class ListSelectionListener
}
