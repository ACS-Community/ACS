package cern.laser.guiplatform.actions.category;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import alma.acs.container.ContainerServicesBase;

import cern.gp.nodes.GPNode;
import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.services.browsing.AlarmBrowsingHandler;
import cern.laser.console.CommentedAlarm;
import cern.laser.guiplatform.alarms.AlarmBrowsingHandlerFactory;
import cern.laser.guiplatform.alarms.InfoAlarmBean;
import cern.laser.guiplatform.category.CategoryBean;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.alarms.AlarmInfoExplorer;


/** Action sensitive to the node selection that does something useful.
 * Consider using a cookie action instead if you can define what the
 * action is applicable to in terms of cookies.
 * @author Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class ShowAlarmByCategoryAction extends cern.gp.actions.support.NodeAction {

    /** action name */
    private static final String name = 
        NbBundle.getMessage(ShowAlarmByCategoryAction.class, 
                        "LBL_Action_ShowAlarmByCategoryAction_action_name");

    /** logger */
    private static final Logger logger = 
            LogFactory.getLogger(ShowAlarmByCategoryAction.class.getName());
    
    private ContainerServicesBase contSvcs;
    
    public ShowAlarmByCategoryAction(ContainerServicesBase contSvcs) {
    	this.contSvcs=contSvcs;
    }
  
    protected void performAction(GPNode [] activatedNodes) {
        AcWindowManager.setStatusText("Show alarm by category action is running .......");
        
        CategoryBean cb = (CategoryBean) activatedNodes[0].getBean();
        logger.debug("cateogory Id: " + cb.getCategoryId());

        AlarmBrowsingHandler browser = null;
        AlarmInfoExplorer expl = null;
        List abAlarms = new java.util.ArrayList();
        try {
            browser = AlarmBrowsingHandlerFactory.getHandler(contSvcs);
            Collection alarms = browser.getAlarmsByCategory(cb.getCategoryId());

            GPNode [] nodes = new GPNode[alarms.size()];
            int i = 0;
            // rest alarms to active list
            for (Iterator iter = alarms.iterator(); iter.hasNext(); ) {
                Alarm alarm = (Alarm) iter.next();
                CommentedAlarm commentedAlarm = new CommentedAlarm(alarm, null); // without comment
                InfoAlarmBean ab = new InfoAlarmBean(commentedAlarm);

                // temporary solution (info mode)
                ab.setInfoMode(true);

                abAlarms.add(ab);
            }
            
            // if expolorer is already open update nodes 
            TopComponent top = AcWindowManager.findTopComponent(
                        NbBundle.getMessage(
                            AlarmInfoExplorer.class, 
                            "LBL_AlarmInfoExplorer_component_name"));

            if ( top != null ) {
                expl = (AlarmInfoExplorer) top;
                expl.update(abAlarms, 
                            "Alarms belonging to " + cb.getPath()  + " category");
                logger.debug("expl found");
            } else {
                logger.debug("expl not found");
                expl = new AlarmInfoExplorer(abAlarms, 
                            "Alarms belonging to " + cb.getPath()  + " category",
                            Constants.getColumnsToDisplay());
                expl.open();
            }

            expl.requestFocus();

        } catch (LaserException le) {
            logger.error(le.getMessage(), le);
            logger.error(le.getRootCause().getMessage(), le.getRootCause());
            AcWindowManager.notifyError("Can't dispaly alarms.\nAlrmBrowsinHandler can't be found.\n" +
                    "Can't connect to database.");
        }

        AcWindowManager.setStatusText("Show alarm by category action finished");
    }
   
    protected boolean enable(Node[] nodes) {
        // e.g.:
        return nodes.length == 1;// && nodes[0] instanceof MyKindOfNode;
    }
    
    public String getName() {
        return name;
    }
    
    protected String iconResource() {
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ShowAlarmByCategoryAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ShowAlarmByCategoryAction.class, "HINT_Action"));
     * }
     */
    
}
