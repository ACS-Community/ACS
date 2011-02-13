package cern.laser.guiplatform.actions.alarms;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.actions.NodeAction;
import org.openide.windows.TopComponent;

import alma.acs.container.ContainerServicesBase;

import cern.gp.nodes.GPNode;
import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.services.reduction.AlarmReductionHandler;
import cern.laser.console.CommentedAlarm;
import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmReductionHandlerFactory;
import cern.laser.guiplatform.alarms.InfoAlarmBean;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.alarms.AlarmInfoExplorer;


/** Action sensitive to the node selection that does something useful.
 * Consider using a cookie action instead if you can define what the
 * action is applicable to in terms of cookies.
 * @author Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class ShowNodeChildrenAction extends NodeAction {

    /** logger */
    private static final Logger logger = 
            LogFactory.getLogger(ShowNodeChildrenAction.class.getName());
    
    private final ContainerServicesBase contSvcs;
    
    public ShowNodeChildrenAction(ContainerServicesBase contSvcs) {
    	this.contSvcs=contSvcs;
    }
     
    protected void performAction(Node[] _nodes) {

        //try if this work with NB actions instead of GP actions
        AcWindowManager.setStatusText("Show node children action is running .......");

        AlarmBean _ab = (AlarmBean) _nodes[0].getCookie(AlarmBean.class);

        AlarmReductionHandler browser = null;
        AlarmInfoExplorer expl = null;
        List abAlarms = new java.util.ArrayList();
         try {
            browser = AlarmReductionHandlerFactory.getHandler(contSvcs);
            Collection alarms = browser.getNodeChildren(_ab.getAlarmId());

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
                            _ab.getFaultFamily() + " node children");
                logger.debug("expl found");
            } else {
                logger.debug("expl not found");
                expl = new AlarmInfoExplorer(abAlarms, 
                            _ab.getFaultFamily() + " node children",
                            Constants.getColumnsToDisplay());
                expl.open();
            }

            expl.requestFocus();

        } catch (LaserException le) {
            logger.error(le.getMessage(), le);
            logger.error(le.getRootCause().getMessage(), le.getRootCause());
            AcWindowManager.notifyError("Can't display alarms.\nAlrmReductionHandler can't be found.\n" +
                    "Can't connect to database.");
        }


        AcWindowManager.setStatusText("Show node children action finished");

   }
    
    protected boolean enable(Node[] nodes) {
        // e.g.:
        return nodes.length == 1 && checkBean(nodes[0]); 
    }
    
    public String getName() {
        return NbBundle.getMessage(ShowNodeChildrenAction.class, 
                                "LBL_ShowNodeChildrenAction_action_name");
    }
    
    protected String iconResource() {
        //return "cern/laser/guiplatform/actions/alarms/ShowNodeChildrenActionIcon.gif";
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ShowNodeChildrenAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ShowNodeChildrenAction.class, "HINT_Action"));
     * }
     */
    
    /**
     * check if this action should be enable depending on AlarmBean state
     * @return 
     */ 
    private boolean checkBean(Node node) {
        boolean result = false;
        
        org.openide.nodes.Node.Cookie cookie = null;
        cookie = node.getCookie(AlarmBean.class);
        
        if ( cookie != null && ((AlarmBean) cookie).isNodeParent() )  
            result = true;

        return result;
    }
 
}
