package cern.laser.guiplatform.actions.alarms;

import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import alma.acs.container.ContainerServicesBase;

import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;

/** Action sensitive to the node selection that does something useful.
 * Consider using a cookie action instead if you can define what the
 * action is applicable to in terms of cookies.
 * @author pawlowsk
 */
public class DetailsAction extends org.openide.util.actions.NodeAction {
	
	private final ContainerServicesBase contSvcs;
	
	public DetailsAction(ContainerServicesBase contSvcs) {
		this.contSvcs=contSvcs;
	}
    
    /*
    protected void performAction(Node[] nodes) {
        // do work based on the current node selection, e.g.:
        MyKindOfNode node = (MyKindOfNode)nodes[0];
        // ...
        // Note that casting to a type of node is often not the right
        // solution; try using a CookieAction, unless it is really the
        // node itself and not the underlying data that needs to be
        // considered. Also remember that some tests on nodes (casts
        // as well as reorderability of children etc.) will not work
        // when applied to filter nodes, whereas cookies will.
    }
    */
    /*
    protected boolean enable(Node[] nodes) {
        // e.g.:
        return nodes.length == 1 && nodes[0] instanceof MyKindOfNode;
    }
    */
    protected boolean enable(Node[] nodes) {
        // e.g.:
        return nodes.length == 1;// && nodes[0] instanceof MyKindOfNode;
    }
 
    public String getName() {
        return NbBundle.getMessage(DetailsAction.class, 
                                    "LBL_Action_DetailsAction_action_name");
    }
    
    protected String iconResource() {
        return "cern/laser/guiplatform/images/view_details.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(DetailsActionAction.class);
    }
    
    protected void performAction(org.openide.nodes.Node[] node) {
        for ( int i=0; i< node.length; i++) {
            ((AlarmBean)((AlarmBeanNode)node[i]).getBean()).details(contSvcs);
        }
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(DetailsActionAction.class, "HINT_Action"));
     * }
     */
    
}
