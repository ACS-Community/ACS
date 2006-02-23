package cern.laser.test;

import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.gp.actions.support.NodeAction;
import cern.laser.guiplatform.util.LogFactory;

/** Action sensitive to the node selection that does something useful.
 * Consider using a cookie action instead if you can define what the
 * action is applicable to in terms of cookies.
 * @author pawlowsk
 */
public class CopyNodesActionAction extends NodeAction {
    static int counter = 0;
    /** logger */
    static Logger logger = LogFactory.getLogger(CopyNodesActionAction.class.getName());
    
    protected void performAction(Node[] nodes) {
        // do work based on the current node selection, e.g.:
        //MyKindOfNode node = (MyKindOfNode)nodes[0];
        // ...
        // Note that casting to a type of node is often not the right
        // solution; try using a CookieAction, unless it is really the
        // node itself and not the underlying data that needs to be
        // considered. Also remember that some tests on nodes (casts
        // as well as reorderability of children etc.) will not work
        // when applied to filter nodes, whereas cookies will.
        System.out.println("test " + counter++);
        
        //logger.debug("test " + counter++);
    }
    
    
    protected boolean enable(Node[] nodes) {
        // e.g.:
        //return nodes.length == 1 && nodes[0] instanceof MyKindOfNode;
        //System.out.println("enable " + nodes.length + " counter: " + counter++);
        return nodes.length == 1;
        
    }
    
    public String getName() {
        return NbBundle.getMessage(CopyNodesActionAction.class, "LBL_Action_CopyNodesActionAction");
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/find.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(CopyNodesActionAction.class);
    }
    
    protected void performAction(cern.gp.nodes.GPNode[] gPNode) {
        System.out.println("performAction GPNode[] nodes");
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(CopyNodesActionAction.class, "HINT_Action"));
     * }
     */
    
}
