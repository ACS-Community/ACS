package si.ijs.acs.objectexplorer.engine;

import si.ijs.acs.objectexplorer.OETreeNode;
/**
 * This is the primary interface that GUI can
 * use to browse the remote objects. This interface
 * contains methods that initialize the remote
 * communication mechanism and browse the
 * name space of remote objects. It also contains
 * references to all transient objects (invocations).
 * GUI should begin its operation by instantiation of
 * one of the implementors of this interface. By passing it
 * a reference to the 
 */
public interface RemoteAccess {
/**
 * Destroys the complete remote access interface. This
 * includes the destruction of all transient remote objects
 * (invocations) as well as destruction of all nodes. The
 * implementor of this function is responsible for such
 * proper destruction sequence.
 * 
 */
void destroy();
/**
 * Given a tree node in the name space of remote objects,
 * create and initialize nodes that a given node contains.
 * <b>Note: this function will only construct the nodes, it will
 *	  not set their parents or establish any kind of relationship
 *	  between the argument node and the return values that is
 *	  connected to the management of the tree structure.
 * </b>.
 * This method should be called only on the first time that the
 * node is expanded, because it will do some remote communication.
 * The information will be cached. If the part of the tree should be
 * rescanned / reconnected, the node should destroy its content and the
 * next time the node is accessed, this function should be called again
 * to reconstruct all node children.
 * 
 * @param node a node for which this method will find children
 */
OETreeNode[] explodeTreeNode(OETreeNode node);
/**
 * Return a special engine-specific menu to be displayed in the GUI
 * 
 * @return javax.swing.JMenu
 */
javax.swing.JMenu getEngineMenu();
/**
 * Returns a list of all remote invocations managed by
 * this remote access. The list contains all invocations,
 * regardless of which introspectable instance they belong
 * to.
 *
 * @return an array of invocation instances that are currently
 *		   in progress
 */
Invocation[] getInvocations();
/**
 * Obtain the fist level of nodes in the hierarchy. These
 * will be top level nodes displayed when the tree view is
 * initialized in the GUI. Each node can be subsequently
 * expanded with a call to <code>explodeTreeNode()</code>.
 *
 * @return a sequence of tree nodes.
 */
OETreeNode[] getTreeRoots();
/**
 * Explicit instruction for the engine implementation
 * to start the communication with the remote system. Before
 * any other call is made on this interface, <code>initialize()</code>
 * must be called by the GUI. This method should do any
 * logins neccessary and should perform the lookup of the root nodes
 * that will be returned upon the calls to <code>getTreeRoots()</code>.
 * 
 */
void initialize();
}
