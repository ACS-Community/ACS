package cern.gp.nodes.impl;

import org.openide.nodes.FilterNode;
import org.openide.nodes.Node;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.children.NodeCollection;

/**
 * A start of a GP implementation of the FilterNode
 * TODO finish it...
 * @see org.openide.nodes.FilterNode
 * @author Vito Baggiolini
 * @version $Revision: 1.1 $ $Date: 2005/06/07 03:26:13 $
 */
public class GPFilterNode extends FilterNode implements GPNode {
	private final GPNode origGPNode;
	
	public GPFilterNode(GPNode original) {
		super(original.getPeerNode());
		this.origGPNode = original; 
	}
	
	
  /**
   * @see cern.gp.nodes.GPNode#getBean()
   */
  public Object getBean() {
    return origGPNode.getBean();
  }

  /**
   * @see cern.gp.nodes.GPNode#getNodeCollection()
   */
  public NodeCollection getNodeCollection() {
    return origGPNode.getNodeCollection();
  }

  /**
   * @see cern.gp.nodes.GPNode#getParent()
   */
  public GPNode getParent() {
    return origGPNode.getParent();
  }

  /**
   * @see cern.gp.nodes.GPNode#getPeerNode()
   */
  public Node getPeerNode() {
    return origGPNode.getPeerNode();
  }

}
