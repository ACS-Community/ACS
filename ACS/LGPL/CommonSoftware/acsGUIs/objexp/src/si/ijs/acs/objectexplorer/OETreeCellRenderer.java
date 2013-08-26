package si.ijs.acs.objectexplorer;

/**
 * Insert the type's description here.
 * Creation date: (3/26/2001 7:44:36 PM)
 * @author: Miha Kadunc
 */
import javax.swing.JTree;
import java.awt.Component;
import si.ijs.acs.objectexplorer.engine.*;
 
public class OETreeCellRenderer extends javax.swing.tree.DefaultTreeCellRenderer {

  private boolean hasFocus;
  private java.awt.Color connectedColor= new java.awt.Color(0,120,116);
  private java.awt.Color connectedColorSticky= new java.awt.Color(120,0,32);
  private java.awt.Color connectedBackColor= new java.awt.Color(147,185,243);
  private java.awt.Color selectedBackColor=null;
  private boolean  valuesAreOE=false;

/**
 * Insert the method's description here.
 * Creation date: (3/26/2001 9:24:59 PM)
 */
public OETreeCellRenderer() {
  super();
  
  this.selectedBackColor=getBackgroundSelectionColor();	
}
/**
 * Insert the method's description here.
 * Creation date: (3/26/2001 9:21:43 PM)
 * @return java.awt.Color
 */
public java.awt.Color getConnectedBackColor() {
	return connectedBackColor;
}
/**
 * Insert the method's description here.
 * Creation date: (3/26/2001 9:21:43 PM)
 * @return java.awt.Color
 */
public java.awt.Color getConnectedColor() {
	return connectedColor;
}
public java.awt.Color getConnectedColorSticky() {
	return connectedColorSticky;
}
/**
 * Insert the method's description here.
 * Creation date: (3/26/2001 7:48:20 PM)
 */
public Component getTreeCellRendererComponent(
	JTree tree,
	Object value,
	boolean sel,
	boolean expanded,
	boolean leaf,
	int row,
	boolean hasFocus) {
	//ibm.597
	setComponentOrientation(tree.getComponentOrientation()); //ibm.597
	//ibm.597

	String stringValue= tree.convertValueToText(value, sel, expanded, leaf, row, hasFocus);

	this.hasFocus= hasFocus;
	if (!valuesAreOE) {
	    if (value instanceof OETreeNode) valuesAreOE=true;
	    else return this;
	}
	if ((((OETreeNode)value).isIntrospectable()) && (((Introspectable) value).isConnected())) {
		boolean isNonSticky = ((Introspectable) value).isNonSticky();
		// indicate non-sticky usuing italic style & different color
		setForeground(isNonSticky ? getConnectedColor() : getConnectedColorSticky());
		setFont(new java.awt.Font("dialog", java.awt.Font.BOLD, 12));
		setBackgroundSelectionColor(getConnectedBackColor());
	} else if (sel) {
		setForeground(getTextSelectionColor());
		setFont(new java.awt.Font("dialog", 0, 12));
		setBackgroundSelectionColor(selectedBackColor);
		stringValue=stringValue+" ";
	} else {
		setFont(new java.awt.Font("dialog", 0, 12));
		setForeground(getTextNonSelectionColor());
		stringValue=stringValue+" ";
	}

	// There needs to be a way to specify disabled icons.
	if (!tree.isEnabled()) {
		setEnabled(false);
		if (leaf) {
			setDisabledIcon(getOpenIcon());
		} else if (expanded) {
			setDisabledIcon(getLeafIcon());
		} else {
			setDisabledIcon(getClosedIcon());
		}
	} else {
		setEnabled(true);
		if ((value instanceof OETreeNode) && (((OETreeNode) value).getIcon() != null)) {
			setIcon(((OETreeNode)value).getIcon());
		} else if (leaf) {
			setIcon(getLeafIcon());
		} else if (expanded) {
			setIcon(getOpenIcon());
		} else {
			setIcon(getClosedIcon());
		}
	}
	if (value instanceof Invocation) {
	    setText(stringValue+"["+((Invocation)value).getInvocationRequest().getSN()+"]");
	}
	else setText(stringValue);
	setSize(getHeight(), tree.getWidth());
	selected= sel;

	return this;
}
}
