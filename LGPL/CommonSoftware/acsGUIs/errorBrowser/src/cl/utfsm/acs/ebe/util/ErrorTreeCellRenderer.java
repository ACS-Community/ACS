package cl.utfsm.acs.ebe.util;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

public class ErrorTreeCellRenderer extends DefaultTreeCellRenderer {
	
	/*
	 *  ImageIcon tutorialIcon = createImageIcon("images/middle.gif");
     *  if (tutorialIcon != null) {
     *  tree.setCellRenderer(new MyRenderer(tutorialIcon));
     *  
	 */

	/**
	 * 
	 */
	private static final long serialVersionUID = 2773311893986633892L;
	Icon errorIcon;
	Icon completionIcon;

    public ErrorTreeCellRenderer() {
        /*Initilize here the icons*/
    }

    public Component getTreeCellRendererComponent(
                        JTree tree,
                        Object value,
                        boolean sel,
                        boolean expanded,
                        boolean leaf,
                        int row,
                        boolean hasFocus) {

        super.getTreeCellRendererComponent(
                        tree, value, sel,
                        expanded, leaf, row,
                        hasFocus);
        if (leaf && isTutorialBook(value)) {
            setIcon(null);
            setToolTipText("This book is in the Tutorial series.");
        } else {
            setToolTipText(null); //no tool tip
        } 

        return this;
    }

    protected boolean isTutorialBook(Object value) {
        DefaultMutableTreeNode node =
                (DefaultMutableTreeNode)value;
        Object nodeInfo =
                (Object)(node.getUserObject());
        String title = nodeInfo.toString();
        if (title.indexOf("Tutorial") >= 0) {
            return true;
        }

        return false;
    }
}
