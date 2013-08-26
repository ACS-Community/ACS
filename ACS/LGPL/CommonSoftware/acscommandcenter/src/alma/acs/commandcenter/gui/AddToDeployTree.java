/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.KeyEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.SpringLayout;

import alma.acs.commandcenter.gui.CommandCenterGui.BackgroundAction;
import alma.acs.commandcenter.gui.thirdparty.SpringUtilities;
import alma.acs.commandcenter.util.MiscUtils;



/**
 * A litte panel that allows to manually add managers to the deployment tree.
 */
class AddToDeployTree extends JPanel {

	private final CommandCenterGui master;
	private final DeploymentTree deployTree;
	protected JPanel content, content2;
	protected JTextField addressF = new JTextField(10);

	public AddToDeployTree(CommandCenterGui gui, DeploymentTree deployTree) {

		JLabel lbl;
		JButton btnAdd;
		JButton btnRefresh;
		JToggleButton btnFreeze;

		this.master = gui;
		this.deployTree = deployTree;

		this.setLayout(new BorderLayout());

		content = new JPanel(new SpringLayout());
		this.add(content, BorderLayout.WEST);

		content.add(btnRefresh = new JButton(new ActionRefresh()));
		content.add(btnFreeze = new JToggleButton()); /* action assigned below */
		content.add(new JSeparator(JSeparator.VERTICAL));
		content.add(new JButton(new ActionShowAdd()));

		SpringUtilities.makeCompactGrid(content, 0, 4);


		content2 = new JPanel();
		this.add(content2, BorderLayout.SOUTH);
		content2.setVisible(false);

		content2.add(lbl = new JLabel("Enter \"host:instance\", corbaloc, or IOR:"));
		content2.add(Box.createVerticalStrut(5));
		content2.add(addressF);
		JPanel temp = new JPanel();
		content2.add(temp);
		temp.add(btnAdd = new JButton(new ActionAdd()));
		temp.add(new JButton(new ActionCancelAdd()));

		content2.setLayout(new BoxLayout(content2, BoxLayout.PAGE_AXIS));
		// http://java.sun.com/docs/books/tutorial/uiswing/layout/box.html#features
		for (Component c : content2.getComponents())
			((JComponent)c).setAlignmentX(LEFT_ALIGNMENT);


		btnFreeze.setAction(new ActionFreeze(btnFreeze));
		/* initially sync the button state with the tree.
		 * note the button will not continuously be synced later 
		 * on, so if somebody other than the button changes the
		 * deploytree's freeze flag, we'll be out of sync. */
		btnFreeze.setSelected(deployTree.isViewFrozen()); 

		lbl.setLabelFor(addressF);
		lbl.setDisplayedMnemonic(KeyEvent.VK_M);
		addressF.setText("");
		btnAdd.setToolTipText("Add specified Manager to Deployment Info");
		btnRefresh.setToolTipText("Refresh all Managers in Deployment Info");
		btnFreeze.setToolTipText("Halt automatic Refresh of Deployment Info");

		btnAdd.setName("btn_Add_To_DeployTree");
		btnRefresh.setName("btn_Refresh_DeployTree");
		addressF.setName("txt_Add_Mgr_Host");
	}

	
	/**
	 * Signals to the user that an action takes longer.
	 */
	protected void setBusy (boolean b) {
		int cursor = (b)? Cursor.WAIT_CURSOR : Cursor.DEFAULT_CURSOR; 
		this.setCursor(Cursor.getPredefinedCursor(cursor));
	}

	
	/**
	 * An extension of {@link CommandCenterGui.ActionBaseClass} that
	 * switches to the {@link Cursor#WAIT_CURSOR} cursor while the action
	 * is running and switches back to the {@link Cursor#DEFAULT_CURSOR}
	 * afterwards.
	 */
	abstract protected class MyActionBaseClass extends BackgroundAction {
		public MyActionBaseClass(String name) {
			master.super(name);
		}
		@Override
		final protected void actionPerformed () throws Throwable {
			setBusy(true);
			deployTree.setBusy(true);
			
			try {
				myActionPerformed();
				
			}  finally {
				setBusy(false);
				deployTree.setBusy(false);
			}
		}
		abstract protected void myActionPerformed() throws Throwable;
	}

	protected class ActionShowAdd extends MyActionBaseClass {

		public ActionShowAdd() {
			super("Add...");
			super.putValue(SHORT_DESCRIPTION, "Manually add a Manager to Deployment Info");
		}

		@Override
		protected void myActionPerformed () throws Throwable {
			content.setVisible(false);
			content2.setVisible(true);
		}
		
	}

	protected class ActionCancelAdd extends MyActionBaseClass {

		public ActionCancelAdd() {
			super("Cancel");
		}

		@Override
		protected void myActionPerformed () throws Throwable {
			content2.setVisible(false);
			content.setVisible(true);
		}
		
	}
	
	protected class ActionAdd extends MyActionBaseClass {

		public ActionAdd() {
			super("Add to View");
		}

		@Override
		protected void myActionPerformed () throws Throwable {
			String input = addressF.getText().trim();

			if (input == null || "".equals(input)) {
				JOptionPane.showMessageDialog(AddToDeployTree.this, "Please enter the network address of an Acs Manager");
				return;
			}

			String host;
			if (input.length() > 4 && input.substring(0,4).equalsIgnoreCase("IOR:")) {
				// msc 2005-05: we allow to give an IOR in the host field
				host = input;
			} 
			else
			if (input.length() > 9 && input.substring(0,9).equalsIgnoreCase("corbaloc:")) {
				// msc 2006-10: we allow to give an corbaloc in the host field
				host = input;
			} 
			else {
				String quick = MiscUtils.convertShortNotationToCorbaloc(input);
				if (quick != null) {
					// msc 2009-12: we allow to give "host:instance" in the host field
					host = quick;
				} else {
					JOptionPane.showMessageDialog(AddToDeployTree.this, "Cannot parse your input, please use a supported address format");
					return;
				}
			}

			content2.setVisible(false);
			content.setVisible(true);
			deployTree.shieldedAddManager(host);
		}

	}


	protected class ActionRefresh extends MyActionBaseClass {

		public ActionRefresh() {
			super("Refresh");
		}

		@Override
		protected void myActionPerformed () throws Throwable {
			/* if view is frozen, no updates will occur on the tree
			 * including those that the user forces through the
			 * refresh button. this context info ('automatic refresh'
			 * or 'user refresh') is not available in the deploytree.
			 * thus, we temporarily disable the freeze and reenable it
			 * afterwards. note we could use the public getter and setter
			 * for the flag, but we're not interested in any of the logic
			 * inside the setter, so we operate on the flag directly */
			boolean isViewFrozen = deployTree.isViewFrozen;
			if (isViewFrozen)
				deployTree.isViewFrozen = false;
			
			try {
				deployTree.refreshManagers();
			
			} finally {
				if (isViewFrozen)
					deployTree.isViewFrozen = true;
			}
		}

	}

	protected class ActionFreeze extends MyActionBaseClass {
		
		protected JToggleButton btn;
		
		public ActionFreeze (JToggleButton btn) {
			super("Freeze");
			this.btn = btn;
		}
		
		@Override
		protected void myActionPerformed () throws Throwable {
			boolean b = btn.isSelected();
			deployTree.setViewFrozen(b);
		}
	}
	
	
}

