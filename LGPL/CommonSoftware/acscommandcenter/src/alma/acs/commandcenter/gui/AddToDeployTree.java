/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.event.KeyEvent;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;

import alma.acs.commandcenter.gui.CommandCenterGui.ActionBaseClass;
import alma.acs.commandcenter.gui.thirdparty.SpringUtilities;
import alma.acs.util.AcsLocations;



/**
 * A litte panel that allows to manually add managers to the deployment tree.
 */
class AddToDeployTree extends JPanel {

	private final CommandCenterGui master;
	private final DeploymentTree deployTree;
	protected JTextField hostF = new JTextField(10);
	protected JTextField portF = new JTextField(4);

	public AddToDeployTree(CommandCenterGui gui, DeploymentTree deployTree) {

		JLabel lbl;
		JButton btnAdd;
		JButton btnRefresh;
		JPanel temp;

		this.master = gui;
		this.deployTree = deployTree;

		this.setLayout(new BorderLayout());

		temp = new JPanel(new SpringLayout());
		this.add(temp, BorderLayout.WEST);

		temp.add(lbl = new JLabel("Mgr Host"));
		temp.add(new JLabel("Port"));
		temp.add(new JPanel(/* empty */));

		temp.add(hostF);
		temp.add(portF);
		temp.add(btnAdd = new JButton(new ActionAdd()));

		temp.add(new JPanel(/* empty */));
		temp.add(new JPanel(/* empty */));
		temp.add(btnRefresh = new JButton(new ActionRefresh()));

		SpringUtilities.makeCompactGrid(temp, 0, 3);


		lbl.setLabelFor(hostF);
		lbl.setDisplayedMnemonic(KeyEvent.VK_M);
		hostF.setText("");
		portF.setText("");
		btnAdd.setToolTipText("Add specified Manager to Deployment Info");
		btnRefresh.setToolTipText("Refresh all Managers in Deployment Info");

		btnAdd.setName("btn_Add_To_DeployTree");
		btnRefresh.setName("btn_Refresh_DeployTree");
		hostF.setName("txt_Add_Mgr_Host");
		portF.setName("txt_Add_Mgr_Port");
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
	abstract protected class MyActionBaseClass extends ActionBaseClass {
		public MyActionBaseClass(String name) {
			master.super(name);
		}
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
	
	protected class ActionAdd extends MyActionBaseClass {

		public ActionAdd() {
			super("Add to View");
		}

		protected void myActionPerformed () throws Throwable {
			String host = hostF.getText().trim();
			String port = portF.getText().trim();

			if (host == null || "".equals(host)) {
				JOptionPane.showMessageDialog(deployTree, "The specified Host is invalid");
				return;
			}

			if (host.length() > 4 && host.substring(0,4).equalsIgnoreCase("IOR:")) {
				// msc 2005-05: we allow (for now)
				// to give an IOR in the host field
				deployTree.shieldedAddManager(host);
				return;
			} 
			
			if (port == null || "".equals(port) || port.length() < 4) {
					JOptionPane.showMessageDialog(deployTree, "The specified TCP Port is invalid");
					return;
			}
						
			deployTree.shieldedAddManager(AcsLocations.convertToManagerLocation(host, port));
		}

	}


	protected class ActionRefresh extends MyActionBaseClass {

		public ActionRefresh() {
			super("Full Refresh");
		}

		protected void myActionPerformed () throws Throwable {
			deployTree.refreshManagers();
		}

	}

}
