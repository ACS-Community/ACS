/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.border.EmptyBorder;

import alma.acs.commandcenter.gui.thirdparty.SpringUtilities;



class EditContainerSettingsDialog extends JDialog implements ActionListener {

	private final CommandCenterGui master;
	JRadioButton btnGlobal, btnCustom;
	JTextField modifF, heapF;
	JLabel modifL, heapL;
	JTextField defaultHostF, defaultAccountF, defaultPasswordF;
	JTextField hostF, accountF, passwordF;
	JLabel hostL, accountL, passwordL;
	
	ButtonGroup buttonGroup;
	boolean okChosen; // this dialog's "return value"

	protected EditContainerSettingsDialog(CommandCenterGui gui) {
		super(gui.frame, "Container Settings ", true);
		this.master = gui;
		this.getContentPane().setLayout(new BorderLayout());
		
		// --- content
		JPanel content = new JPanel();
		content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS));

		JPanel pnlFlagsAndMods = new JPanel();
		pnlFlagsAndMods.setBorder(master.createTitledBorder(" Tweak container "));
		pnlFlagsAndMods.setLayout(new SpringLayout());
		pnlFlagsAndMods.add(modifL = new JLabel("Type Modifiers"));
		pnlFlagsAndMods.add(modifF = new JTextField());
		pnlFlagsAndMods.add(heapL = new JLabel("Heap Size (MB)"));
		pnlFlagsAndMods.add(heapF = new JTextField());
		heapF.setToolTipText("Enter custom heap size (in Megabytes)");
		modifF.setToolTipText("Enter comma-separated Modifiers, e.g. \"archiveContainer\"");
		SpringUtilities.makeCompactGrid(pnlFlagsAndMods, 0, 2);
		content.add(pnlFlagsAndMods);

		JPanel pnlLocation = new JPanel();
		pnlLocation.setBorder(master.createTitledBorder(" Where to run this container "));
		pnlLocation.setLayout(new SpringLayout());
		pnlLocation.add(btnGlobal = new JRadioButton("On same host as manager"));
		pnlLocation.add(new JLabel("(see Common Settings)"));
		pnlLocation.add(new JLabel("Remote Host"));
		pnlLocation.add(defaultHostF = new JTextField());
		pnlLocation.add(new JLabel("Remote Username"));
		pnlLocation.add(defaultAccountF = new JTextField());
		pnlLocation.add(new JLabel("Remote Password"));
		pnlLocation.add(defaultPasswordF = new JPasswordField());
		defaultHostF.setEditable(false);
		defaultAccountF.setEditable(false);
		defaultPasswordF.setEditable(false);

		pnlLocation.add(btnCustom = new JRadioButton("On the following host"));
		pnlLocation.add(new JLabel("")); // "(implies SSH connection)"
		pnlLocation.add(hostL = new JLabel("Remote Host"));
		pnlLocation.add(hostF = new JTextField());
		pnlLocation.add(accountL = new JLabel("Remote Username"));
		pnlLocation.add(accountF = new JTextField());
		pnlLocation.add(passwordL = new JLabel("Remote Password"));
		pnlLocation.add(passwordF = new JPasswordField());
		SpringUtilities.makeCompactGrid(pnlLocation, 0, 2);

		buttonGroup = new ButtonGroup();
		buttonGroup.add(btnGlobal);
		buttonGroup.add(btnCustom);
		btnGlobal.setSelected(true);
		content.add(pnlLocation);
		
		// --- controls
		JPanel controls = new JPanel();
		JButton btnOk = new JButton("Set");
		controls.add(btnOk);
		btnOk.addActionListener(this);

		
		// need 'root' to be able to set a border
		Box root = Box.createVerticalBox();
		root.setBorder(new EmptyBorder(10, 10, 10, 10));
		root.add(content);
		root.add(Box.createVerticalGlue());
		root.add(controls);
		this.getContentPane().add(root);
		this.pack();

		btnGlobal.setName("rdb_Global");
		btnCustom.setName("rdb_Custom");
		defaultHostF.setName("txt_DefaultHost");
		defaultAccountF.setName("txt_DefaultUser");
		defaultPasswordF.setName("txt_DefaultPassword");
		hostF.setName("txt_CustomHost");
		accountF.setName("txt_CustomUser");
		passwordF.setName("txt_CustomPassword");
		btnOk.setName("btn_Ok");
	}

	public void bringUp () {
		this.okChosen = false;
		this.master.correctDialogLocation(this);
		super.setVisible(true);
	}

	public void actionPerformed (ActionEvent evt) {
		this.okChosen = true;
		setVisible(false);
	}
}
