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
	JTextField modifF;
	JLabel modifL;
	JTextField defaultScriptBaseF, defaultHostF, defaultAccountF, defaultPasswordF;
	JTextField customScriptBaseF, customHostF, customAccountF, customPasswordF;
	JLabel customAccountL, customPasswordL;
	
	ButtonGroup buttonGroup;
	boolean okChosen; // this dialog's "return value"

	protected EditContainerSettingsDialog(CommandCenterGui gui) {
		super(gui.frame, "Edit detail settings for container", true);
		this.master = gui;
		this.getContentPane().setLayout(new BorderLayout());
		
		// --- content
		JPanel content = new JPanel();
		content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS));

		JPanel pnlModifiers = new JPanel();
		pnlModifiers.setBorder(master.createTitledBorder(" Tweak type of container "));
		pnlModifiers.setLayout(new SpringLayout());
		pnlModifiers.add(modifL = new JLabel("Type Modifiers"));
		pnlModifiers.add(modifF = new JTextField());
		modifF.setToolTipText("Enter comma-separated Modifiers, e.g. \"archiveContainer\"");
		SpringUtilities.makeCompactGrid(pnlModifiers, 0, 2);
		content.add(pnlModifiers);

		JPanel pnlLocation = new JPanel();
		pnlLocation.setBorder(master.createTitledBorder(" Where to run this container "));
		pnlLocation.setLayout(new SpringLayout());
		pnlLocation.add(btnGlobal = new JRadioButton("On same host as manager"));
		pnlLocation.add(new JLabel("(see Common Settings)"));
		pnlLocation.add(new JLabel("Acs Instance"));
		pnlLocation.add(defaultScriptBaseF = new JTextField());
		pnlLocation.add(new JLabel("Remote Host"));
		pnlLocation.add(defaultHostF = new JTextField());
		pnlLocation.add(new JLabel("Remote Username"));
		pnlLocation.add(defaultAccountF = new JTextField());
		pnlLocation.add(new JLabel("Remote Password"));
		pnlLocation.add(defaultPasswordF = new JPasswordField());
		defaultScriptBaseF.setEditable(false);
		defaultHostF.setEditable(false);
		defaultAccountF.setEditable(false);
		defaultPasswordF.setEditable(false);

		pnlLocation.add(btnCustom = new JRadioButton("On the following host"));
		pnlLocation.add(new JLabel("")); // "(implies SSH connection)"
		pnlLocation.add(new JLabel("Acs Instance"));
		pnlLocation.add(customScriptBaseF = new JTextField());
		pnlLocation.add(new JLabel("Remote Host"));
		pnlLocation.add(customHostF = new JTextField());
		pnlLocation.add(customAccountL = new JLabel("Remote Username"));
		pnlLocation.add(customAccountF = new JTextField());
		pnlLocation.add(customPasswordL = new JLabel("Remote Password"));
		pnlLocation.add(customPasswordF = new JPasswordField());
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
		defaultScriptBaseF.setName("txt_DefaultInstance");
		defaultHostF.setName("txt_DefaultHost");
		defaultAccountF.setName("txt_DefaultUser");
		defaultPasswordF.setName("txt_DefaultPassword");
		customScriptBaseF.setName("txt_CustomInstance");
		customHostF.setName("txt_CustomHost");
		customAccountF.setName("txt_CustomUser");
		customPasswordF.setName("txt_CustomPassword");
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
