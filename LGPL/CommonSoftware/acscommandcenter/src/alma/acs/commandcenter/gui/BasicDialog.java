/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;



/**
 */
class BasicDialog extends JDialog implements ActionListener {

	private final CommandCenterGui master;
	protected JComponent content;
	protected JButton btnOk;
	protected boolean okChosen; // this dialog's "return value"

	public BasicDialog(CommandCenterGui gui, String title, String buttonText, JComponent content) {

		super(gui.frame, title, true);
		this.master = gui;
		this.content = content;
		
		content.setBorder(new EmptyBorder(10, 10, 10, 10));
		this.getContentPane().add(content, BorderLayout.CENTER);
		
		JPanel south = new JPanel();
		btnOk = new JButton(buttonText);
		south.add(btnOk);
		btnOk.addActionListener(this);
		
		this.getContentPane().add(south, BorderLayout.SOUTH);
		this.pack();
		
		btnOk.setName("btn_Ok");
	}

	public void actionPerformed (ActionEvent evt) {
		this.okChosen = true;
		setVisible(false);
	}

	public void bringUp () {
		this.okChosen = false;
		this.master.correctDialogLocation(this);
		super.setVisible(true);
	}
}
////////////////////////////////////////////////////////
/// ------------------- API ------------------------ ///
////////////////////////////////////////////////////////

////////////////////////////////////////////////////////
/// ----------------- Internal --------------------- ///
////////////////////////////////////////////////////////

//
//
//
//
//
//
//
//
//
//
//
//