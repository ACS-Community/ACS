/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

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




class EditContainerSettingsDialog extends JDialog implements
        ActionListener {

    private final CommandCenterGui master;

    JRadioButton btnGlobal, btnCustom;

    JTextField defaultScriptBaseF, defaultHostF, defaultAccountF,
            defaultPasswordF;

    JTextField customScriptBaseF, customHostF, customAccountF,
            customPasswordF;

    ButtonGroup buttonGroup;

    boolean okChosen; // this dialog's "return value"

    protected EditContainerSettingsDialog(CommandCenterGui gui, String title,
            String explanationText, String buttonText) {
        super(gui.frame, title, true);
        this.master = gui;
        this.getContentPane().setLayout(new BorderLayout());
        JPanel root = new JPanel(new BorderLayout()); // need 'root' to be
        // able to set a
        // border
        root.setBorder(new EmptyBorder(10, 10, 10, 10));
        // --- content
        JPanel content = new JPanel();
        content.setLayout(new BorderLayout());
        JLabel north = new JLabel(explanationText);
        content.add(north, BorderLayout.NORTH);
        JPanel center = new JPanel();
        center.setLayout(new SpringLayout());
        center.add(btnGlobal = new JRadioButton("On same host as manager"));
        center.add(new JLabel("(see Common Settings)"));
        center.add(new JLabel("Acs Instance"));
        center.add(defaultScriptBaseF = new JTextField());
        center.add(new JLabel("Remote Host"));
        center.add(defaultHostF = new JTextField());
        center.add(new JLabel("Remote Username"));
        center.add(defaultAccountF = new JTextField());
        center.add(new JLabel("Remote Password"));
        center.add(defaultPasswordF = new JPasswordField());
        defaultScriptBaseF.setEditable(false);
        defaultHostF.setEditable(false);
        defaultAccountF.setEditable(false);
        defaultPasswordF.setEditable(false);
        center.add(btnCustom = new JRadioButton("On the following host"));
        center.add(new JLabel("")); // "(implies SSH connection)"
        center.add(new JLabel("Acs Instance"));
        center.add(customScriptBaseF = new JTextField());
        center.add(new JLabel("Remote Host"));
        center.add(customHostF = new JTextField());
        center.add(new JLabel("Remote Username"));
        center.add(customAccountF = new JTextField());
        center.add(new JLabel("Remote Password"));
        center.add(customPasswordF = new JPasswordField());
        SpringUtilities.makeCompactGrid(center, 0, 2);
        buttonGroup = new ButtonGroup();
        buttonGroup.add(btnGlobal);
        buttonGroup.add(btnCustom);
        btnGlobal.setSelected(true);
        content.add(center, BorderLayout.CENTER);
        root.add(content, BorderLayout.CENTER);
        // --- controls
        JPanel controls = new JPanel();
        JButton btnOk = new JButton(buttonText);
        controls.add(btnOk);
        btnOk.addActionListener(this);
        root.add(controls, BorderLayout.SOUTH);
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

    public void bringUp() {
        this.okChosen = false;
        this.master.correctDialogLocation(this);
        super.setVisible(true);
    }

    public void actionPerformed(ActionEvent evt) {
        this.okChosen = true;
        setVisible(false);
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