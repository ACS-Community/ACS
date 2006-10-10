/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.util.HashMap;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;

import alma.acs.commandcenter.gui.thirdparty.SpringUtilities;
import alma.entity.xmlbinding.acscommandcentertools.Insertion;
import alma.entity.xmlbinding.acscommandcentertools.Tool;
import alma.entity.xmlbinding.acscommandcentertools.types.InsertionSourceType;



class ToolInputPanel extends JPanel {

    private final CommandCenterGui master;
    protected int counter = 0;

    protected ToolInputPanel(CommandCenterGui gui, Tool tool) {
        super(new SpringLayout());
        this.master = gui;
        Insertion[] insertions = tool.getInsertion();
        for (int i = 0; i < insertions.length; i++) {
            Insertion ins = tool.getInsertion(i);
            if (ins.getSource().equals(InsertionSourceType.INPUT)) {
                String name = (ins.getContent() != null) ? ins.getContent()
                        .trim() : "";
                String fallback = (ins.getDefault() != null) ? ins
                        .getDefault().trim() : "";
                this.add(new JLabel(name));
                this.add(new JTextField(fallback));
                counter += 1;
            }
        }
        SpringUtilities.makeCompactGrid(this, 0, 2);
    }

    public boolean showPanel() {
        int answer = JOptionPane.showConfirmDialog(this.master.frame, this,
                "Specify Values for this Tool",
                JOptionPane.OK_CANCEL_OPTION);
        return (answer == JOptionPane.OK_OPTION);
    }

    public HashMap<String,String> evaluate() {
        HashMap<String,String> ret = new HashMap<String,String>();
        for (int i = 0; i < this.getComponentCount() - 1; i++) {
            String name = ((JLabel) this.getComponent(i)).getText();
            String value = ((JTextField) this.getComponent(++i)).getText();
            ret.put(name, value);
        }
        return ret;
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