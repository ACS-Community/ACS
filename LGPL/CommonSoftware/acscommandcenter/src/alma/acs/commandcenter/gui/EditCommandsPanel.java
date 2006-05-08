/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Field;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.BevelBorder;

import alma.acs.commandcenter.app.CommandCenterLogic;
import alma.acs.commandcenter.util.PreparedString;



/**
 * Allows to edit the content of some members of the 'Executor' delegates.
 */
class EditCommandsPanel extends JPanel implements ActionListener {

    protected EditCommandsPanel(CommandCenterLogic controller) {

        setLayout(new GridLayout(0, 1));
        process(controller.executeServices);
        process(controller.executeManager);
        process(controller.executeContainer);
        process(controller.executeAcs);
        process(controller.executeTools);
        JButton btnSave = new JButton("Store values !");
        btnSave.addActionListener(this);
        btnSave.setBorder(new BevelBorder(BevelBorder.RAISED));
        this.add(new JPanel());
        this.add(btnSave);
        
        btnSave.setName("btn_Save");
    }

    public void actionPerformed(ActionEvent evt) {
        for (int i = 0; i < getComponentCount(); i++) {
            if (getComponent(i) instanceof FieldStripe) {
                FieldStripe s = null;
                try {
                    s = (FieldStripe) getComponent(i);
                    setFieldValue(s);
                } catch (Exception e) {
                    System.err.println("can't set field " + s.field
                            + " to value " + s.valueF.getText() + ": " + e);
                }
            }
        }
    }

    protected void process(Object obj) {
        Field[] f = obj.getClass().getDeclaredFields();
        for (int i = 0; i < f.length; i++) {
            Field field = f[i];
            if (isInterestingField(field)) {
                try {
                    this.add(new FieldStripe(obj, field));
                } catch (Exception e) {
                    System.err.println("can't read field: " + e);
                }
            }
        }
    }

    protected boolean isInterestingField(Field f) {
        return f.getType().isAssignableFrom(PreparedString.class);
    }

    protected void setFieldValue(FieldStripe s) throws Exception {
        s.field.set(s.object, new PreparedString(s.valueF
                .getText()));
    }

    protected class FieldStripe extends JPanel {

        Object object;

        Field field;

        JTextField valueF;

        FieldStripe(Object o, Field f) throws Exception {
            this.object = o;
            this.field = f;
            this.setLayout(new GridLayout(1, 2));
            String cn = o.getClass().getName();
            this.add(new JLabel(cn.substring(cn.lastIndexOf(".") + 1) + "."
                    + field.getName()));
            this.add(valueF = new JTextField(30));
            valueF.setText(String.valueOf(field.get(object)));
        }
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