package com.cosylab.cdb.browser;

import javax.swing.*;
import java.beans.*;

class CDBDialog extends JDialog
{
    /**
     *  Constructor that creates a warning dialog (with "YES" option).
     *  @param owner the non-null Dialog from which the dialog is displayed.
     *  @param msg1 the first message to be displayed.
     *  @param msg2 the second message to be displayed.
     */
    CDBDialog(JFrame owner, String msg1, String msg2){
	super(owner,"Warning Dialog - CDB Browser",true);

	Object [] messages = {msg1,msg2};
	
	final String button1 = "OK ";
	Object [] options = {button1};
	
	final JOptionPane optionPane = new JOptionPane(messages, JOptionPane.WARNING_MESSAGE, JOptionPane.YES_OPTION,
						       null, options, options[0]);
	optionPane.addPropertyChangeListener(new PropertyChangeListener() {
		public void propertyChange(PropertyChangeEvent e) {
		    
		    String prop = e.getPropertyName();
		    
		    Object value = optionPane.getValue();
		    		    
		    if(value.equals(button1)){		
			setVisible(false);
		    } 		    
		}
	    });
	
	setContentPane(optionPane);
	setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
	
	pack();
	setLocationRelativeTo(owner);
	super.show();
    }
}
