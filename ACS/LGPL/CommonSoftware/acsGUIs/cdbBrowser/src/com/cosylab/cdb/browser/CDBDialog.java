/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
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
