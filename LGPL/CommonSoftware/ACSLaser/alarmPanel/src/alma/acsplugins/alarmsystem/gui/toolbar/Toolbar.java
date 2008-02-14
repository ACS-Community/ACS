/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.toolbar;

import java.awt.FlowLayout;
import java.awt.Font;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * The toolbar for the alarm panel
 * 
 * @author acaproni
 *
 */
public class Toolbar extends JPanel {
	
	// The combo box to choose the auto acknowledge level
	private static final String[] autoAckLevels = {
		"None",
		"Priority 3",
		"Priority 2",
		"Priority 1"
	};
	private JComboBox autoAckLevelCB=new JComboBox(autoAckLevels);

	private JLabel autoAckLbl = new JLabel("Auto ack: ");
	
	/**
	 * Constructor
	 */
	public Toolbar() {
		super();
		initialize();
	}
	
	/**
	 * Initialize the toolbar
	 */
	private void initialize() {
		FlowLayout layout = (FlowLayout)getLayout();
		layout.setAlignment(FlowLayout.LEFT);
		
		// Add the label and the combobox for auto ack
		Font fnt = autoAckLbl.getFont();
		Font newFont = fnt.deriveFont(fnt.getSize()*80/100);
		autoAckLbl.setFont(newFont);
		add(autoAckLbl);
		autoAckLevelCB.setFont(newFont);
		add(autoAckLevelCB);
	}
}
